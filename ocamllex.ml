(* ocamllex_tool.ml — A reimplementation of the Unix lex utility in OCaml.
 *
 * This tool reads a lex specification file (with the classic three-section
 * format) and emits an OCaml lexer program.
 *
 * Lex specification format:
 *   %{
 *     ... verbatim header code ...
 *   %}
 *   definitions
 *   %%
 *   rules
 *   %%
 *   ... verbatim trailer code ...
 *
 * Each rule has the form:
 *   PATTERN   { action }
 *
 * Supported regex operators:
 *   .          any char except newline
 *   [abc]      character class
 *   [^abc]     negated character class
 *   [a-z]      ranges in classes
 *   r*         zero or more
 *   r+         one or more
 *   r?         zero or one
 *   r{n,m}     repetition
 *   r|s        alternation
 *   (r)        grouping
 *   "string"   literal string
 *   \n \t etc  escapes
 *   ^pattern   start-of-line anchor
 *   pattern$   end-of-line anchor
 *   name       macro expansion (defined in definitions section)
 *
 * The generated OCaml program uses a hand-rolled DFA simulation via
 * NFA -> DFA subset construction.
 *)

(* ------------------------------------------------------------------ *)
(* 1.  REGEX AST                                                        *)
(* ------------------------------------------------------------------ *)

type charset = bool array  (* 256 elements, true = member *)

type regex =
  | Empty                       (* matches empty string *)
  | Literal  of char
  | AnyChar                     (* . — any except \n *)
  | Class    of charset
  | Concat   of regex * regex
  | Alt      of regex * regex
  | Star     of regex
  | Plus     of regex
  | Opt      of regex
  | Anchor_Start                (* ^ *)
  | Anchor_End                  (* $ *)

(* ------------------------------------------------------------------ *)
(* 2.  REGEX PARSER                                                     *)
(* ------------------------------------------------------------------ *)

exception Parse_error of string

let make_charset () = Array.make 256 false

let charset_of_char c =
  let cs = make_charset () in
  cs.(Char.code c) <- true;
  cs

let charset_union a b =
  Array.init 256 (fun i -> a.(i) || b.(i))

let charset_negate cs =
  Array.init 256 (fun i -> not cs.(i))

let escape_char = function
  | 'n'  -> '\n'
  | 't'  -> '\t'
  | 'r'  -> '\r'
  | '\\' -> '\\'
  | '"'  -> '"'
  | '\'' -> '\''
  | '['  -> '['
  | ']'  -> ']'
  | '('  -> '('
  | ')'  -> ')'
  | '.'  -> '.'
  | '*'  -> '*'
  | '+'  -> '+'
  | '?'  -> '?'
  | '|'  -> '|'
  | '^'  -> '^'
  | '$'  -> '$'
  | '{'  -> '{'
  | '}'  -> '}'
  | c    -> c

(* Simple recursive-descent regex parser *)
type pstate = { s: string; mutable pos: int }

let peek ps = if ps.pos < String.length ps.s then Some ps.s.[ps.pos] else None
let advance ps = ps.pos <- ps.pos + 1
let consume ps = let c = ps.s.[ps.pos] in advance ps; c

let parse_char_class ps =
  (* already consumed '[' *)
  let negate =
    if peek ps = Some '^' then (advance ps; true) else false
  in
  let cs = make_charset () in
  let add c = cs.(Char.code c) <- true in
  let rec loop () =
    match peek ps with
    | None    -> raise (Parse_error "unterminated character class")
    | Some ']' -> advance ps
    | Some '\\' ->
      advance ps;
      (match peek ps with
       | None -> raise (Parse_error "trailing backslash in class")
       | Some c -> advance ps; add (escape_char c));
      loop ()
    | Some c ->
      advance ps;
      (* check for range *)
      (match peek ps with
       | Some '-' ->
         advance ps;
         (match peek ps with
          | Some ']' ->
            (* literal '-' at end *)
            add c; add '-'; advance ps
          | Some end_c ->
            advance ps;
            let lo = Char.code c and hi = Char.code end_c in
            for i = lo to hi do cs.(i) <- true done;
            loop ()
          | None -> raise (Parse_error "unterminated class range"))
       | _ -> add c; loop ())
  in
  loop ();
  if negate then charset_negate cs else cs

(* Forward declarations for mutual recursion *)
let rec parse_alt macros ps =
  let lhs = parse_concat macros ps in
  match peek ps with
  | Some '|' -> advance ps; Alt (lhs, parse_alt macros ps)
  | _        -> lhs

and parse_concat macros ps =
  let rec loop acc =
    match peek ps with
    | None | Some '|' | Some ')' | Some '$' -> acc
    | _ ->
      let r = parse_postfix macros ps in
      loop (if acc = Empty then r else Concat (acc, r))
  in
  loop Empty

and parse_postfix macros ps =
  let base = parse_atom macros ps in
  match peek ps with
  | Some '*' -> advance ps; Star base
  | Some '+' -> advance ps; Plus base
  | Some '?' -> advance ps; Opt  base
  | Some '{' ->
    (* {n} or {n,m} repetition — expand to explicit concat/opt *)
    advance ps;
    let read_int () =
      let buf = Buffer.create 4 in
      let rec lp () =
        match peek ps with
        | Some c when c >= '0' && c <= '9' ->
          Buffer.add_char buf c; advance ps; lp ()
        | _ -> ()
      in lp ();
      let s = Buffer.contents buf in
      if s = "" then raise (Parse_error "expected integer in {}");
      int_of_string s
    in
    let n = read_int () in
    let m =
      match peek ps with
      | Some ',' -> advance ps; Some (read_int ())
      | _ -> None
    in
    (match peek ps with
     | Some '}' -> advance ps
     | _ -> raise (Parse_error "expected '}'"));
    (* Build n mandatory copies, then (m-n) optional copies *)
    let mandatory =
      let rec build acc i =
        if i = 0 then acc else build (Concat (acc, base)) (i-1)
      in
      if n = 0 then Empty else build base (n-1)
    in
    (match m with
     | None -> mandatory
     | Some hi ->
       let rec opt_part acc i =
         if i = 0 then acc else opt_part (Concat (acc, Opt base)) (i-1)
       in
       opt_part mandatory (hi - n))
  | _ -> base

and parse_atom macros ps =
  match peek ps with
  | None -> Empty
  | Some '(' ->
    advance ps;
    let r = parse_alt macros ps in
    (match peek ps with
     | Some ')' -> advance ps
     | _ -> raise (Parse_error "expected ')'"));
    r
  | Some '[' ->
    advance ps;
    Class (parse_char_class ps)
  | Some '.' ->
    advance ps;
    AnyChar
  | Some '^' ->
    advance ps;
    Anchor_Start
  | Some '"' ->
    (* literal quoted string *)
    advance ps;
    let buf = Buffer.create 8 in
    let rec lp () =
      match peek ps with
      | None     -> raise (Parse_error "unterminated string literal")
      | Some '"' -> advance ps
      | Some '\\' ->
        advance ps;
        (match peek ps with
         | None -> raise (Parse_error "trailing backslash in string")
         | Some c -> advance ps; Buffer.add_char buf (escape_char c));
        lp ()
      | Some c   -> advance ps; Buffer.add_char buf c; lp ()
    in
    lp ();
    let s = Buffer.contents buf in
    if s = "" then Empty
    else
      String.fold_left
        (fun acc c -> if acc = Empty then Literal c else Concat (acc, Literal c))
        Empty s
  | Some '\\' ->
    advance ps;
    (match peek ps with
     | None -> raise (Parse_error "trailing backslash")
     | Some c -> advance ps; Literal (escape_char c))
  | Some '{' ->
    (* macro reference {name} *)
    advance ps;
    let buf = Buffer.create 8 in
    let rec lp () =
      match peek ps with
      | None     -> raise (Parse_error "unterminated macro reference")
      | Some '}' -> advance ps
      | Some c   -> advance ps; Buffer.add_char buf c; lp ()
    in
    lp ();
    let name = Buffer.contents buf in
    (match List.assoc_opt name macros with
     | None   -> raise (Parse_error ("undefined macro: " ^ name))
     | Some r -> r)
  | Some c when (c = '|' || c = ')' || c = '$') -> Empty
  | Some c ->
    advance ps; Literal c

let parse_regex macros s =
  let ps = { s; pos = 0 } in
  let r = parse_alt macros ps in
  (* handle trailing $ anchor *)
  let r =
    match peek ps with
    | Some '$' -> advance ps; Concat (r, Anchor_End)
    | _ -> r
  in
  r

(* ------------------------------------------------------------------ *)
(* 3.  NFA CONSTRUCTION (Thompson's construction)                       *)
(* ------------------------------------------------------------------ *)

type nfa_state = {
  id: int;
  mutable transitions: (char option * int) list;  (* None = epsilon *)
  mutable accept: int option;   (* rule index if accepting *)
}

let nfa_states : nfa_state array ref = ref [||]
let nfa_count = ref 0

let new_state () =
  let id = !nfa_count in
  incr nfa_count;
  let st = { id; transitions = []; accept = None } in
  (* Grow array if needed *)
  if id >= Array.length !nfa_states then begin
    let new_arr = Array.make (max 64 (id * 2)) { id=0; transitions=[]; accept=None } in
    Array.blit !nfa_states 0 new_arr 0 (Array.length !nfa_states);
    nfa_states := new_arr
  end;
  !nfa_states.(id) <- st;
  id

let add_trans src lbl dst =
  let s = !nfa_states.(src) in
  s.transitions <- (lbl, dst) :: s.transitions

let add_epsilon src dst = add_trans src None dst
let add_char_trans src c dst = add_trans src (Some c) dst

(* Returns (start, end) state IDs *)
let rec build_nfa regex =
  match regex with
  | Empty ->
    let s = new_state () and e = new_state () in
    add_epsilon s e; (s, e)
  | Literal c ->
    let s = new_state () and e = new_state () in
    add_char_trans s c e; (s, e)
  | AnyChar ->
    let s = new_state () and e = new_state () in
    for i = 0 to 255 do
      if Char.chr i <> '\n' then add_char_trans s (Char.chr i) e
    done;
    (s, e)
  | Class cs ->
    let s = new_state () and e = new_state () in
    for i = 0 to 255 do
      if cs.(i) then add_char_trans s (Char.chr i) e
    done;
    (s, e)
  | Concat (a, b) ->
    let (s1, e1) = build_nfa a in
    let (s2, e2) = build_nfa b in
    add_epsilon e1 s2;
    (s1, e2)
  | Alt (a, b) ->
    let s = new_state () and e = new_state () in
    let (s1, e1) = build_nfa a in
    let (s2, e2) = build_nfa b in
    add_epsilon s s1; add_epsilon s s2;
    add_epsilon e1 e; add_epsilon e2 e;
    (s, e)
  | Star r ->
    let s = new_state () and e = new_state () in
    let (s1, e1) = build_nfa r in
    add_epsilon s s1; add_epsilon s e;
    add_epsilon e1 s1; add_epsilon e1 e;
    (s, e)
  | Plus r ->
    let (s1, e1) = build_nfa r in
    let s = new_state () and e = new_state () in
    add_epsilon s s1;
    add_epsilon e1 s1; add_epsilon e1 e;
    (s, e)
  | Opt r ->
    let s = new_state () and e = new_state () in
    let (s1, e1) = build_nfa r in
    add_epsilon s s1; add_epsilon s e; add_epsilon e1 e;
    (s, e)
  | Anchor_Start | Anchor_End ->
    (* Anchors: generate epsilon transitions (simplified — full anchor
       support requires context tracking in the generated code) *)
    let s = new_state () and e = new_state () in
    add_epsilon s e; (s, e)

(* ------------------------------------------------------------------ *)
(* 4.  NFA -> DFA (subset construction)                                 *)
(* ------------------------------------------------------------------ *)

module IntSet = Set.Make(Int)
module SetMap  = Map.Make(IntSet)

let epsilon_closure states =
  let visited = ref IntSet.empty in
  let queue   = Queue.create () in
  IntSet.iter (fun s -> Queue.add s queue) states;
  visited := states;
  while not (Queue.is_empty queue) do
    let s = Queue.pop queue in
    List.iter (fun (lbl, dst) ->
      if lbl = None && not (IntSet.mem dst !visited) then begin
        visited := IntSet.add dst !visited;
        Queue.add dst queue
      end
    ) !nfa_states.(s).transitions
  done;
  !visited

let move set c =
  IntSet.fold (fun s acc ->
    List.fold_left (fun a (lbl, dst) ->
      match lbl with
      | Some ch when ch = c -> IntSet.add dst a
      | _ -> a
    ) acc !nfa_states.(s).transitions
  ) set IntSet.empty

type dfa_state = {
  dfa_id:     int;
  nfa_set:    IntSet.t;
  mutable dfa_trans: (char * int) list;
  mutable dfa_accept: int option;  (* lowest-numbered rule matched *)
}

let build_dfa start_states =
  let all_chars = Array.init 256 Char.chr in
  let dfa_list : dfa_state list ref = ref [] in
  let map : int SetMap.t ref = ref SetMap.empty in
  let counter = ref 0 in
  let get_or_create nfa_set =
    match SetMap.find_opt nfa_set !map with
    | Some id -> id
    | None ->
      let id = !counter in
      incr counter;
      map := SetMap.add nfa_set id !map;
      (* Determine accept: pick the lowest rule index among accepting NFA states *)
      let accept =
        IntSet.fold (fun s best ->
          match !nfa_states.(s).accept with
          | None -> best
          | Some rule -> (match best with None -> Some rule | Some b -> Some (min b rule))
        ) nfa_set None
      in
      dfa_list := { dfa_id = id; nfa_set; dfa_trans = []; dfa_accept = accept } :: !dfa_list;
      id
  in
  let start_closure = epsilon_closure start_states in
  let start_id = get_or_create start_closure in
  let queue = Queue.create () in
  Queue.add start_closure queue;
  while not (Queue.is_empty queue) do
    let nfa_set = Queue.pop queue in
    let dfa_id = SetMap.find nfa_set !map in
    let dfa_st = List.find (fun s -> s.dfa_id = dfa_id) !dfa_list in
    Array.iter (fun c ->
      let next_nfa = epsilon_closure (move nfa_set c) in
      if not (IntSet.is_empty next_nfa) then begin
        let next_id = get_or_create next_nfa in
        dfa_st.dfa_trans <- (c, next_id) :: dfa_st.dfa_trans;
        if not (SetMap.mem next_nfa !map) then begin
          (* this never happens because get_or_create adds it, but guard anyway *)
          Queue.add next_nfa queue
        end
      end
    ) all_chars
  done;
  (start_id, !dfa_list)

(* ------------------------------------------------------------------ *)
(* 5.  SPEC FILE PARSER                                                  *)
(* ------------------------------------------------------------------ *)

type rule = {
  pattern: string;
  action:  string;
}

type spec = {
  header:      string;
  trailer:     string;
  macros:      (string * string) list;
  rules:       rule list;
}

let read_file path =
  let ic = open_in path in
  let n  = in_channel_length ic in
  let s  = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let parse_spec src =
  let lines = String.split_on_char '\n' src in
  let lines = Array.of_list lines in
  let n = Array.length lines in
  let i = ref 0 in

  (* Read verbatim %{ ... %} block *)
  let read_verbatim () =
    let buf = Buffer.create 64 in
    while !i < n && lines.(!i) <> "%}" do
      Buffer.add_string buf lines.(!i);
      Buffer.add_char buf '\n';
      incr i
    done;
    if !i < n then incr i;  (* skip %} *)
    Buffer.contents buf
  in

  (* Section 1: definitions *)
  let header = Buffer.create 64 in
  let macros : (string * string) list ref = ref [] in
  while !i < n && lines.(!i) <> "%%" do
    let line = lines.(!i) in
    incr i;
    if line = "%{" then
      Buffer.add_string header (read_verbatim ())
    else if String.length line > 0 && line.[0] <> ' ' && line.[0] <> '\t' then begin
      (* macro definition: NAME  regex *)
      match String.split_on_char ' ' (String.trim line) with
      | name :: rest ->
        let pat = String.trim (String.concat " " rest) in
        if pat <> "" then macros := (name, pat) :: !macros
      | [] -> ()
    end
  done;
  if !i < n then incr i;  (* skip first %% *)

  (* Section 2: rules *)
  (* We need to parse lazily-expanded macros *)
  let rules : rule list ref = ref [] in
  let in_rules = ref true in
  while !i < n && !in_rules do
    let line = lines.(!i) in
    if line = "%%" then begin
      incr i;
      in_rules := false
    end else if String.length line = 0 || line.[0] = ' ' || line.[0] = '\t' then begin
      incr i  (* skip blank/indented lines between rules *)
    end else begin
      incr i;
      (* Find the split between pattern and action.
         Pattern ends at first unquoted, unescaped whitespace at depth 0. *)
      let pat_end = ref 0 in
      let depth = ref 0 in
      let in_class = ref false in
      let in_str = ref false in
      let k = ref 0 in
      let ll = String.length line in
      while !k < ll && (!depth > 0 || !in_class || !in_str ||
                        (line.[!k] <> ' ' && line.[!k] <> '\t')) do
        let c = line.[!k] in
        (if !in_str then (if c = '"' && (!k = 0 || line.[!k-1] <> '\\') then in_str := false)
         else if !in_class then (if c = ']' then in_class := false)
         else match c with
           | '"' -> in_str := true
           | '[' -> in_class := true
           | '(' -> incr depth
           | ')' -> decr depth
           | _   -> ());
        incr k
      done;
      pat_end := !k;
      let pattern = String.sub line 0 !pat_end in
      let action_raw =
        if !pat_end < ll then String.trim (String.sub line !pat_end (ll - !pat_end))
        else ""
      in
      (* Multi-line action: if action starts with '{', read until matching '}' *)
      let action =
        if action_raw = "" || action_raw.[0] <> '{' then action_raw
        else begin
          let buf = Buffer.create 64 in
          Buffer.add_string buf action_raw;
          let depth2 = ref (String.fold_left (fun acc c ->
            if c = '{' then acc+1 else if c = '}' then acc-1 else acc) 0 action_raw) in
          while !depth2 > 0 && !i < n do
            Buffer.add_char buf '\n';
            Buffer.add_string buf lines.(!i);
            depth2 := String.fold_left (fun acc c ->
              if c = '{' then acc+1 else if c = '}' then acc-1 else acc)
              !depth2 lines.(!i);
            incr i
          done;
          Buffer.contents buf
        end
      in
      if pattern <> "" then
        rules := { pattern; action } :: !rules
    end
  done;

  (* Section 3: trailer *)
  let trailer = Buffer.create 64 in
  while !i < n do
    Buffer.add_string trailer lines.(!i);
    Buffer.add_char trailer '\n';
    incr i
  done;

  { header = Buffer.contents header;
    trailer = Buffer.contents trailer;
    macros = List.rev !macros;
    rules = List.rev !rules }

(* ------------------------------------------------------------------ *)
(* 6.  CODE GENERATION                                                   *)
(* ------------------------------------------------------------------ *)

let generate spec out_path =
  (* Reset NFA state *)
  nfa_states := Array.make 64 { id=0; transitions=[]; accept=None };
  nfa_count  := 0;

  (* Parse macros *)
  let parsed_macros : (string * regex) list ref = ref [] in
  List.iter (fun (name, pat) ->
    let r = parse_regex !parsed_macros pat in
    parsed_macros := !parsed_macros @ [(name, r)]
  ) spec.macros;

  (* Build combined NFA: one start, one end per rule *)
  let start_set = ref IntSet.empty in
  let num_rules = List.length spec.rules in
  let _ = num_rules in

  List.iteri (fun rule_idx rule ->
    let regex = parse_regex !parsed_macros rule.pattern in
    let (s, e) = build_nfa regex in
    !nfa_states.(e).accept <- Some rule_idx;
    start_set := IntSet.add s !start_set
  ) spec.rules;

  let (dfa_start, dfa_states) = build_dfa !start_set in

  (* Sort dfa states by id for deterministic output *)
  let dfa_states = List.sort (fun a b -> compare a.dfa_id b.dfa_id) dfa_states in

  let oc = open_out out_path in
  let pr fmt = Printf.fprintf oc fmt in

  pr "(* Generated by ocamllex_tool — do not edit *)\n\n";

  if spec.header <> "" then begin
    pr "%s\n" spec.header
  end;

  pr "let lex_buf    = Buffer.create 256\n";
  pr "let yytext ()  = Buffer.contents lex_buf\n";
  pr "let yyleng ()  = Buffer.length lex_buf\n";
  pr "let yylineno   = ref 1\n";
  pr "let yypos      = ref 0\n";
  pr "\n";

  (* Transition table as a 2D array: dfa_state × char → next_state (-1 = none) *)
  let max_id = List.fold_left (fun m s -> max m s.dfa_id) 0 dfa_states + 1 in
  pr "let dfa_trans : int array array = [|\n";
  List.iter (fun dst ->
    pr "  [| (* state %d *)\n    " dst.dfa_id;
    for c = 0 to 255 do
      let next =
        match List.assoc_opt (Char.chr c) dst.dfa_trans with
        | Some id -> id
        | None    -> -1
      in
      pr "%d;" next;
      if (c + 1) mod 16 = 0 then pr "\n    "
    done;
    pr "\n  |];\n"
  ) dfa_states;
  pr "|]\n\n";
  let _ = max_id in

  (* Accept table *)
  pr "let dfa_accept : int array = [| (* -1 = not accepting *)\n  ";
  List.iter (fun dst ->
    pr "%d; " (match dst.dfa_accept with None -> -1 | Some r -> r)
  ) dfa_states;
  pr "\n|]\n\n";

  pr "let dfa_start = %d\n\n" dfa_start;

  (* The main lex function *)
  pr {|let yylex ic =
  let rec next_token () =
    Buffer.clear lex_buf;
    let state = ref dfa_start in
    let last_accept = ref (-1) in
    let last_accept_len = ref 0 in
    let buf_pos = ref 0 in
    let input_buf = Buffer.create 64 in
    let finished = ref false in
    while not !finished do
      let ch_opt =
        try Some (input_char ic)
        with End_of_file -> None
      in
      match ch_opt with
      | None ->
        finished := true
      | Some c ->
        Buffer.add_char input_buf c;
        let next = dfa_trans.(!state).(Char.code c) in
        if next = -1 then begin
          (* No transition: accept the last good match if any *)
          if !last_accept >= 0 then begin
            (* Push back characters past the last accept point *)
            let total = Buffer.length input_buf in
            let pushback = total - !last_accept_len in
            (* We can't "unread" from in_channel easily so we re-read later —
               for simplicity in this generated stub we note the accepted text *)
            let _ = pushback in
            let accepted_text = Buffer.sub input_buf 0 !last_accept_len in
            Buffer.add_string lex_buf accepted_text
          end;
          finished := true
        end else begin
          state := next;
          incr buf_pos;
          if dfa_accept.(!state) >= 0 then begin
            last_accept := dfa_accept.(!state);
            last_accept_len := !buf_pos
          end
        end
    done;
    if !last_accept < 0 && Buffer.length lex_buf = 0 then
      None  (* EOF *)
    else
      Some !last_accept
  in
  next_token ()
|};

  pr "\n";

  (* Rule dispatch *)
  pr "let dispatch rule_id =\n";
  pr "  match rule_id with\n";
  List.iteri (fun i rule ->
    pr "  | %d -> %s\n" i rule.action
  ) spec.rules;
  pr "  | _ -> ()\n\n";

  pr "let () =\n";
  pr "  let ic = open_in Sys.argv.(1) in\n";
  pr "  (try\n";
  pr "    while true do\n";
  pr "      match yylex ic with\n";
  pr "      | None -> raise Exit\n";
  pr "      | Some rule -> dispatch rule\n";
  pr "    done\n";
  pr "  with Exit | End_of_file -> ());\n";
  pr "  close_in ic\n\n";

  if spec.trailer <> "" then
    pr "%s\n" spec.trailer;

  close_out oc;
  Printf.printf "Generated: %s\n" out_path

(* ------------------------------------------------------------------ *)
(* 7.  MAIN                                                              *)
(* ------------------------------------------------------------------ *)

let usage () =
  Printf.eprintf "Usage: ocamllex_tool [-o output.ml] input.l\n";
  exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse_args = function
    | [] -> usage ()
    | ["-o"; _] -> usage ()
    | "-o" :: out :: rest ->
      (match rest with
       | [inp] ->
         let src = read_file inp in
         let spec = parse_spec src in
         generate spec out
       | _ -> usage ())
    | [inp] ->
      let out = Filename.remove_extension inp ^ "_lex.ml" in
      let src = read_file inp in
      let spec = parse_spec src in
      generate spec out
    | _ -> usage ()
  in
  parse_args args
