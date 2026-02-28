# ocamllex_tool — A `lex` reimplementation in OCaml

## Overview

`ocamllex_tool` reads a classic Unix `lex` specification (three-section `.l`
file) and emits a standalone OCaml lexer program.

---

## Architecture

```
.l file
  │
  ├─ parse_spec        — splits header / macros / rules / trailer
  │
  ├─ parse_regex       — recursive-descent regex → AST
  │       (supports: . [] [^] * + ? {n,m} | () " " \ ^ $  macro refs)
  │
  ├─ build_nfa         — Thompson's construction  (NFA AST)
  │
  ├─ build_dfa         — subset construction       (NFA → DFA)
  │
  └─ generate          — emit OCaml source with:
           • dfa_trans  (state × char → next state)
           • dfa_accept (state → rule index | -1)
           • yylex      (scanner loop)
           • dispatch   (rule-index → action code)
```

---

## Supported Regex Syntax

| Pattern   | Meaning                             |
|-----------|-------------------------------------|
| `.`       | Any character except `\n`           |
| `[abc]`   | Character class                     |
| `[^abc]`  | Negated class                       |
| `[a-z]`   | Range inside class                  |
| `r*`      | Zero or more                        |
| `r+`      | One or more                         |
| `r?`      | Optional                            |
| `r{n,m}`  | Exactly n to m repetitions          |
| `r\|s`    | Alternation                         |
| `(r)`     | Grouping                            |
| `"str"`   | Literal string                      |
| `\n \t`   | Escape sequences                    |
| `^`       | Start-of-line anchor (simplified)   |
| `$`       | End-of-line anchor (simplified)     |
| `{NAME}`  | Macro expansion                     |

---

## Spec File Format

```
%{
  (* verbatim OCaml header — pasted at top of output *)
%}

DIGIT   [0-9]
ALPHA   [a-zA-Z]
ID      {ALPHA}({ALPHA}|{DIGIT})*

%%

{DIGIT}+   { Printf.printf "INT %s\n" (yytext ()) }
{ID}       { Printf.printf "ID  %s\n" (yytext ()) }
[ \t\n]+   { (* whitespace — skip *) }
.          { Printf.printf "UNK %s\n" (yytext ()) }

%%

(* verbatim OCaml trailer — pasted at bottom of output *)
```

---

## Usage

```bash
# Compile the tool
ocamlfind ocamlopt -package str -linkpkg ocamllex.ml -o ocamllex_tool

# Or without findlib:
ocamlopt ocamllex.ml -o ocamllex_tool

# Run on a spec file
./ocamllex_tool tokens.l              # → tokens_lex.ml
./ocamllex_tool -o lexer.ml tokens.l  # → lexer.ml

# Compile and run the generated lexer
ocamlopt lexer.ml -o lexer
./lexer input.txt
```

---

## Sample `.l` file (`example.l`)

```
%{
let () = ()
%}

DIGIT  [0-9]
UPPER  [A-Z]
LOWER  [a-z]
ALPHA  ({UPPER}|{LOWER})
ALNUM  ({ALPHA}|{DIGIT})
ID     {ALPHA}{ALNUM}*

%%

{DIGIT}+"."{DIGIT}*   { Printf.printf "FLOAT  %s\n" (yytext ()) }
{DIGIT}+              { Printf.printf "INT    %s\n" (yytext ()) }
"if"                  { Printf.printf "IF\n" }
"else"                { Printf.printf "ELSE\n" }
"while"               { Printf.printf "WHILE\n" }
{ID}                  { Printf.printf "ID     %s\n" (yytext ()) }
[ \t]+                { (* skip whitespace *) }
\n                    { incr yylineno }
.                     { Printf.printf "UNK    %s\n" (yytext ()) }

%%
(* end of spec *)
```

---

## Key Design Decisions

- **NFA → DFA via subset construction** — exact same algorithm used by real
  `lex`/`flex`. Produces a minimal, fast transition table.
- **Priority by rule order** — when multiple rules accept the same string the
  one with the lower index (earlier in the file) wins, matching `lex` semantics.
- **Longest match** — the scanner keeps track of the last accepting state as it
  consumes characters, identical to the "maximal munch" rule in `lex`.
- **Macro expansion** — macros in the definitions section are expanded
  recursively before regex parsing, exactly as in `lex`.
- **Self-contained output** — the generated `.ml` file has no dependencies
  beyond the OCaml standard library.
