(lang dune 3.6)

(name json_to_html)

(generate_opam_files true)

(source
 (github l1yefeng/json_to_html))

(authors "l1yefeng")

(package
 (name json_to_html)
 (synopsis "Command-line JSON-to-HTML converter")
 (depends ocaml dune yojson tyxml))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
