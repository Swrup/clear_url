(lang dune 2.9)

(implicit_transitive_deps false)

(name clear_url)

(license ISC)

(authors "swrup <swrup@protonmail.com>")

(source
 (uri git+https://git.zapashcanon.fr/swrup/clear-url.git))

(homepage https://git.zapashcanon.fr/swrup/clear-url)

(bug_reports https://git.zapashcanon.fr/swrup/ocaml-clear-url/issues)

(generate_opam_files true)

(package
 (name clear_url)
 (synopsis "OCaml library to remove tracking in URLs")
 (description
  "Use `Clear_url.clean` to remove tracking elements from URLs.")
 (tags
  (clear_url clean url tracker web3))
 (depends
  (ocaml
   (>= 4.08))))
