(lang dune 2.9)
(name regrade)
(version 0.1.0)
(source (github smimram/regrade))
(authors "Samuel Mimram <smimram@gmail.com>")
(license GPL-3.0)
(generate_opam_files)

(package
 (name regrade)
 (synopsis "Automatic homework grader")
 (description "Simple grading system based on regular expressions.")
 (depends
   csv
   re
 )
)
