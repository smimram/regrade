(lang dune 2.9)
(version 0.1.0)
(source (github smimram/grader))
(authors "Samuel Mimram <smimram@gmail.com>")
(license GPL-3.0)
(generate_opam_files)

(package
 (name grader)
 (synopsis "Automatic homework grader")
 (description "Simple grading system.")
 (depends
   csv
 )
)
