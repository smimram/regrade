# REGrade: a simple grading system based on regular expressions

This is a simple program to automatically grade homework. Each question is validated by looking a whether the file matches a regular expression. It can be useful as a first approximation of the correction, or in cases where tests are sufficient to ensure the validity of the answers.

The correction is specified by a csv file of the form

```csv
name,Test
question,Q1,Q2,Q3
regexp,module,bla,\(if\|and\)
points,,2,3
```

where the signification of the lines is the following

- `name`: name of the exam,
- `question`: name of each question,
- `regexp`: regexp for correcting each question,
- `points`: number of points for each question (1 if not specified).

You can then run a command of the form

```shell
regrade exam.csv file1 file file3
```

and it will grade the various files, outputting the results in `grades.csv`. If a folder is given instead of a file, all the file it contains are considered as one homework.
