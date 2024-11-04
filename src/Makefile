all: test

test:
	@dune exec ./regrade.exe -- test.csv --extension ml --formulas && cat grades.csv
