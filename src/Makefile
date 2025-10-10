all: test

test:
	@dune exec ./regrade.exe -- test.csv --extension ml --formulas --sed "s/\\.ml//" && cat grades.csv
