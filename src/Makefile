all: test

test:
	@dune exec ./regrade.exe -- test.csv --extension ml --extension test --filename-to-utf8 --formulas --sed "s/\\.ml//" && cat grades.csv
