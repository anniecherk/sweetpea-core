stack build
echo "DONE WITH BUILD"
stack exec generate-popcount > ex.cnf
echo "DONE WITH SAT GEN, file was this many lines long:"
wc -l ex.cnf
head -1 ex.cnf
