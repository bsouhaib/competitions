#!/bin/bash

#R CMD BATCH --no-restore analysis.R


compet=NN5
H=56

#compet=M3
#H=18

algopath="/u/sbentaie/friedmantest"
algopathbis="/u/sbentaie/friedmantestold"
csvpath="/u/sbentaie/strategies/friedman/csv"
pdfpath="/u/sbentaie/strategies/friedman/pdf"


javac "$algopath/CDF_Normal.java"
javac "$algopath/Fichero.java"
javac "$algopath/Pareja.java"
javac "$algopath/Relation.java"

algo="modifiedFriedman"
algo2="MonFriedman"
seminalalgo="Friedman"

javac -classpath $algopath "$algopath/$algo.java"
javac -classpath $algopath "$algopath/$algo2.java"
javac -classpath $algopathbis "$algopathbis/$seminalalgo.java"

for id in  $(seq 1 $H)
do
	java -classpath $algopath $algo2 "$csvpath/table-$compet-$id.csv"  > "$pdfpath/table-$compet-$id.tex"
done
	java -classpath $algopath $algo2 "$csvpath/table-$compet-total.csv"  > "$pdfpath/table-$compet-total.tex"


	java -classpath $algopathbis  $seminalalgo "$csvpath/table-$compet-total.csv"  > "/u/sbentaie/strategies/latex/testingSeminal.tex"
