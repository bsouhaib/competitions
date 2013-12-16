#!/bin/bash

localfolder="$HOME/simulations"
workingfolder="/projects/mlg/sbentaie/strategies/RESDATA/"
outfolder="$WORKDIR/OUT"
jobfolder="jobs"
rscript="main-nn5.R"
prefix=NN5

bigfile="$jobfolder/nn5file.job"
cat /dev/null > $bigfile


nbsubjobs=0

for idjob in $(seq 1 111) 
do			
			name=$prefix-$idjob
			
			a="/usr/local/opt/R/2.15.0/bin/R  CMD BATCH --no-restore "
			b=" '--args idjob<-$idjob"
			c="folder<-\""$workingfolder/$prefix-"\"' "
			d="$rscript "$outfolder/$name.Rout" "
				
			echo "$a $b $c $d" >> $bigfile	
			
			nbsubjobs=$(($nbsubjobs+1))	
			echo $nbsubjobs	
done

# PAUSE
read touche

name=$prefix
file="jobfolder/nn5newfile.job"
echo "#!/bin/bash -l" > $file

echo "#PBS -t 1-$nbsubjobs" >> $file
echo "#PBS -l file=10gb" >> $file
echo "#PBS -l mem=20gb" >> $file
echo "#PBS -l nodes=1:ppn=1" >> $file
echo "#PBS -l walltime=100:00:00" >> $file
echo "cd $localfolder" >> $file 


# Execute the line matching the array index from file one_command_per_index.list:
echo "cmd=\`head -\${PBS_ARRAYID} $bigfile | tail -1\`" >> $file


# Execute the command extracted from the file:
echo "eval \$cmd" >> $file

qsub   -M "bensouhaib@gmail.com" -N "$name" -o "$outfolder/$name.out" -e "$outfolder/$name.err"  $file


