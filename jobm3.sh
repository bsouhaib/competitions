#!/bin/bash

localfolder="$HOME/competitions"
wdfolder="$HOME/WDFOLDER/RESULTS"
outfolder="$HOME/WDFOLDER/OUT"
jobfolder="$localfolder/jobs"
rscript="main-m3.R"

prefix=m3thesis
tag=""

allowdiff=FALSE

#allowdiff=TRUE

file="$jobfolder/m3file.job"
cat /dev/null > $file
nbsubjobs=0

for idjob in $(seq 1 143) 
do			
		name=$prefix-$allowdiff-$idjob
			
		echo "#!/bin/bash" > $file
		echo "#SBATCH --time=100:0" >> $file
		echo "#SBATCH -o $outfolder/$name.out" >> $file
		echo "#SBATCH -e $outfolder/$name.err" >> $file
		echo "#SBATCH --job-name=$name" >> $file
		echo "module load R/3.0.2/gcc/4.8.2" >> $file		
		echo "cd $localfolder" >> $file 
		
			
		
		a="/usr/local/opt/R/3.0.2/gcc/4.8.2/bin/R  CMD BATCH --no-restore "
		b=" '--args id.job<-$idjob  allow.differencing<-$allowdiff  "
		c="folder<-\""$wdfolder/$prefix-"\"' "
		d="$rscript "$outfolder/$name$tag.Rout" "
			
		echo "$a $b $c $d" >> $file	
		
		#read touche

		sbatch $file
		nbsubjobs=$(($nbsubjobs+1))	
		echo $nbsubjobs	
done
