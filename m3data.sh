#!/bin/bash

allowdiff=FALSE
#allowdiff=TRUE

folder="/projects/mlg/sbentaie/strategies/RESDATA"
prefix="m3"

lowb=1
upb=1430
by=10

echo " M3 - $allowdiff "





allstrat=("MEAN" "REC-LIN" "DIR-LIN"
"REC-KNN" "RTI-KNN" "RJT-KNN"  "RJT4-KNN"
"DIR-KNN" "JNT-KNN" "JNT4-KNN" "RFY-KNN"
"REC-MLP" "DIR-MLP" "JNT-MLP" "JNT4-MLP" "RFY-MLP"
"REC-BST1" "DIR-BST1" "RFY-BST1"
"REC-BST2" "DIR-BST2" "RFY-BST2")

	for strat in "${allstrat[@]}"
	do
			echo "$strat"
			idjob=1
			missing=0 
			for runstart in $(seq $lowb $by $upb) 
			do
				runend=$(($runstart+$by-1))
				file="$folder/$prefix-$runstart-$runend-$strat-$allowdiff.Rdata"
				
				if [ ! -f "$file" ]
				then
				    #echo "$file not found - $idjob"
					missing=$(($missing+1))
				fi
				idjob=$(($idjob+1))
			done

			if test $missing -ne 0
                        then
				echo "MISSING : $missing"
				#echo " $(($upb/$by - $missing))/$(($upb/$by))"	
			fi
			echo "----------------"
#		echo "T= $ntrain - $strat -  $(($upb/$by - $missing))/$(($upb/$by))"
	done


