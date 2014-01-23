#!/bin/bash


folder="$HOME/WDFOLDER/RESULTS"
prefix="NN5"

lowb=1
upb=111
by=1






allstrat=("MEAN" "REC-LIN" "DIR-LIN"
"REC-KNN" "RTI-KNN" "RJT-KNN"  "RJT4-KNN"
"DIR-KNN" "JNT-KNN" "JNT4-KNN" "RFY-KNN"
"REC-MLP" "DIR-MLP" "JNT-MLP" "JNT4-MLP" "RFY-MLP"
"REC-BST1" "DIR-BST1" "RFY-BST1"
"REC-BST2" "DIR-BST2" "RFY-BST2")

allstrat=("MEAN" "REC-MLP" "DIR-MLP" "REC-LIN" "RFY-BST2")
allstrat=("MEAN" "REC-KNN" "DIR-KNN" "REC-LIN" "RFY-KNN")

	for strat in "${allstrat[@]}"
	do
			echo "$strat"
			idjob=1
			missing=0 
			for runstart in $(seq $lowb $by $upb) 
			do
				runend=$(($runstart+$by-1))
				file="$folder/$prefix-$runstart-$runend-$strat.Rdata"
				
				if [ ! -f "$file" ]
				then
				    echo "$file not found - $idjob"
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


