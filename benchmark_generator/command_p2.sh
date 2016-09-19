#!/bin/bash
for p in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24"
do
#		mkdir p2/p"$p"
#		mkdir p2/p"$p"/smv
#		mkdir p2/p"$p"/verds
#		mkdir p2/p"$p"/sctl
		mv "$p"*.smv p2/p"$p"*/smv/
		mv "$p"*.vvm p2/p"$p"*/verds/
		mv "$p"*.model p2/p"$p"*/sctl/
done

