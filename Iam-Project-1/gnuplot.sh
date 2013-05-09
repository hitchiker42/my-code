
#!/bin/bash
MAX=100
echo "clear" > plot.gpi
echo "reset" >> plot.gpi
#echo "set terminal gif animate delay 10" >> plot.gpi
echo "set terminal png" >> plot.gpi
#echo "set output \"animate.gif\"" >> plot.gpi
echo "set output \"data.png\"" >> plot.gpi

for i in `ls kdv-data`;do
if [[ $i == "000000.acs" ]]; then
echo "plot \"kdv-data/${i}\" using 1:2 title \"$i\", \\">>plot.gpi
elif [[ $i = "052841.acs" ]];then
echo "\"kdv-data/${i}\" using 1:2 title \"$i\"">>plot.gpi
else 
echo "\"kdv-data/${i}\" using 1:2 title \"$i\", \\">>plot.gpi
fi
#echo "plot \"kdv-data/${i}\" ">>plot.gpi
done
