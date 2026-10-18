#!/bin/bash

set -e    # exit script on first error


# Usage example:  ./ini_plot_channels_log.sh  v07112    1.0   1.0   0.01  0.01      0     390    0.   2.000    WHITE  BLUE  CYAN   1   33
#                                                                                          ^     ^      ^        ^       ^       ^     ^      ^            ^         ^       ^      ^    ^
#                                                                                           |       |       |          |         |         |      |        |              |           |         |       |      |lumi exponent
#                                                                                           |       |       |          |         |         |      |        |              |           |         |       |ee scale factor
#                                                                                           |       |       |          |         |         |      |        |              |           |         |ee color
#                                                                                           |       |       |          |         |         |      |        |              |           |ep+pe color
#                                                                                           |       |       |          |         |         |      |        |              |eg+ge color
#                                                                                           |       |       |          |         |         |      |        |ymax
#                                                                                           |       |       |          |         |         |      |ymin      
#                                                                                           |       |       |          |         |         |xmax
#                                                                                           |       |       |          |         | xmin
#						                                              |       |       |          | pe scale factor
#						                                              |       |       | ep scale factor  
#                                                                                           |       | eg scale factor
#					                                                      | ge lumi scale factor

#echo $1

DIRROOT=$1
GE=$2
EG=$3
EP=$4
PE=$5
XMIN=$6
XMAX=$7
YMIN=$8
YMAX=$9
CEGGE=${10}
CEPPE=${11}
CEE=${12}
EE=${13}
LUMEXP=${14}


echo " CEGGE= " $CEGGE
echo " CEPPE= " $CEPPE
echo " CEE= " $CEE
echo " LUMEXP=" $LUMEXP "AFTER"


cd $CA/out/"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+

grep -B9 'Spectrum (e2\-3,e2\-3)' HiggsLumi_"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+.top | head -n 10  > HiggsLumi_"$DIRROOT"_ee_channel.top
sed  '1,/Spectrum (e2\-3,e2\-3)/d'  HiggsLumi_"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+.top > HiggsLumi_"$DIRROOT"_ee_channel_temp.top
sed  -i '/NEWFRAME/,$d'  HiggsLumi_"$DIRROOT"_ee_channel_temp.top
sed  '/^ *[0-9]/,$d' HiggsLumi_"$DIRROOT"_ee_channel_temp.top >> HiggsLumi_"$DIRROOT"_ee_channel.top
cp HiggsLumi_"$DIRROOT"_ee_channel_temp.top HiggsLumi_"$DIRROOT"_ee_channel_numbers.top
sed -i  '/^ [^0-9]/d' HiggsLumi_"$DIRROOT"_ee_channel_numbers.top
sed -i 's/\([0-9]\);/\1;\n/g' HiggsLumi_"$DIRROOT"_ee_channel_numbers.top
sed -i 's/\([0-9]\)D/\1E/g' HiggsLumi_"$DIRROOT"_ee_channel_numbers.top
sed -i  '/^$/d' HiggsLumi_"$DIRROOT"_ee_channel_numbers.top
cat HiggsLumi_"$DIRROOT"_ee_channel_numbers.top  | awk -v ee=$EE '{print $1 " " ee*$2}' >> HiggsLumi_"$DIRROOT"_ee_channel.top
sed -i "s/(1023../(1023$LUMEXP/" HiggsLumi_"$DIRROOT"_ee_channel.top
echo "SET COLOR " $CEE >>  HiggsLumi_"$DIRROOT"_ee_channel.top
sed '1,/HISTOGRAM/{/HISTOGRAM/!d}'  HiggsLumi_"$DIRROOT"_ee_channel_temp.top >> HiggsLumi_"$DIRROOT"_ee_channel.top
sed -i 's/^\([0-9]\)/ \1/' HiggsLumi_"$DIRROOT"_ee_channel.top


cp HiggsLumi_"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+.top  HiggsLumi_"$DIRROOT"_ge_channel.top
cp HiggsLumi_"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+.top  HiggsLumi_"$DIRROOT"_eg_channel.top
cp HiggsLumi_"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+.top  HiggsLumi_"$DIRROOT"_ep_channel.top
cp HiggsLumi_"$DIRROOT"_BW_BH_NPH3_x1000_po0p90_lpc+.top  HiggsLumi_"$DIRROOT"_pe_channel.top



sed -i '1,/Spectrum (G,e2\-3)/d'  HiggsLumi_"$DIRROOT"_ge_channel.top 
sed -i '/NEWFRAME/,$d'  HiggsLumi_"$DIRROOT"_ge_channel.top 

sed -i '1,/Spectrum (e2\-3,G)/d'  HiggsLumi_"$DIRROOT"_eg_channel.top 
sed -i '/NEWFRAME/,$d'  HiggsLumi_"$DIRROOT"_eg_channel.top 

sed -i '1,/Spectrum (e2\-3,e2+3)/d'  HiggsLumi_"$DIRROOT"_ep_channel.top 
sed -i '/NEWFRAME/,$d'  HiggsLumi_"$DIRROOT"_ep_channel.top 

sed -i '1,/Spectrum (e2+3,e2\-3)/d'  HiggsLumi_"$DIRROOT"_pe_channel.top 
sed -i '/NEWFRAME/,$d'  HiggsLumi_"$DIRROOT"_pe_channel.top 


sed -i 's/\([0-9]\);/\1;\n/g' HiggsLumi_"$DIRROOT"_*_channel.top
sed -i 's/\([0-9]\)D/\1E/g' HiggsLumi_"$DIRROOT"_*_channel.top
sed -i  '/^$/d' HiggsLumi_"$DIRROOT"_*_channel.top

paste HiggsLumi_"$DIRROOT"_eg_channel.top HiggsLumi_"$DIRROOT"_ge_channel.top > HiggsLumi_"$DIRROOT"_eg_ge_channel_combined.top
sed -i  '/^ [^0-9]/d' HiggsLumi_"$DIRROOT"_eg_ge_channel_combined.top
paste HiggsLumi_"$DIRROOT"_ep_channel.top HiggsLumi_"$DIRROOT"_pe_channel.top > HiggsLumi_"$DIRROOT"_ep_pe_channel_combined.top
sed -i  '/^ [^0-9]/d' HiggsLumi_"$DIRROOT"_ep_pe_channel_combined.top
cat HiggsLumi_"$DIRROOT"_eg_ge_channel_combined.top  | awk -v eg=$EG -v ge=$GE '{print $1 " " eg*$2+ge*$4}' >  HiggsLumi_"$DIRROOT"_eg_ge_channel_sum.top
cat HiggsLumi_"$DIRROOT"_ep_pe_channel_combined.top  | awk -v ep=$EP -v pe=$PE '{print $1 " " ep*$2+pe*$4}' >  HiggsLumi_"$DIRROOT"_ep_pe_channel_sum.top


 cp HiggsLumi_"$DIRROOT"_ee_channel.top HiggsLumi_"$DIRROOT"_all_channels.top

echo "SET COLOR " $CEGGE >> HiggsLumi_"$DIRROOT"_all_channels.top
cat HiggsLumi_"$DIRROOT"_eg_ge_channel_sum.top >>  HiggsLumi_"$DIRROOT"_all_channels.top
echo "HISTOGRAM" >> HiggsLumi_"$DIRROOT"_all_channels.top
echo "SET COLOR " $CEPPE >> HiggsLumi_"$DIRROOT"_all_channels.top
cat  HiggsLumi_"$DIRROOT"_ep_pe_channel_sum.top >>  HiggsLumi_"$DIRROOT"_all_channels.top
echo "HISTOGRAM" >> HiggsLumi_"$DIRROOT"_all_channels.top

sed -i "s/SET LIMIT X FROM.*/SET LIMIT X FROM $XMIN TO $XMAX/" HiggsLumi_"$DIRROOT"_all_channels.top
sed -i "s/SET LIMIT Y FROM.*/SET LIMIT Y FROM $YMIN TO $YMAX/" HiggsLumi_"$DIRROOT"_all_channels.top
sed -i 's/SET SCALE Y.*/SET SCALE Y LOG/' HiggsLumi_"$DIRROOT"_all_channels.top

 td  HiggsLumi_"$DIRROOT"_all_channels.top  POSTSCRIPT
 $CA/scripts/ps2pdfRotate.sh HiggsLumi_"$DIRROOT"_all_channels


cd $CA
