#!/bin/bash

set -e    # exit script on first error



cd $CA/out/v06888_BW_BH_NPH3_x1000_po0p90_lpc+

cp HiggsLumi_v06888_gg_channel.top $CA/scripts/HiggsLumi_v06888_gg_channel.txt
cp HiggsLumi_v06888_eg_ge_channel_sum.top $CA/scripts/HiggsLumi_v06888_eg_ge_channel_sum.txt
cp HiggsLumi_v06888_ep_pe_channel_sum.top $CA/scripts/HiggsLumi_v06888_ep_pe_channel_sum.txt
