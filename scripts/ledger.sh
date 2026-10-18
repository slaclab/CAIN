#!/bin/bash
# ledger.sh v2.1 — energy-ledger analysis of PRINT STAT blocks
# usage: ./ledger.sh stat_v08770.txt stat_v08771.txt stat_v08772.txt

for f in "$@"; do
echo "===================================================================="
echo " FILE: $f"
echo "===================================================================="
awk '
function d2e(x) { sub(/D/,"E",x); return x+0 }
function firstnum(s) {
    match(s, /-?[0-9]+\.[0-9]+D[+-][0-9]+/)
    return d2e(substr(s, RSTART, RLENGTH))
}
/^##########/ {
    it = $2; sub(/it=/,"",it); it += 0
    if (!(it in seen)) { seen[it]=1; order[++nit]=it }
    cur = it
}
/macro particles/ {
    dir  = ($0 ~ /Right/) ? "R" : "L"
    kind = ($0 ~ /photon/) ? "gam" : ($0 ~ /positron/) ? "pos" : "ele"
    match($0, /[0-9]+ +macro particles/)
    mac = substr($0, RSTART, RLENGTH) + 0
    match($0, /-?[0-9]+\.[0-9]+D[+-][0-9]+ +real/)
    r = substr($0, RSTART, RLENGTH); sub(/ +real/,"",r)
    key = cur SUBSEP dir SUBSEP kind
    NM[key]=mac; NRL[key]=d2e(r)
    pend = key
    next
}
pend != "" && /Average \(En/ {
    s = $0; sub(/^ *Average \(En,Px,Py,Ps\) */,"",s)
    EN[pend] = firstnum(s)
    SE[pend] = NRL[pend]*EN[pend]
    pend = ""
}
END {
    for (i=2;i<=nit;i++){v=order[i];j=i-1
        while(j>0 && order[j]>v){order[j+1]=order[j];j--} order[j+1]=v}

    nd=split("R L",dirs," "); nk=split("gam ele pos",kinds," ")

    # ---------- 1. per-step detail ----------
    printf "\n%-7s %-3s %-4s %12s %13s %13s %13s\n",
           "it","dir","kind","macros","N_real","<En> eV","sumE eV"
    for (i=1;i<=nit;i++){ it=order[i]
      for (d=1;d<=nd;d++) for (k=1;k<=nk;k++){
        key=it SUBSEP dirs[d] SUBSEP kinds[k]
        if (key in NRL)
          printf "%-7d %-3s %-4s %12d %13.4e %13.4e %13.4e\n",
                 it,dirs[d],kinds[k],NM[key],NRL[key],EN[key],SE[key]
      }
    }

    # ---------- 2. delta decomposition (count vs energy minting) ----------
    printf "\n---- step-to-step changes ----\n"
    printf "%-13s %-3s %-4s %11s %11s %13s %13s %13s\n",
           "interval","dir","kind","dMacro","dN_real","dE_total",
           "from_dN","from_d<En>"
    for (i=2;i<=nit;i++){ it=order[i]; ip=order[i-1]
      for (d=1;d<=nd;d++) for (k=1;k<=nk;k++){
        k2=it SUBSEP dirs[d] SUBSEP kinds[k]
        k1=ip SUBSEP dirs[d] SUBSEP kinds[k]
        if ((k2 in NRL)&&(k1 in NRL)){
          dN=NRL[k2]-NRL[k1]; dE=EN[k2]-EN[k1]
          Ebar=(EN[k2]+EN[k1])/2; Nbar=(NRL[k2]+NRL[k1])/2
          printf "%5d->%-6d %-3s %-4s %11d %11.3e %13.4e %13.4e %13.4e\n",
                 ip,it,dirs[d],kinds[k],NM[k2]-NM[k1],dN,
                 SE[k2]-SE[k1], dN*Ebar, Nbar*dE
        }
      }
    }

    # ---------- 3. side totals vs it=0 and physical cap ----------
    cap = 3.15e22 + 2.0e20
    printf "\n---- side totals ----\n"
    printf "%-7s %-3s %15s %12s %12s  %s\n",
           "it","dir","sumE (eV)","/it0","/cap","flag"
    for (i=1;i<=nit;i++){ it=order[i]
      for (d=1;d<=nd;d++){
        tot=0
        for (k=1;k<=nk;k++){key=it SUBSEP dirs[d] SUBSEP kinds[k]
          if (key in SE) tot+=SE[key]}
        T[it,dirs[d]]=tot
        if (i==1) T0[dirs[d]]=tot
        r0=(T0[dirs[d]]>0)?tot/T0[dirs[d]]:0
        printf "%-7d %-3s %15.5e %12.5f %12.5f  %s\n",
               it,dirs[d],tot,r0,tot/cap,(tot>1.02*cap)?"*** OVER CAP ***":""
      }
    }

    # ---------- 4. machine total (R+L, must be conserved) ----------
    printf "\n---- machine total (R+L, should be constant) ----\n"
    for (i=1;i<=nit;i++){ it=order[i]
      g=T[it,"R"]+T[it,"L"]
      if (i==1) g0=g
      printf "it=%-7d total=%15.6e eV   ratio_to_it0=%10.6f %s\n",
             it,g,g/g0,(g/g0>1.001||g/g0<0.999)?"  <-- NOT CONSERVED":""
    }
}
' "$f"
done
