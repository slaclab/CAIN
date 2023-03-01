      INTEGER,PARAMETER:: MLUM=9,MWLUM=200000
      INTEGER KLUM(2,MLUM),KKLUM(3,2),IPWLUM(0:2,MLUM),
     %   IPELUM(0:2,MLUM),IPPFLG(MLUM),LBINW(MLUM),LBINE(2,MLUM),
     %   IPBINW(MLUM),IPBINE(2,MLUM),
     %   NBNWLM(MLUM),NBNELM(2,MLUM),LHEL(0:2,MLUM),NLUM,IPLUM0
C LBINW:  0:no W bin, 1:equal-space bin, 2:unequal-space bin
C IPBINW: Pointer in DLUM for the bin edges of center-of-mass energy.
C         If 0, the bin is equal width and is defined by WMMLUM and
C         NBNWLM. If >0, the bin edge is defined by DWLUM(IPBINW+i),
C         (i=0,1,...NBNWLM).
C LBINE,IPBINE: Similar to IPBINW but for 2-dim luminosity.
      REAL*8 DLUM(MWLUM),LUM(0:20,MLUM),VLUM(MLUM),
     %   WMMLUM(2,MLUM),EMMLUM(2,2,MLUM)
      COMMON/LUMCOM2/DLUM,LUM,VLUM,WMMLUM,EMMLUM,
     %   NLUM,IPLUM0,KLUM,KKLUM,IPWLUM,IPELUM,IPPFLG,
     %   LBINW,IPBINW,LBINE,IPBINE,
     %   NBNWLM,NBNELM,LHEL
      INTEGER MWLUMR,MWLUMI
      PARAMETER (MWLUMR=21*MLUM+MLUM+2*MLUM+4*MLUM,
     %   MWLUMI=1+1+2*MLUM+6+2*MLUM+3*MLUM+MLUM+MLUM+2*MLUM+3*MLUM)
C      MWLUMR: length of real*8 data in LUMCOM2 except DLUM
C      MWLUMI: length of integer*4 data in LUMCOM2
C               ("NLUM" must be the first element)
C