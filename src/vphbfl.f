      SUBROUTINE VPHBFL(IS,IRTN,MSGFL)
C     Bin fill for virtual photons
      USE BEAMCM
      USE LUMCOM
      IMPLICIT NONE
      INTEGER IS,IRTN,MSGFL
C     INCLUDE 'include/beamcm.h'
C     INCLUDE 'include/lumcom.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER N,L,L1,IXY,IJ(2),K,IVPH,IV,NV,IRTN1
      REAL*8 WGTV(2),PV(3),FAC1,EMAX(2),WMIN(2),FT(3),UPSILN
      INTEGER NVMAX
      PARAMETER (NVMAX=100000)
      REAL*8 EV(NVMAX),XY(2,NVMAX)
      INTEGER IDBG/1/
C     IDBG: debug flag. Also needed is to make the line
C     IF(LPPINT0.GE.1) CALL VPHDBG(2,0D0,0D0,0D0,0D0)
C     in endpsh.f active.
C     
      CALL CPUTIM('VPHBFL',1)
      IRTN=0
C     (Above 1 line added Jan.27.2000 according to Ohgaki)
      EMAX(1)=0
      EMAX(2)=0
      WGTV(1)=0
      WGTV(2)=0
      DO 140 K=0,NDD2(MM)-1
         IPBNV(K,1)=0
         IPBNV(K,2)=0
 140  CONTINUE
c     print *, " vphbfl np= ", np
      DO 160 N=1,NP
         IWORK(N)=0
         IF(LOST(N).NE.0) GOTO 160
         IF(IS.GE.0.AND.ISBIN(N).NE.IS) GOTO 160
         IF(PNAME(N).NE.'    ') GOTO 160
         IF(EP(3,N).GE.0) THEN
            L=1
         ELSE
            L=2
         ENDIF
         IF(KIND(N).EQ.1) THEN
            EMAX(L)=MAX(EMAX(L),EP(0,N))
            GOTO 160
         ENDIF
c     print *, " vphbfl n= ", n," l= ",l," kvph(l)= ",kvph(l)
         IF(IS.GE.0.AND.KVPH(L).EQ.0) GOTO 160
         IWORK(N)=L
         EMAX(L)=MAX(EMAX(L),EP(0,N))
         WGTV(L)=MAX(WGTV(L),WGT(N))
 160  CONTINUE
      DO 200 L=1,2
         L1=3-L
         IF(EMAX(L).LE.0) THEN
            WMIN(L1)=1D61
         ELSE
            WMIN(L1)=EMASS**2/EMAX(L)
         ENDIF
C     The weight of virtual photons is common to all right(left)-going ones.
C     WGTV(L)=WGTV(L)*0.1D0
C     Multiply a small number to increase the number of macro-virtual photons
 200  CONTINUE
c     print *, " vphbfl emax= ", emax, " wmin= ", wmin
      DO 220 N=1,MVPH
         IADRSV(N)=0
 220  CONTINUE
      NVPH=0
      UPSILN=0
      DO 400 N=1,NP
         L=IWORK(N)
         IF(L.EQ.0) GOTO 400
         IF(WMIN(L).GE.1D60) GOTO 400
         IF(IS.GE.0.AND.LFLPPI.NE.0) THEN
            CALL FTREB(EP(0,N),FLD(1,1,N),FT)
            UPSILN=SQRT(FT(1)**2+FT(2)**2+FT(3)**2)*EP(0,N)
     %           *CMPTNWL/EMASS**2
         ENDIF
c     CALL VPHOTON(EP(0,N),WMIN(L),WGT(N)/WGTV(L),LCLPPI,
c     %          UPSILN,NVMAX,NV,EV,XY,IRTN1)
         CALL VPHGEN(EP(0,N),WMIN(L),WGT(N)/WGTV(L),LCLPPI,
     %        UPSILN,NVMAX,NV,EV,XY,IRTN1)
c     print *, " vphbfl n= ",n," nv= ", nv
         IF(IRTN1.NE.0) GOTO 920
         IF(IDBG.GE.1) CALL VPHDBG(1,WGT(N),NV,EV,XY,WGTV(L))
         IF(NV.LE.0) GOTO 400
         FAC1=1/SQRT(EP(1,N)**2+EP(2,N)**2+EP(3,N)**2)
         DO 240 IXY=1,3
            PV(IXY)=FAC1*EP(IXY,N)
 240     CONTINUE
c     23456789012345678901234567890123456789012345678901234567890123456789012
c     10        20        30        40        50        60        70      
         DO 380 IV=1,NV
            IF(IS.GE.0) THEN
               DO 260 IXY=1,2
                  XY(IXY,IV)=XY(IXY,IV)+TXYS(IXY,N)
                  IJ(IXY)=INT((XY(IXY,IV)-XYMLUM(1,IXY))/DXY(IXY)+1D0)-1
                  IF(IJ(IXY).LT.0.OR.IJ(IXY).GE.NDD(MM)) GOTO 380
 260           CONTINUE
               K=KIJDD(IJ(1))+2*KIJDD(IJ(2))
            ENDIF
            NVPH=NVPH+1
            IF(NVPH.GT.MVPH) GOTO 900
            IDVPH(NVPH)=N
            EPVPH(0,NVPH)=EV(IV)
            WGTVPH(NVPH)=WGTV(L)
            DO 280 IXY=1,3
               EPVPH(IXY,NVPH)=EV(IV)*PV(IXY)
 280        CONTINUE
            TXYVPH(0,NVPH)=TXYS(0,N)
            TXYVPH(3,NVPH)=TXYS(3,N)
            DO 320 IXY=1,2
               TXYVPH(IXY,NVPH)=TXYS(IXY,N)+XY(IXY,IV)
 320        CONTINUE
            IF(IS.GE.0) THEN
               IF(IPBNV(K,L).EQ.0) THEN
                  IPBNV(K,L)=NVPH
               ELSE
                  IVPH=IPBNV(K,L)
 360              IF(IADRSV(IVPH).NE.0) THEN
                     IVPH=IADRSV(IVPH)
                     GOTO 360
                  ENDIF
                  IADRSV(IVPH)=NVPH
               ENDIF
            ENDIF
 380     CONTINUE
 400  CONTINUE
      GOTO 1000
 900  IRTN=1000
      WRITE(MSGFL,910)
     %     " VPHBFL Too many virtual photons in a step.  nvph,mvph= ",
     %     nvph,mvph
 910  FORMAT(1x,a,2i12)
      GOTO 1000
 920  IRTN=1000
      WRITE(MSGFL,930)
     %     " VPHBFL Too many virtual photons from electron, irtn1= ",
     %     irtn1 
 930  FORMAT(1x,a,i12)
      GOTO 1000
 1000 CALL CPUTIM('VPHBFL',2)
      RETURN
      END
C------------------------------------
      SUBROUTINE VPHDBG(LL,WGTE,NV,EV,XY,WGT1)
C     LL=2 called by subroutine ENDPSH
      USE BEAMCM
      USE LUMCOM
      IMPLICIT NONE
C     INCLUDE 'include/beamcm.h'
C     INCLUDE 'include/lumcom.h'
      INCLUDE 'include/topdraw.h'
      INCLUDE 'include/ctrlcm.h'
      INTEGER LL,NV
      REAL*8 WGTE,EV(NV),XY(2,NV),WGT1
C     
      INTEGER MB
      PARAMETER (MB=50)
      REAL*8 VMM(2,2)/1D1,1D10,1D-13,1D-5/
      REAL*8 FBIN2(MB,MB),FBIN1(MB,2),WGTESUM,WGTSUM,
     %     FOUT(2,2),VLMM(2,2),DL(2),V(2)
      INTEGER IV,I,J,K,II(2)
      REAL*8 FMM(2,2),F2MM(2)
      REAL*8 VUNIT(2)/1D0,1D0/
      CHARACTER*6 VNAME(2)/'Energy','Radial'/
      CHARACTER*5 VNAM2(2)/'E(eV)','R(m)'/
      INTEGER NELEC/0/,NPH
      SAVE FBIN1,FBIN2,WGTESUM,WGTSUM,FOUT,VLMM,DL,NELEC,NPH
C     
      IF(LL.EQ.2) GOTO 300
      IF(NELEC.EQ.0) THEN
         WGTESUM=0
         WGTSUM=0
         NPH=0
         DO 160 K=1,2
            DO 140 J=1,2
               FOUT(J,K)=0
               VLMM(J,K)=LOG10(VMM(J,K))
 140        CONTINUE
            DL(K)=(VLMM(2,K)-VLMM(1,K))/MB
 160     CONTINUE
         DO 200 I=1,MB
            FBIN1(I,1)=0
            FBIN1(I,2)=0
            DO 180 J=1,MB
               FBIN2(I,J)=0
 180        CONTINUE
 200     CONTINUE
      ENDIF
      NELEC=NELEC+1
      WGTESUM=WGTESUM+WGTE
      IF(NV.LE.0) RETURN
      DO 280 IV=1,NV
         NPH=NPH+1
         WGTSUM=WGTSUM+WGT1
         V(1)=LOG10(EV(IV))
         V(2)=0.5D0*LOG10(XY(1,IV)**2+XY(2,IV)**2)
         I=0
         DO 240 K=1,2
            IF(V(K).LT.VLMM(1,K)) THEN
               FOUT(1,K)=FOUT(1,K)+WGT1
            ELSE
               II(K)=INT((V(K)-VLMM(1,K))/DL(K))+1
               IF(II(K).GT.MB) THEN
                  FOUT(2,K)=FOUT(2,K)+WGT1
               ELSE
                  FBIN1(II(K),K)=FBIN1(II(K),K)+WGT1
                  I=I+1
               ENDIF
            ENDIF
 240     CONTINUE
         IF(I.EQ.2) FBIN2(II(1),II(2))=FBIN2(II(1),II(2))+WGT1
 280  CONTINUE
      RETURN
C     
 300  IF(NPH.LE.0) RETURN
      DO 340 K=1,2
         FMM(1,K)=1D60
         FMM(2,K)=-1D60
         DO 320 I=1,MB
            FMM(1,K)=MIN(FMM(1,K),FBIN1(I,K))
            FMM(2,K)=MAX(FMM(2,K),FBIN1(I,K))
 320     CONTINUE
         FMM(1,K)=0
 340  CONTINUE
      F2MM(1)=1D60
      F2MM(2)=-1D60
      DO 360 I=1,MB
         DO 350 J=1,MB
            F2MM(1)=MIN(F2MM(1),FBIN2(I,J))
            F2MM(2)=MAX(F2MM(2),FBIN2(I,J))
 350     CONTINUE
 360  CONTINUE
C     
      DO 480 K=1,2
         WRITE(TDFL,420) (VMM(J,K)/VUNIT(K),J=1,2),(FMM(J,K),J=1,2),
     %        VNAME(K),VNAM2(K)
 420     FORMAT(' NEWFRAME; SET FONT DUPLEX',/,
     %        ' SET LIMIT X FROM ',1PD11.4,' TO ',1PD11.4,/,
     %        ' SET LIMIT Y FROM ',1PD11.4,' TO ',1PD11.4,/,
     %        ' SET SCALE X LOG',/,
     %        ' TITLE TOP SIZE 1.6 ',2H'',/,
     %        ' MORE ',1H',' Virtual Photon ',A,' Distribution',1H',/,
     %        ' TITLE BOTTOM ',1H',A,1H')
         WRITE(TDFL,430) (10D0**(VLMM(1,K)+DL(K)*(I-0.5D0))/VUNIT(K),
     %        FBIN1(I,K),I=1,MB)
 430     FORMAT(3(1PD11.4,D12.4,';'))
         WRITE(TDFL,440) PATTRN(1)
 440     FORMAT(' SET PATTERN ',A,';JOIN 1 PATTERNED')
         WRITE(TDFL,460) NELEC,NPH,WGTESUM,WGTSUM
c     WRITE(TDFL,460) NELEC,WGTESUM,NPH,WGTSUM    replaced Feb.05.2003
 460     FORMAT(' TITLE 9.0 0.4 SIZE 1.5 ',"''",/,
     %        ' MORE ',"'Macro: N0e1=",I9," N0vG1=",I9,
     %        " Real: N0e1=",1PD9.3," N0vG1=",1PD9.3,"'",/,
     %        ' CASE ',"'        X X ",9X,"  X  X ",9X,
     %        "        X X ",9X,    "  X  X ",9X,    "'")
 480  CONTINUE
      NELEC=0
C     
      RETURN
      END

