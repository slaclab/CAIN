      SUBROUTINE LUMBFL(T,IS)
C           Corrected on May.17.1999
C  Bin fill for luminosity
	USE BEAMCM
	USE LUMCOM
      IMPLICIT NONE
      INTEGER IS
      REAL*8 T
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/lumcom2.h'
      INTEGER N,L,IXY,IJ(2),KIN,K,K0,KN,IP,IP1
      REAL*8 XYC(2),XYR(2),WGTL,XY1
      REAL*8 OUTMAX/0.05D0/
C
      CALL CPUTIM('LUMBFL',1)
      DO 140 L=1,2
        DO 120 KIN=1,3
          NPKINL(KIN,L)=0
 120    CONTINUE
 140  CONTINUE
      DO 160 N=1,NP
        IWORK(N)=0
	  IF(LOST(N).NE.0) GOTO 160
        IF(ISBIN(N).NE.IS) GOTO 160
        IF(PNAME(N).NE.'    ') GOTO 160
        IF(EP(3,N).GE.0) THEN
          L=1
        ELSE
          L=2
        ENDIF
        IF(KKLUM(KIND(N),L).EQ.0) GOTO 160
        NPKINL(KIND(N),L)=NPKINL(KIND(N),L)+1
        IWORK(N)=L
        DO 150 IXY=1,2
          WORK(IXY+2*(N-1))=TXYS(IXY,N)+EP(IXY,N)/EP(0,N)*(T-TXYS(0,N))
 150    CONTINUE
 160  CONTINUE
      N=MIN(NPKINL(1,1)+NPKINL(2,1)+NPKINL(3,1),
     %        NPKINL(1,2)+NPKINL(2,2)+NPKINL(3,2))
      IF(N.LE.1) THEN
        MM=0
        GOTO 1000
      ENDIF
c      MM=MIN(MAX(NPKINL(1,1),NPKINL(2,1),NPKINL(3,1)),
c     %      MAX(NPKINL(1,2),NPKINL(2,2),NPKINL(3,2)))
c      MM=MAX(3,MIN(MMM,INT(LOG10(DFLOAT(MM))+3)))
      MM=6
      CALL BINRNG(NP,1,IWORK,WGT,WORK,XYLMM,OUTMAX,0,WGTL,XYC,XYR)
      MM=MIN(MAX(NPKINL(1,1),NPKINL(2,1),NPKINL(3,1)),
     %      MAX(NPKINL(1,2),NPKINL(2,2),NPKINL(3,2)))
      MM=MAX(3,MIN(MMM,INT(LOG10(DFLOAT(MM))+3)))
      DO 220 IXY=1,2
        DXY(IXY)=XYR(IXY)/NDD(MM)
        XYMLUM(1,IXY)=XYC(IXY)-XYR(IXY)/2
        XYMLUM(2,IXY)=XYC(IXY)+XYR(IXY)/2
 220  CONTINUE
      LUMFAC=2D0*FREP*DTLUM/(NDD2(MM)*DXY(1)*DXY(2)*DSLUM)*1D-4
C       factor 2: relative velocity (assume almost head-on)
C       factor 1D-4:  1/m**2 --> 1/cm**2
C
      DO 320 L=1,2
        DO 310 KIN=1,3
CC          IF(NPKINL(KIN,L).NE.0) THEN
            DO 300 K=0,NDD2(MM)-1
              DIST(IPDD(MM)+K,KIN,L)=0
              VDIST(IPDD(MM)+K,KIN,L)=0
              IPBN(K,KIN,L)=0
 300        CONTINUE
CC          ENDIF
 310    CONTINUE
 320  CONTINUE
      DO 330 N=1,NP
        IADRS(2,N)=0
 330  CONTINUE
C
      IP=0
      DO 400 N=1,NP
        L=IWORK(N)
        IF(L.EQ.0) GOTO 400
        KN=KIND(N)
        DO 340 IXY=1,2
          XY1=WORK(IXY+2*(N-1))
          IJ(IXY)=INT((XY1-XYMLUM(1,IXY))/DXY(IXY)+1D0)-1
          IF(IJ(IXY).LT.0.OR.IJ(IXY).GE.NDD(MM)) GOTO 400
 340    CONTINUE
        K=KIJDD(IJ(1))+2*KIJDD(IJ(2))
        K0=IPDD(MM)+K
        DIST(K0,KN,L)=DIST(K0,KN,L)+WGT(N)
        VDIST(K0,KN,L)=VDIST(K0,KN,L)+WGT(N)**2
        IP=IP+1
        IF(IPBN(K,KN,L).EQ.0) THEN
          IPBN(K,KN,L)=IP
        ELSE
          IP1=IPBN(K,KN,L)
 360      IF(IADRS(2,IP1).NE.0) THEN
            IP1=IADRS(2,IP1)
            GOTO 360
          ENDIF
          IADRS(2,IP1)=IP
        ENDIF
        IADRS(1,IP)=N
 400  CONTINUE
CC      CALL CHKADRS('lumbfl.f',0)
 1000 CALL CPUTIM('LUMBFL',2)
      RETURN
      END
      SUBROUTINE CHKADRS(TEXT,ISTOP)
	USE BEAMCM
	USE LUMCOM
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER ISTOP
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/lumcom.h'
	INCLUDE 'include/ctrlcm.h'
      INTEGER L,K,KN,I,N,II
      KN=1
      L=1
      DO 280 K=0,NDD2(MM)-1
        I=IPBN(K,KN,L)
        DO WHILE (I.GE.1)
          N=IADRS(1,I)
          I=IADRS(2,I)
          IF(N.LE.0.OR.N.GT.NP) THEN
            WRITE(MSGFL,220) TEXT,N,IPBN(K,KN,L),K
 220        FORMAT(' Error in ',A,'  N=',I10,' IPBN=',I10,
     %             ' K=',I10)
            II=IPBN(K,KN,L)
            DO WHILE (II.GE.1)
              WRITE(MSGFL,240) II,IADRS(1,II),II,IADRS(2,II)
 240          FORMAT(' N=IADRS(1,',I5,')=',I10,
     %               ' N=IADRS(2,',I5,')=',I10)
              II=IADRS(2,II)
            ENDDO
            IF(ISTOP.NE.0) CALL STOPCAIN(0)
          ENDIF
        ENDDO
 280  CONTINUE
      RETURN
      END

