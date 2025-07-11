      SUBROUTINE BBMESH(T,IS,IRTN)
	USE BEAMCM
	USE BBCOM
      IMPLICIT NONE
      INTEGER IS,IRTN
      REAL*8 T
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      INTEGER IXY,L,N
      REAL*8 WGTL(2),XYR(2,2)
      REAL*8 OUTMAX/0.04D0/
C     
      CALL CPUTIM('BBMESH',1)
      DO 200 L=1,2
        BBFLG(L)=0
 200  CONTINUE
      DO 220 N=1,NP
        IWORK(N)=0
        IF(LOST(N).NE.0) GOTO 220 
        IF(ISBIN(N).NE.IS) GOTO 220
        IF(KIND(N).EQ.1) GOTO 220				
        IF(EP(3,N).GE.0) THEN
          L=1
        ELSE
          L=2
        ENDIF
        BBFLG(3-L)=1
C          BBFLG: whether there is on-coming particle or not.
C                  (include test particles)
        IF(PNAME(N).NE.'    ') GOTO 220
        IF(TXYS(0,N).GT.T) GOTO 220
C          exclude those created at time>T.
        IWORK(N)=L
        DO 210 IXY=1,2
          WORK(IXY+2*(N-1))=TXYS(IXY,N)+EP(IXY,N)/EP(0,N)*(T-TXYS(0,N))
 210    CONTINUE
 220  CONTINUE
c			print *,' (BBMESH) call BINRNG'
      CALL BINRNG(NP,2,IWORK,WGT,WORK,BBXYM,OUTMAX,1,WGTL,XYCENT,XYR)
      IF(WGTL(1).EQ.0) BBFLG(1)=0
      IF(WGTL(2).EQ.0) BBFLG(2)=0
C           Now BBFLG: whether the bb-field is needed or not.
      IF(BBFLG(1).EQ.0.AND.BBFLG(2).EQ.0) GOTO 900
      DO 250 IXY=1,2
        DO 240 L=1,2
          IF(WGTL(L).EQ.0) XYCENT(IXY,L)=XYCENT(IXY,3-L)
 240    CONTINUE
 250  CONTINUE
      DO 300 L=1,2
        DO 280 IXY=1,2
          BBDXY(IXY,L)=XYR(IXY,L)/NXY(IXY)
          XYMIN(IXY,L)=XYCENT(IXY,L)-NXY(IXY)*BBDXY(IXY,L)/2
          XYMAX(IXY,L)=XYCENT(IXY,L)+NXY(IXY)*BBDXY(IXY,L)/2
 280  CONTINUE
 300  CONTINUE
      IRTN=0
      CALL CPUTIM('BBMESH',2)
      RETURN
C     
 900  IRTN=1
      CALL CPUTIM('BBMESH',2)
      RETURN
      END
