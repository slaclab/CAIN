	SUBROUTINE LSRCOORD(LSR,TXYS,FLG)
C  Laser coordinate  <--> world coordinate (in place)
C      FLG  <=0:  world --> laser
C           >=1:  laser --> world
	USE LASRDATA
	IMPLICIT NONE
	INTEGER LSR,FLG
	REAL*8 TXYS(0:3)
	INTEGER I,J
	REAL*8 TXYS1(0:3)
	INCLUDE 'include/lasrcm.h'
	
	IF(LSR.LE.0.OR.LSR.GT.NLSR) RETURN
	IF(FLG.LE.0) THEN
	  IF(LTRLSR(LSR).GE.1) THEN
          DO 160 I=0,3
            TXYS1(I)=TXYS(I)-TR0LSR(I,LSR)
 160      CONTINUE
          CALL MATVEC(TRILSR(0,0,LSR),4,4,TXYS1)
        ELSE
          DO 180 I=0,3
            TXYS1(I)=TXYS(I)
 180      CONTINUE
        ENDIF
	  TXYS(0)=TXYS1(0)-TXYSLS(0,LSR)
        DO 200 I=1,3
          TXYS1(I)=TXYS1(I)-TXYSLS(I,LSR)
 200    CONTINUE
        DO 240 I=1,3
          TXYS(I)=0
          DO 220 J=1,3
            TXYS(I)=TXYS(I)+TXYS1(J)*EVLSR(J,I,LSR)
 220      CONTINUE
 240    CONTINUE
	ELSE
	  DO 340 I=1,3
	    TXYS1(I)=TXYSLS(I,LSR)
	    DO 320 J=1,3
	      TXYS1(I)=TXYS1(I)+EVLSR(I,J,LSR)*TXYS(J)
320       CONTINUE
340     CONTINUE
        TXYS1(0)=TXYS(0)+TXYSLS(0,LSR)
	  IF(LTRLSR(LSR).GE.1) THEN
	    CALL MATVEC(TRLSR(0,0,LSR),4,4,TXYS1)
          DO 360 I=0,3
            TXYS(I)=TXYS1(I)+TR0LSR(I,LSR)
360       CONTINUE
        ELSE
          DO 380 I=0,3
            TXYS(I)=TXYS1(I)
380       CONTINUE
	  ENDIF
	ENDIF
	RETURN
	END