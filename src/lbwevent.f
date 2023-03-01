	SUBROUTINE LBWEVENT(N,DT,WGT1,PE2,SPE2,PP2,SPP2,LENHBW,IRTN)
C  Add particles after laser-BreitWheeler event
C  New particles are appended at the end of the list.
C  The ID of existing particles do not change.
C  To be called when one BreitWheeler event happened
C  Input
C   N         particle (electron/positron) ID 
C   DT        time interval
C   WGT1      event weight  (0<=WGT1<=1)
C   PE2(0:3)  
C   SPE2(1:3) 
C   LENHBW    LENHANCE parameter (1,2,3). See CAIN manual
C  Output
C   IRTN      100: too many new particles in one time step.

	USE BEAMCM
      IMPLICIT NONE
	INTEGER N,LENHBW,IRTN
	REAL(8) DT,WGT1,PE2(0:3),SPE2(1:3),PP2(0:3),SPP2(3)
	INTEGER I,LBBF,GEN2,IS2
	REAL(8) DT1,TXYS2(0:3),FLD1(3,2),WGT2
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN

      DT1=DT*RANDCAIN()
      DO 440 I=0,3
        TXYS2(I)=TXYS(I,N)+DT1*(EP(I,N)/EP(0,N))
 440  CONTINUE
      DO 450 I=1,3
        FLD1(I,1)=FLD(I,1,N)
        FLD1(I,2)=FLD(I,2,N)
 450  CONTINUE
      LBBF=LBBFIN(N)
      GEN2=GEN(N)+1
      IS2=ISBIN(N)
      IF(WGT1.EQ.1) THEN
        CALL ADDONE(0,2,GEN2,'    ',IS2,WGT(N),
     %        TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
        IF(IRTN.NE.0) GOTO 920
        CALL ADDONE(0,3,GEN2,'    ',IS2,WGT(N),
     %        TXYS2,PP2,SPP2,LBBF,FLD1,IRTN)
        IF(IRTN.NE.0) GOTO 920
        LOST(N)=1
        WGT(N)=0
      ELSE
	  WGT2=WGT(N)*WGT1
		IF(LENHBW.LE.1) THEN
	    CALL ADDONE(0,2,GEN2,'    ',IS2,WGT2,
     %        TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
          CALL ADDONE(0,3,GEN2,'    ',IS2,WGT2,
     %        TXYS2,PP2,SPP2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
	    WGT(N)=WGT(N)-WGT2
	  ELSEIF(LENHBW.EQ.2) THEN
	    CALL ADDONE(0,2,GEN2,'    ',IS2,WGT2,
     %        TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
          CALL ADDONE(0,3,GEN2,'    ',IS2,WGT2,
     %        TXYS2,PP2,SPP2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
	    IF(RANDCAIN().LE.WGT1) THEN
	      LOST(N)=1
            WGT(N)=0
	    ENDIF
	  ELSE
	    IF(RANDCAIN().LE.WGT1) THEN
	      CALL ADDONE(0,2,GEN2,'    ',IS2,WGT(N),
     %        TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
            IF(IRTN.NE.0) GOTO 920
            CALL ADDONE(0,3,GEN2,'    ',IS2,WGT(N),
     %        TXYS2,PP2,SPP2,LBBF,FLD1,IRTN)
            IF(IRTN.NE.0) GOTO 920
	    ENDIF
          WGT(N)=WGT(N)-WGT2
	  ENDIF
      ENDIF
	IRTN=0
	RETURN
920   IRTN=100
	RETURN
	END