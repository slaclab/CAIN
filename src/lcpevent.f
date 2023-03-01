	SUBROUTINE LCPEVENT(N,DT,WGT1,PE2,SPE2,PG2,STK2,LENHCP,IRTN)
C  Add particles after laser-Compton event
C  New particles are appended at the end of the list.
C  The ID of existing particles do not change.
C  To be called when one Compton event happened
C  Input
C   N         particle (electron/positron) ID 
C   DT        time interval
C   WGT1      event weight  (0<=WGT1<=1)
C   PE2(0:3)  
C   SPE2(1:3) 
C   PG2(0:3)  
C   STK2(3)
C   LENHCP    LENHANCE parameter (1,2,3). See CAIN manual
C  Output
C   IRTN      100: too many new particles in one time step.
	USE BEAMCM
      IMPLICIT NONE
	INTEGER N,LENHCP,IRTN
	REAL(8) DT,WGT1,PE2(0:3),SPE2(1:3),PG2(0:3),STK2(3)
	INTEGER I,LBBF,GEN2,IS2
	REAL(8) DT1,TXYS2(0:3),FLD1(3,2),WGT2
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN

C       Distribute the event time over (T0,T0+DT)
      DT1=DT*RANDCAIN()
      DO I=0,3
        TXYS2(I)=TXYS(I,N)+DT1*(EP(I,N)/EP(0,N))
      ENDDO
      DO I=1,3
        FLD1(I,1)=FLD(I,1,N)
        FLD1(I,2)=FLD(I,2,N)
      ENDDO
      LBBF=LBBFIN(N)
      GEN2=GEN(N)+1
      IS2=ISBIN(N)
      IF(WGT1.EQ.1) THEN
        DO I=0,3
          EP(I,N)=PE2(I)
        ENDDO
        GEN(N)=GEN2
        DO I=1,3
          SPIN(I,N)=SPE2(I)
        ENDDO
        CALL ADDONE(0,1,GEN2,'    ',IS2,WGT(N),
     %      TXYS2,PG2,STK2,LBBF,FLD1,IRTN)
        IF(IRTN.NE.0) GOTO 920
      ELSE
	  WGT2=WGT(N)*WGT1
	  IF(LENHCP.LE.1) THEN
C     reat all deterministic
          CALL ADDONE(0,KIND(N),GEN2,'    ',IS2,WGT2,
     %       TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
          CALL ADDONE(0,1,GEN2,'    ',IS2,WGT2,
     %       TXYS2,PG2,STK2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
	    WGT(N)=WGT(N)-WGT2
	  ELSEIF(LENHCP.EQ.2) THEN
	    CALL ADDONE(0,1,GEN2,'    ',IS2,WGT2,
     %       TXYS2,PG2,STK2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
C     reat electron probabilistically
	    IF(RANDCAIN().LE.WGT1) THEN	        
	      CALL ADDONE(0,KIND(N),GEN2,'    ',IS2,WGT(N),
     %        TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
	      IF(IRTN.NE.0) GOTO 920
	      LOST(N)=1
            WGT(N)=0
	    ENDIF
	  ELSE
C     reat photon probabilistically
	    IF(RANDCAIN().LE.WGT1) THEN
            CALL ADDONE(0,1,GEN2,'    ',IS2,WGT(N),
     %        TXYS2,PG2,STK2,LBBF,FLD1,IRTN)
            IF(IRTN.NE.0) GOTO 920
	    ENDIF
          CALL ADDONE(0,KIND(N),GEN2,'    ',IS2,WGT2,
     %       TXYS2,PE2,SPE2,LBBF,FLD1,IRTN)
          IF(IRTN.NE.0) GOTO 920
	    WGT(N)=WGT(N)-WGT2
	  ENDIF
      ENDIF
	IRTN=0
	RETURN
920   IRTN=100
	RETURN
	END