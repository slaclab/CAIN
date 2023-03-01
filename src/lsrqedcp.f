	SUBROUTINE LSRQEDCP(T1,IRTN)
	USE BEAMCM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER IRTN
      REAL*8 T1
      INCLUDE 'include/lasrcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER NP0,N,LSR,LR,ISPNCP
      REAL*8 TXYS1(0:3),PD,PD0,EV(3,3),PE2(0:3),PG2(0:3),PP2(0:3),
     %   PROB,WGT1,T2,DT,SPE2(3),STK2(3),GVEC(3,3),OMG,PMAXCP1,PMAX1,
     %   PDMAX
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      INTEGER I,NPH1,NDIV
	INTEGER NDBG/0/
	SAVE NDBG

C   When artificially enhanced event rate is used, the generator creates an event
C   with event weight WGT1 (0<WGT1<=1).
C   Enhance policy
C      1:  Leave initial particle with the reduced weight w0*(1-WGT1)
C          (w0= initial weight of the initial particle)
C          and create new particle(s) with weight w0*WGT1
C      2:  Eliminate the initial particle with the probability WGT1 
C          (otherwise leave it as it was)
C          and create new particle(s) with weight w0*WGT1
C      3:  Leave initial particle with the reduced weight w0*(1-WGT1)
C          and create new particle(s) with the probability WGT1
C   This parameter was introduced 2000 Aug.25.
C
      IRTN=0
      IF(NLSR.LE.0.OR.NPHCP.LT.0) RETURN
      CALL CPUTIM('LSRQEDCP',1)
      ISPNCP=1
      IF(ISPIN.EQ.0.OR.NPHCP.GE.1) ISPNCP=0
	PMAXCP1=MIN(1D0,PMAXCP)
C            non-linear qed is not ready for transverse polarization
      NP0=NP
C        Note that NP can change when calling ADDONE.
C  Do loop nest has been changed such that do over LSR is outside N
C  on Jun.24 in order to accept event probalility > 1 in DT. 
C  This will cause changes of random number generation when 
C  there are more than one lasers. Also, the random number generation
C  will be affected by this change.
      DO LSR=1,NLSR
	  OMG=OMGLSR(LSR)
	  DO N=1,NP0
	    IF(LOST(N).NE.0) CYCLE
          IF(PNAME(N).NE.'    ') CYCLE
	    IF(KIND(N).NE.2.AND.KIND(N).NE.3) CYCLE
	    LR=1
          IF(EP(3,N).LT.0) LR=2
          IF(LRLSR(LSR).EQ.LR) CYCLE
	    IF(T1.LE.TXYS(0,N)) CYCLE
          T2=TXYS(0,N)
          NDIV=1
	    PMAX1=0
	    PDMAX=0
100       CONTINUE
          DT=(T1-T2)/NDIV
          DO I=0,3
            TXYS1(I)=TXYS(I,N)+(T2-TXYS(0,N)+0.5*DT)*(EP(I,N)/EP(0,N))
          ENDDO
          CALL LSRGEO(LSR,TXYS1,PD,PD0,EV,OMG,ISPNCP)
	    PROB=0
          IF(PD.EQ.0) GOTO 200
          IF(NPHCP.EQ.0) THEN
            CALL LNCPGN(EP(1:3,N),OMG,EV(1:3,3),SPIN(1:3,N),
     %        EV(1:3,1),EV(1:3,2),STKSLS(1:3,LSR),
     %        PD,DT,PMAXCP1,ISPIN,ISPIN,ISPIN,
     %        NPH1,PE2(1:3),PG2(1:3),SPE2,STK2,0,PROB,IRTN)
            IF(NPH1.GE.1) THEN
              PE2(0)=SQRT(MASS(2)**2+PE2(1)**2+PE2(2)**2+PE2(3)**2)
              PG2(0)=SQRT(PG2(1)**2+PG2(2)**2+PG2(3)**2)
              WGT1=1
            ENDIF
          ELSE
	      CALL NLCPGN(N,LSR,OMG,EV,PD,DT,PMAXCP1,NPH1,WGT1,PROB,PE2,
     %              PG2,SPE2,STK2,IRTN)
          ENDIF
	    IF(IRTN.NE.0) THEN
	      NDIV=INT(NDIV*PROB/PMAXCP1+1D0)
	      GOTO 100
	    ENDIF
          IF(NPH1.GT.0) THEN
	      CALL LCPEVENT(N,DT,WGT1,PE2,SPE2,PG2,STK2,LENHCP,IRTN)
	      IF(IRTN.NE.0) GOTO 920
	    ENDIF
200	    T2=T2+DT
	    NDIV=NDIV-1
	    PMAX1=PMAX1+PROB
	    PDMAX=MAX(PDMAX,PD)
	    IF(NDIV.GE.1) GOTO 100
	    PMMCP=MAX(PMMCP,PMAX1)
	    IF(PMAX1.GT.PSTOPCP) GOTO 900
        ENDDO
      ENDDO
      IRTN=0
      GOTO 1000
C
 900  IRTN=1000
      WRITE(MSGFL,905) PMAX1,PDMAX,EP(0,N)
 905  FORMAT(' (SUBR.LSRQEDCP) Pstop for Compton exceeded limit.',/,
     %   '   Prob=',1PD10.3,'  for Power density=',1PD10.3,' W/m^2',
     %   'E(e)=',1PD9.3,'eV')
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.LSRQEDCP) Too many new particles in one time step')
      GOTO 1000
 1000 CALL CPUTIM('LSRQEDCP',2)
      RETURN
      END
