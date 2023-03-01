      SUBROUTINE LSRQED(T1,IRTN)
C  This routine has been replaced with LSRQEDCP and LSRQEDBW 
C  on Jun.24, 2011. Obsolete since then.
	USE BEAMCM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER IRTN
      REAL*8 T1
      INCLUDE 'include/lasrcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER NP0,N,LSR,LR,ISPNCP,ISPNBW
      REAL*8 TXYS1(0:3),PD,PD0,EV(3,3),PE2(0:3),PG2(0:3),
     %   PP2(0:3),HE1,
     %   HE2,HG2,HP2,PROB,WGT1,DT,SPE2(3),SPP2(3),STK2(3),
     %   GVEC(3,3),OMG,
     %   ABSP,ABSP2,ABSPE,ABSPP
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      INTEGER I,NPH1,iii

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
      IF(NLSR.LE.0.OR.(NPHCP.LT.0.AND.NPHBW.LT.0)) RETURN
      CALL CPUTIM('LSRQED',1)
      ISPNCP=1
      IF(ISPIN.EQ.0.OR.NPHCP.GE.1) ISPNCP=0
      ISPNBW=1
      IF(ISPIN.EQ.0.OR.NPHBW.GE.1) ISPNBW=0
C            non-linear qed is not ready for transverse polarization
      NP0=NP
C        Note that NP can change when calling ADDONE.
      N=0
 200  N=N+1
      IF(N.GT.NP0) GOTO 600
	IF(LOST(N).NE.0) GOTO 200
      IF(PNAME(N).NE.'    ') GOTO 200
      DT=T1-TXYS(0,N)
      IF(DT.LE.0) GOTO 200
      DO 210 I=0,3
        TXYS1(I)=TXYS(I,N)+0.5*DT*(EP(I,N)/EP(0,N))
 210  CONTINUE
      LR=1
      IF(EP(3,N).LT.0) LR=2
      IF(KIND(N).EQ.1) GOTO 400
      IF(NPHCP.LT.0) GOTO 200
C-- Laser-Compton
      DO 300 LSR=1,NLSR
        IF(LRLSR(LSR).EQ.LR) GOTO 300
        OMG=OMGLSR(LSR)
        CALL LSRGEO(LSR,TXYS1,PD,PD0,EV,OMG,ISPNCP)
        IF(PD.EQ.0) GOTO 300
        IF(NPHCP.EQ.0) THEN
          CALL LNCPGN(EP(1,N),OMG,EV(1,3),SPIN(1,N),
     %      EV(1,1),EV(1,2),STKSLS(1,LSR),
     %      PD,DT,PMAXCP,ISPIN,ISPIN,ISPIN,
     %      NPH1,PE2(1),PG2(1),SPE2,STK2,0,PROB,IRTN)
          IF(IRTN.NE.0) GOTO 900
          PMMCP=MAX(PMMCP,PROB)
          IF(NPH1.GE.1) THEN
            PE2(0)=SQRT(MASS(2)**2+PE2(1)**2+PE2(2)**2+PE2(3)**2)
            PG2(0)=SQRT(PG2(1)**2+PG2(2)**2+PG2(3)**2)
            WGT1=1
          ENDIF
        ELSE
          IF(ISPIN.GE.1) THEN
            HE1=EP(1,N)*SPIN(1,N)+EP(2,N)*SPIN(2,N)+EP(3,N)*SPIN(3,N)
            ABSP=SQRT(EP(1,N)**2+EP(2,N)**2+EP(3,N)**2)
            HE1=HE1/ABSP
          ENDIF
!       Begin Change by Li Dongguo March.11.2003 
          CALL NLCPGN0(EP(0:3,N),OMG,EV,HE1,STKSLS(1:3,LSR),
     %      PD,DT,PMAXCP,ISPIN,
     %      NPH1,PE2,HE2,PG2,STK2,PROB,WGT1,IRTN)

!       End Change by Li Dongguo March.11.2003
!       Old Version before Change March.11.2003
!          CALL NLCPGN0(EP(0,N),OMG,EV(1,3),HE1,STKSLS(2,LSR),
!     %      PD,DT,PMAXCP,ISPIN,
!     %      NPH1,PE2,HE2,PG2,HG2,PROB,WGT1,IRTN)

          IF(IRTN.NE.0) GOTO 900
          PMMCP=MAX(PMMCP,PROB)
C          ----- for circular polarization till next ++++++
          IF(NPH1.GE.1) THEN
            IF(ISPIN.GE.1) THEN
              ABSP2=SQRT(PE2(1)**2+PE2(2)**2+PE2(3)**2)
              DO 220 I=1,3
                SPE2(I)=HE2*PE2(I)/ABSP2
 220          CONTINUE
!              STK2(1)=0
!              STK2(2)=HG2
!              STK2(3)=0
            ENDIF
          ENDIF
          IF(ISPIN.GE.1.AND.(NPH1.EQ.0.OR.WGT1.LT.1)) THEN
            DO 230 I=1,3
              SPIN(I,N)=HE1*EP(I,N)/ABSP
 230        CONTINUE
          ENDIF
c                +++++++++++++++++++++++++++
        ENDIF
        IF(NPH1.GT.0) THEN
	    CALL LCPEVENT(N,DT,WGT1,PE2,SPE2,PG2,STK2,LENHCP,IRTN)
	    IF(IRTN.NE.0) GOTO 920
	  ENDIF
 300  CONTINUE
      GOTO 200
 400  IF(NPHBW.LT.0) GOTO 200
C-- Laser-Breit-Wheeler
      DO 500 LSR=1,NLSR
        IF(LRLSR(LSR).EQ.LR) GOTO 500
        OMG=OMGLSR(LSR)
C        CALL LSRGEO(LSR,TXYS(0,N),PD,PD0,EV,OMG,ISPNBW)
C           Replaced by the following line. Jun.23.2011
        CALL LSRGEO(LSR,TXYS1,PD,PD0,EV,OMG,ISPNBW)
        IF(PD.EQ.0) GOTO 500
        IF(NPHBW.EQ.0) THEN
          DO 420 I=1,3
            GVEC(I,3)=EP(I,N)/EP(0,N)
            GVEC(I,1)=0
            GVEC(I,2)=0
 420      CONTINUE
          GVEC(1,1)=1
          GVEC(2,2)=1
          IF(EP(3,N).LT.0) GVEC(2,2)=-1
          CALL LNBWGN(OMG,EV(1,3),STKSLS(1,LSR),
     %      EV(1,1),EV(1,2),EP(0,N),GVEC(1,3),SPIN(1,N),
     %      GVEC(1,1),GVEC(1,2),PD,DT,ISPIN,
     %      NPH1,PE2(1),PP2(1),SPE2,SPP2,0)
cccc          IF(PROB.GT.PMAXBW) GOTO 910
          IF(NPH1.GE.1) THEN
            PE2(0)=SQRT(MASS(2)**2+PE2(1)**2+PE2(2)**2+PE2(3)**2)
            PP2(0)=SQRT(MASS(2)**2+PP2(1)**2+PP2(2)**2+PP2(3)**2)
            WGT1=1
          ENDIF
        ELSE
          CALL NLBWGN(EP(0,N),SPIN(2,N),OMG,EV(1,3),STKSLS(2,LSR),
     %     PD,DT,PMAXBW,ISPIN,NPH1,PE2,HE2,PP2,HP2,PROB,WGT1,IRTN)
          IF(IRTN.NE.0) GOTO 910
          IF(NPH1.GE.1) THEN
            IF(ISPIN.GE.1) THEN
              ABSPE=SQRT(PE2(1)**2+PE2(2)**2+PE2(3)**2)
              ABSPP=SQRT(PP2(1)**2+PP2(2)**2+PP2(3)**2)
              DO 430 I=1,3
                SPE2(I)=HE2*PE2(I)/ABSPE
                SPP2(I)=HP2*PP2(I)/ABSPP
 430          CONTINUE
            ENDIF
          ENDIF
        ENDIF
        PMMBW=MAX(PMMBW,PROB)
        IF(NPH1.GT.0) THEN
          CALL LBWEVENT(N,DT,WGT1,PE2,SPE2,PP2,SPP2,LENHBW,IRTN)
	    IF(IRTN.NE.0) GOTO 920
	  ENDIF
 500  CONTINUE
      GOTO 200
 600  IRTN=0
      GOTO 1000
C
 900  IRTN=1000
      WRITE(MSGFL,905) PROB,PD
 905  FORMAT(' (SUBR.LSRQED) Pmax for Compton exceeded limit.',/,
     %   '   Prob=',1PD10.3,'  for Power density=',1PD10.3,' W/m^2')
      GOTO 1000
 910  IRTN=1001
      WRITE(MSGFL,915) PROB,PD,EP(0,N)
 915  FORMAT(' (SUBR.LSRQED) Pmax for Breit-Wheeler exceeded limit.',/,
     %   '   Prob=',1PD10.3,'  for Power density=',1PD10.3,' W/m^2',
     %   'E(photon)=',1PD9.3,'eV')
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.LSRQED) Too many new particles in one time step.')
      GOTO 1000
 1000 CALL CPUTIM('LSRQED',2)
      RETURN
      END
