	SUBROUTINE LSRQEDBW(T1,IRTN)
	USE BEAMCM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER IRTN
      REAL*8 T1
      INCLUDE 'include/lasrcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER NP0,N,LSR,LR,ISPNBW,NDIV
      REAL*8 TXYS1(0:3),PD,PD0,EV(3,3),PE2(0:3),PP2(0:3),
     %   HE2,HP2,PROB,WGT1,T2,DT,SPE2(3),SPP2(3),GVEC(3,3),
     %   OMG,ABSPE,ABSPP,PMAXBW1,PMAX1,PDMAX
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
	INTEGER I,NPH1
	INTEGER EMSG
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
      IF(NLSR.LE.0.OR.NPHBW.LT.0) RETURN
      CALL CPUTIM('LSRQEDBW',1)
      ISPNBW=1
      IF(ISPIN.EQ.0.OR.NPHBW.GE.1) ISPNBW=0
C            non-linear qed is not ready for transverse polarization
	PMAXBW1=MIN(1D0,PMAXBW)
      NP0=NP
C        Note that NP can change when calling ADDONE.
	DO LSR=1,NLSR
	  OMG=OMGLSR(LSR)
        DO N=1,NP0
	    IF(LOST(N).NE.0) CYCLE
          IF(PNAME(N).NE.'    ') CYCLE
	    IF(KIND(N).NE.1) CYCLE
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
          CALL LSRGEO(LSR,TXYS1,PD,PD0,EV,OMG,ISPNBW)
	    PROB=0
          IF(PD.EQ.0) GOTO 200
          IF(NPHBW.EQ.0) THEN
            DO I=1,3
              GVEC(I,3)=EP(I,N)/EP(0,N)
              GVEC(I,1)=0
              GVEC(I,2)=0
            ENDDO
            GVEC(1,1)=1
            GVEC(2,2)=1
            IF(EP(3,N).LT.0) GVEC(2,2)=-1
c	    if(ep(0,n).gt.62.6e9.or.abs(spin(2,n)).gt.1.05) then
c	    print *, " omg= ", omg, " ev(:,3)= ", ev(:,3),
c     %         " stksls(:,lsr)= ", stksls(:,lsr)
c            print *, " ev(:,1)= ", ev(:,1), " ev(:,2)= ", ev(:,2)
c	    print *, " ep(0,n)= ", ep(0,n), " gvec(:,3)= ", gvec(:,3),
c     %         " spin(:,n)= ", spin(:,n)
c            print *, " gvec(:,1)= ", gvec(:,1), " gvec(:,2)= ", gvec(:,2)
c	    emsg=10000
c	    else
c	    emsg=0
c	    endif
            CALL LNBWGN(OMG,EV(1,3),STKSLS(1,LSR),
     %        EV(1,1),EV(1,2),EP(0,N),GVEC(1,3),SPIN(1,N),
     %        GVEC(1,1),GVEC(1,2),PD,DT,ISPIN,
     %        NPH1,PE2(1),PP2(1),SPE2,SPP2,0)
c     %        NPH1,PE2(1),PP2(1),SPE2,SPP2,emsg)
            IF(NPH1.GE.1) THEN
              PE2(0)=SQRT(MASS(2)**2+PE2(1)**2+PE2(2)**2+PE2(3)**2)
              PP2(0)=SQRT(MASS(2)**2+PP2(1)**2+PP2(2)**2+PP2(3)**2)
              WGT1=1
            ENDIF
          ELSE
            CALL NLBWGN(EP(0,N),SPIN(2,N),OMG,EV(1,3),STKSLS(2,LSR),
     %       PD,DT,PMAXBW1,ISPIN,NPH1,PE2,HE2,PP2,HP2,PROB,WGT1,IRTN)
            IF(NPH1.GE.1) THEN
              IF(ISPIN.GE.1) THEN
                ABSPE=SQRT(PE2(1)**2+PE2(2)**2+PE2(3)**2)
                ABSPP=SQRT(PP2(1)**2+PP2(2)**2+PP2(3)**2)
                DO I=1,3
                  SPE2(I)=HE2*PE2(I)/ABSPE
                  SPP2(I)=HP2*PP2(I)/ABSPP
                ENDDO
              ENDIF
            ENDIF
          ENDIF
	    IF(IRTN.NE.0) THEN
	      NDIV=INT(NDIV*PROB/PMAXBW1+1D0)
	      GOTO 100
	    ENDIF
          IF(NPH1.GT.0) THEN
            CALL LBWEVENT(N,DT,WGT1,PE2,SPE2,PP2,SPP2,LENHBW,IRTN)
	      IF(IRTN.NE.0) GOTO 920
	    ENDIF
200	    T2=T2+DT
	    NDIV=NDIV-1
	    PMAX1=PMAX1+PROB
	    PDMAX=MAX(PDMAX,PD)
	    IF(NDIV.GE.1) GOTO 100	
	    PMMBW=MAX(PMMBW,PMAX1)
	    IF(PMAX1.GT.PSTOPBW) GOTO 900
        ENDDO
      ENDDO
      IRTN=0
      GOTO 1000
C
 900  IRTN=1000
      WRITE(MSGFL,905) PMAX1,PDMAX,EP(0,N)
 905  FORMAT(' (SUBR.LSRQEDBW) Pstop for Breit-Wheeler exceeded limit.',/,
     %   '   Prob=',1PD10.3,'  for Power density=',1PD10.3,' W/m^2',
     %   'E(photon)=',1PD9.3,'eV')
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.LSRQEDBW) Too many new particles in one time step')
      GOTO 1000
 1000 CALL CPUTIM('LSRQEDBW',2)
      RETURN
      END
