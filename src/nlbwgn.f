      SUBROUTINE NLBWGN(PG,HG,WL,NL,HL,PD,DT,PMAX,ISPIN1,
     %   NPH,PELE,HELE,PPOS,HPOS,PROB,WGT,IRTN)
C Input
C    PG(0:3)  Initial (high energy) photon 4-momentum (eV/c)
C    HG     Photon helicity (-1<=HG<=1)
C    WL     Laser photon energy (eV)
C    NL(3)  Unit vector along laser direction
C    HL     Laser helicity  (HL=+1.0 or -1.0)
C    PD     Laser power density (Watt/m**2)
C    DT     Time interval times velocity of light (meter)
C    PMAX:  Maximum probability of pair creation in one time step.
C           If the probability turns out to be >=PMAX,
C           return with no radiation and IRTN=100.
C           PMAX smaller than 0.1 is recommended.
C    ISPIN1 Flag to compute final polarization HELE and HPOS
C Output
C    NPH    Number of absorbed laser photon (no pair creation if NPH=0)
C    PELE(0:3)   Final electron 4-momentum (eV/c)
C    HELE   Final electron helicity
C    PPOS(0:3)   Final positron 4-momentum (eV/c)
C    HPOS   Final positron helicity
C    PROB    Calculated event probability in the given time interval.
C    WGT    Weight factor of the event
C    IRTN   Return code
C
      IMPLICIT NONE
      INTEGER ISPIN1,NPH,IRTN
      REAL*8 PG(0:3),HG,WL,NL(3),HL,PD,DT,PMAX,
     %   PELE(0:3),HELE,PPOS(0:3),HPOS,PROB,WGT
      INTEGER I
      REAL*8 WG,XISQ1,ETA,Q,PNL,XELE,XPOS,PHI,
     %   CPHI,SPHI,V(3,2),VTEMP(3),SUM,PT,FACKLE,FACKLP
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      REAL*8 EMASS/0.51099906D6/,CVEL/2.99292458D8/,
     %   HBARC/1.9732705D-7/,PI/3.14159 26535 89793 238D0/
C        (hbar*c in eV*meter)
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlbwcm.h'
	INCLUDE 'include/ctrlcm.h'
C
      IF(IREADY(2).NE.IREADY0) GOTO 900
      WG=PG(0)
      XISQ1=(HBARC/(EMASS*WL))**2*(CVEL*PI*4D-7*PD)
      PNL=PG(0)-PG(1)*NL(1)-PG(2)*NL(2)-PG(3)*NL(3)
      ETA=0.5D0*WL*PNL/EMASS**2
      Q=ETA/(1+XISQ1)
      CALL NLBWGN0(XISQ1,Q,DT,WG,HL,HG,PMAX,ISPIN1,
     %     NPH,XELE,HELE,HPOS,PROB,WGT,IRTN,
     %     MPH,MY,MXI,MQ,XISQMX,QMIN,QMAX,
     %     QQ,LN0,NL0,R8(IPGG),R8(IPGINT),R8(IPGALL),GALLMX,WGTBW)
      IF(NPH.EQ.0.OR.IRTN.NE.0) RETURN
      PHI=2*PI*RANDCAIN()
      CPHI=COS(PHI)
      SPHI=SIN(PHI)
      SUM=0
      DO 200 I=1,3
        V(I,2)=PG(I2(I))*NL(I3(I))-PG(I3(I))*NL(I2(I))
        SUM=SUM+V(I,2)**2
 200  CONTINUE
      IF(SUM.EQ.0) THEN
        V(1,2)=1-NL(1)**2
        V(2,2)=-NL(1)*NL(2)
        V(3,2)=-NL(1)*NL(3)
        SUM=1-NL(1)**2
      ENDIF
      SUM=1/SQRT(SUM)
      DO 220 I=1,3
        V(I,2)=SUM*V(I,2)
 220  CONTINUE
      DO 240 I=1,3
        VTEMP(I)=(PG(0)*NL(I)-PG(I))/PNL
 240  CONTINUE
      DO 260 I=1,3
        V(I,1)=V(I2(I),2)*VTEMP(I3(I))-V(I3(I),2)*VTEMP(I2(I))
 260  CONTINUE
      XPOS=1-XELE
      PT=EMASS*SQRT(4*NPH*ETA*XELE*XPOS-(1+XISQ1))
      DO 280 I=1,3
        VTEMP(I)=PT*(V(I,1)*CPHI+V(I,2)*SPHI)
 280  CONTINUE
      FACKLE=WL*(NPH*XPOS-4*XISQ1/(ETA*XELE))
      FACKLP=WL*(NPH*XELE-4*XISQ1/(ETA*XPOS))
      DO 300 I=1,3
        PELE(I)=XELE*PG(I)+FACKLE*NL(I)+VTEMP(I)
        PPOS(I)=XPOS*PG(I)+FACKLP*NL(I)-VTEMP(I)
 300  CONTINUE
      PELE(0)=SQRT(PELE(1)**2+PELE(2)**2+PELE(3)**2+EMASS**2)
      PPOS(0)=SQRT(PPOS(1)**2+PPOS(2)**2+PPOS(3)**2+EMASS**2)
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.NLBWGN) SUBR.NLBWST has not yet been called.')
      RETURN
      END

