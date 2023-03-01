!       Begin Change by Li Dongguo March.11.2003 
      SUBROUTINE NLCPGN0(PE1,WL,EV,HE1,STOKESL,PD,DT,PMAX,ISPIN1,
     %    NPH,PE2,HE2,PG,Stokes2,PROB,WGT,IRTN)
!       End Change by Li Dongguo March.11.2003 
!    Old version before change by Li Dongguo March.11.2003
!      SUBROUTINE NLCPGN0(PE1,WL,NL,HE1,HL,PD,DT,PMAX,ISPIN1,
!     %    NPH,PE2,HE2,PG,HG,PROB,WGT,IRTN)

C      Name changed NLCPGN --> NLCPGN0 (Jun.24.2011)

C Input
C   PE1(0:3)  Initial electron 4-momentum (eV/c)
C   WL      Laser photon energy (eV)
C   NL(3)   Unit vector along the laser direction
C   HE1     Electron helicity (-1<=HE1<=1)
C           Note that, if ISPIN1 is on, electron helicity will
C           change even when NO radiation. This new value of helicity
C           will be stored in HE1 on return. Also, when radiated
C           with WGT<1, the portion (1-WGT) of particles still
C           remain unscattered with the initial energy-momentum
C           but revised helicity.
C   HL      Laser helicity
C   PD      Laser power density (Watt/m**2)
C   DT      Time interval times the velocity of light (meter)
C   PMAX    Maximum probability of radiation in one time step.
C           If the probability turns out to be >=PMAX,
C           return with no radiation and IRTN=100.
C           PMAX smaller than 0.1 is recommended.
C   ISPIN1   Flag to take into account polarization effects.
C           If ISPIN1 is zero, initial electron polarization is ignored
C           and the final polarization HE2 and HG are not calculated.
C           (Laser must anyway be circularly polarized.)
C Output
C   NPH     Number of absorbed laser photon (no radiation if NPH=0)
C   PE2(0:3)  Final electron 4-momentum (eV/c)
C   HE2     Final electron helicity
C   PG(0:3)   Final photon 4-momentum (eV/c)
C   HG      Final photon helicity
c   Stokes2(3) stokes parameter of final photons      ! added by Li Dongguo March.12.2003
C   PROB    Calculated event probability in the given time interval.
C   WGT     Event weight. 0<=WGT<=1.
C   IRTN    Return code
C    
      IMPLICIT NONE
      INTEGER ISPIN1,NPH,IRTN

!      Begin Change, March.7.2003(by Li Dongguo)
	 real(8) PE1(0:3),WL,EV(3,3),HE1,STOKESL(3),PD,Xi2,DT,PMAX
	 real(8) PE2(0:3),PG(0:3),Stokes2(3),Prob,WGT
	 real(8) HL,NL(3),HG,HE2
      INTEGER I
      REAL*8 XI,LAM,EEFF,E1,XG,PHI,PNL,V(3,3),CPHI,SPHI,SUM,
     %   FACKL,FACKL2,PT
!      End Change, March.7.2003(by Li Dongguo)


      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      REAL*8 EMASS/0.51099906D6/,CVEL/2.99292458D8/,
     %   HBARC/1.9732705D-7/,PI/3.141592653589793238D0/
C        (hbar*c in eV*meter)
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlcpcm.h'
	INCLUDE 'include/ctrlcm.h'
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN

      IF(IREADY(1).NE.IREADY0) GOTO 900

      E1=PE1(0)
 !      Begin Add, March.7.2003(by Li Dongguo)      
	  NL=EV(1:3,3);        ! old version no this term
 !      End Add, March.7.2003(by Li Dongguo)      
      PNL=E1-NL(1)*PE1(1)-NL(2)*PE1(2)-NL(3)*PE1(3)
      LAM=2*WL*PNL/EMASS**2
      XI=HBARC/(EMASS*WL)*SQRT(CVEL*PI*4D-7*PD)
      IF(LAM.LE.0) THEN
        NPH=0
        IRTN=0
        RETURN
      ENDIF
      XI2=XI**2

	IF(LSRQEDPOL(1).EQ.2) THEN
!      Begin Change, March.7.2003(by Li Dongguo)

        call NLCPLGN(PE1,WL,EV,HE1,STOKESL,Xi2,DT,PMAX,
     %	NPh,PE2,PG,Stokes2,Prob,WGT,ISPIN1,IRTN) 
          HE2=0 !  electron spin not computed

!	  if(NPh.GE.1)
!     %    print *,'KY Stokes2(1)=',Stokes2(1), 'Stokes2(3)=',Stokes2(3)
!      End Change, March.7.2003(by Li Dongguo)
!      old version before change(following two sentences)
!	  CALL NLCPLGN(PE1,WL,NL,HE1,HL,PD,DT,PMAX,ISPIN1,
!     %    NPH,PE2,HE2,PG,HG,PROB,WGT,IRTN)
	  
	  RETURN
	ENDIF

 !      Begin Add, March.12.2003(by Li Dongguo)      
	HL=STOKESL(2)
 !      End Add, March.12.2003(by Li Dongguo)      
      EEFF=E1+XI2/LAM*WL
      CALL NLCPGN00(XI,LAM,DT,EEFF,HL,HE1,PMAX,ISPIN1,
     %     NPH,XG,HE2,HG,
     %     PROB,WGT,IRTN,MPH,MY,MXI,MLM,XISQMX,LMMAX,FALLMX,
     %     R8(IPFF),R8(IPFINT),R8(IPFALL),R8(IPBES),WGTCP)
		if(irtn.eq.1002) then
	       print *,' PNL/2=',pnl/2
	       print *,' WL=',wl
	       print *,' abs(n)=',sqrt(nl(1)**2+nl(2)**2+nl(3)**2)
	       print *,' PE=',sqrt(pe1(1)**2+pe1(2)**2+pe1(3)**2)
	       print *,' E1=',e1
	    endif
	IF(IRTN.NE.0) RETURN
      IF(NPH.EQ.0) RETURN
      PHI=2*PI*RANDCAIN()
      CPHI=COS(PHI)
      SPHI=SIN(PHI)
      SUM=0
      DO 200 I=1,3
        V(I,2)=PE1(I2(I))*NL(I3(I))-PE1(I3(I))*NL(I2(I))
        SUM=SUM+V(I,2)**2
 200  CONTINUE
      IF(SUM.EQ.0) THEN
        V(1,2)=1-NL(1)**2
        V(2,2)=-NL(1)*NL(2)
        V(3,2)=-NL(1)*NL(3)
        SUM=1-NL(1)**2
      ENDIF
      SUM=1/SQRT(SUM)
      DO 240 I=1,3
        V(I,2)=SUM*V(I,2)
 240  CONTINUE
      DO 260 I=1,3
        V(I,3)=(E1*NL(I)-PE1(I))/PNL
 260  CONTINUE
      DO 280 I=1,3
        V(I,1)=V(I2(I),2)*V(I3(I),3)-V(I2(I),3)*V(I3(I),2)
 280  CONTINUE
      PT=EMASS*XG*SQRT(NPH*LAM*(1-XG)/XG-(1+XI2))
      FACKL=WL*(NPH*(1-XG)-(2+XI2)*XG/LAM)
      FACKL2=WL*(NPH-XI2/LAM*XG/(1-XG))
      DO 300 I=1,3
        PG(I)=XG*PE1(I)+FACKL*NL(I)+PT*(V(I,1)*CPHI+V(I,2)*SPHI)
        PE2(I)=PE1(I)+FACKL2*NL(I)-PG(I)
 300  CONTINUE
      PG(0)=SQRT(PG(1)**2+PG(2)**2+PG(3)**2)
      PE2(0)=SQRT(PE2(1)**2+PE2(2)**2+PE2(3)**2+EMASS**2)

!      Begin Change, March.12.2003(by Li Dongguo)
	Stokes2(1)=0
	Stokes2(2)=HG
	Stokes2(3)=0
!      Begin Change, March.12.2003(by Li Dongguo)

      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.NLCPGN0) SUBR.NLCPST has not yet been called.')
      RETURN
      END

