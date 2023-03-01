	SUBROUTINE LSRGEO(LSR,TXYS,PD,PD0,EV,OMG,ISPINT)
C      SUBROUTINE LSRGEO(TXYS,TXYSLS,EV0,WLBAR,PD0,
C     %   LT,TPAR,LS,RL,GCUT,TDL,PD,EV,OMG,ISPINT,LRZ,TR0,TRL,TRLI)
C  Laser Geometry
C  Input
C   LSR        Laser ID
C   TXYS(0:3)  Field point where the laser parameters are to be
C              evaluated.
C   ISPINT     Flag for computing transverse polarization.
C              (Logitudinal polarization is irrelevant because
C               it is Lorentz invariant)
C  Output
C   PD         Power density in Watt/m**2
C   PD0        PD in laser coordinate (can be different from PD
C              only when Lorentz boosted)
C   EV(3,3)    Local unit vector. EV(*,3) is the direction of 
C              propagation and EV(*,1) and EV(*,2) is the new basis
C              vector for polarization. (The last two are arbitrary
C              vector perpendicular to EV(*,3) if ISPINT is off.)
C                (bug fix on Mar.26.2001)
C   OMG        Local wavenumber (1/m)
	USE LASRDATA
      IMPLICIT NONE
      INTEGER LSR,ISPINT,LRZ
      REAL*8 TXYS(0:3),EV(3,3),OMG,PD,PD0
      INCLUDE 'include/lasrcm.h'
	INCLUDE 'include/ctrlcm.h'
      INTEGER I,J,IRTN
      REAL*8 TXYS1(0:3),AA(2),XYS(3),UVW(0:3),V(3),NVEC2(3),
     %   EX,C00,SUM,S(3,3),TFAC,SFAC,T1
	LOGICAL VDEFINED
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
C
      PD0=0
      PD=0
C  Lorentz-transform the particle coordinate to the coordinate
C  when the laser was defined
C  (t,x,y,s)-frame to laser frame (tau,xi,eta,zeta)
	DO 140 I=0,3
	  UVW(I)=TXYS(I)
140   CONTINUE
      CALL LSRCOORD(LSR,UVW,0)

	VDEFINED=.FALSE.
											
	IF(LTPAR(LSR).EQ.DEFINED_BY_FILE.OR.LSPAR(LSR).EQ.DEFINED_BY_FILE
     %   .OR.LSPAR(LSR).EQ.DONUT_SHAPE) THEN
	  CALL LSRTAB(LSR,UVW,TFAC,SFAC,V,IRTN)
	  IF(IRTN.EQ.1) THEN
	    VDEFINED=.TRUE.
	  ELSEIF(IRTN.NE.0) THEN
	    RETURN
	  ENDIF
	ENDIF
C   Laser local intensity PD0 and local direction EV(*,3)
      IF(LTPAR(LSR).EQ.GAUSSIAN) THEN
        EX=(UVW(3)-UVW(0))/TPAR(1,LSR)
        IF(ABS(EX).GE.TPAR(2,LSR)) RETURN
        TFAC=EXP(-EX**2/2)
      ELSEIF(LTPAR(LSR).EQ.TRAPEZOIDAL) THEN
        T1=TPAR(1,LSR)/2D0-ABS(UVW(3)-UVW(0))
        IF(T1.LE.0) THEN
          RETURN
        ELSE
          TFAC=1
          IF(TPAR(2,LSR).GT.0) TFAC=MIN(1D0,T1/TPAR(2,LSR))
        ENDIF
	ELSEIF(LTPAR(LSR).EQ.DEFINED_BY_FILE) THEN

      ENDIF
	IF(LSPAR(LSR).EQ.GAUSSIAN) THEN
C         SFAC is already determined by LSRTAB in other two cases 
C         DONUT_SHAPE and DEFINED_BY_FILE
        EX=0
        DO 260 I=1,2
          AA(I)=TDL(I,LSR)*(1+(UVW(3)/SPAR(I,LSR))**2)
          EX=EX+UVW(I)**2/(WLLSR(LSR)*SPAR(I,LSR)*AA(I))
 260    CONTINUE
        IF(EX.GE.SPAR(5,LSR)**2/2) RETURN
        SFAC=EXP(-EX)
	  IF(TFAC.LE.1D-8) RETURN
        SFAC=SFAC*SQRT(TDL(1,LSR)*TDL(2,LSR)/(AA(1)*AA(2)))
C         Note that power density is defined with TDL already included
        V(1)=UVW(1)*UVW(3)/(SPAR(1,LSR)**2*AA(1))
        V(2)=UVW(2)*UVW(3)/(SPAR(2,LSR)**2*AA(2))
        V(3)=1/SQRT(1+V(1)**2+V(2)**2)
        V(1)=V(1)*V(3)
        V(2)=V(2)*V(3)
	  VDEFINED=.TRUE.
	ENDIF
	PD0=TFAC*SFAC*PLSR(LSR)
c	if(idbgflg(1).ne.0) then
c	write(msgfl,10) txys(0),tfac,sfac,pd0
c10    format('(LSRGEO)  T=',1pd12.3,' TFAC=',1pd10.3,'  SFAC=',d10.3,
c     %   ' PD0=',d10.3)
c	call resume
c	endif
	OMG=OMGLSR(LSR)
	IF(.NOT.VDEFINED) THEN
	  V(1)=0
	  V(2)=0
	  V(3)=1
	ENDIF
      DO 300 I=1,3
        EV(I,3)=0
        DO 280 J=1,3
          EV(I,3)=EV(I,3)+EVLSR(I,J,LSR)*V(J)
 280    CONTINUE
 300  CONTINUE
C   Polarization basis vector
      IF(ISPINT.GE.1) THEN
        C00=EV(1,3)*EVLSR(1,1,LSR)+EV(2,3)*EVLSR(2,1,LSR)
     %     +EV(3,3)*EVLSR(3,1,LSR)
        DO 310 I=1,3
          EV(I,1)=EVLSR(I,1,LSR)-C00*EV(I,3)
          SUM=SUM+EV(I,1)**2
 310    CONTINUE
        SUM=1/SQRT(SUM)
        DO 320 I=1,3
          EV(I,1)=SUM*EV(I,1)
 320    CONTINUE
        DO 330 I=1,3
          EV(I,2)=EV(I2(I),3)*EV(I3(I),1)-EV(I3(I),3)*EV(I2(I),1)
 330    CONTINUE
      ENDIF
C   Back to the current Lorentz frame
	PD=PD0
      IF(LTRLSR(LSR).GE.1) THEN
        C00=TRLSR(0,0,LSR)
        DO 340 I=1,3
          C00=C00+TRLSR(0,I,LSR)*EV(I,3)
          NVEC2(I)=EV(I,3)
 340    CONTINUE
        DO 360 I=1,3
          EV(I,3)=TRLSR(I,0,LSR)
          DO 350 J=1,3
            EV(I,3)=EV(I,3)+TRLSR(I,J,LSR)*NVEC2(J)
 350      CONTINUE
          EV(I,3)=EV(I,3)/C00
 360    CONTINUE
        OMG=OMG*C00
        PD=PD0*C00**2
        IF(ISPINT.GE.1) THEN
          DO 380 I=1,3
            DO 370 J=1,3
              S(I,J)=TRLSR(I,J,LSR)+TRLSR(I,0,LSR)*EVLSR(J,3,LSR)
     %                             -TRLSR(0,J,LSR)*EV(I,3)
 370        CONTINUE
 380      CONTINUE
          DO 400 J=1,2
            CALL MATVEC(S,3,3,EV(1,J))
 400      CONTINUE
        ENDIF
      ENDIF
	IF(ISPINT.LE.0) THEN
	  EV(1,1)=1
	  EV(2,1)=0
	  EV(3,1)=0
	  C00=EV(1,1)*EV(1,3)+EV(2,1)*EV(3,3)+EV(3,1)*EV(3,3)
	  SUM=0
	  DO 420 I=1,3
	    EV(I,1)=EV(I,1)-EV(I,3)*C00
	    SUM=SUM+EV(I,1)**2
420     CONTINUE
        SUM=1/SQRT(SUM)
        DO 440 I=1,3
	    EV(I,1)=EV(I,1)*SUM
440     CONTINUE
        DO 460 I=1,3
          EV(I,2)=EV(I2(I),3)*EV(I3(I),1)-EV(I3(I),3)*EV(I2(I),1)
460     CONTINUE
	ENDIF
      RETURN
      END
      

      
      
