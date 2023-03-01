	SUBROUTINE BLCOORD(LL,SSUM,TXYS0,EE,LSPIN,
     %   NP,LSEL,KIND,LOST,PNAME,TXYS,EP,SPIN)
C   LL    0:  Cain coord -> BL coord,  1:  BL coord -> Cain coord
C   SSUM     Orbit length from the beamline exit (LL=1 only)
C   TXYS0    Cain coordinate of the entrance(LL=0)/exit(LL=1) point
C            of the beamline.
C   EE(1:3,i)  x-, y-, s-axis of BL coordinate expressed in Cain coordinate
C         (Assume orthonormal)
C   Particles with LSEL(n)=0 are ignored.
	IMPLICIT NONE
	INTEGER LL,LSPIN,NP,LSEL(NP),KIND(NP),LOST(NP),IRTN
	REAL(8) SSUM,TXYS0(0:3),EE(3,3),TXYS(0:3,NP),
     %   EP(0:3,NP),SPIN(3,NP)
	CHARACTER(*) PNAME(NP)
	INTEGER I,N
	REAL(8) R(3,3),C1
	
	IF(LL.EQ.0) THEN
	  DO I=1,3
	    R(1:3,I)=EE(1:3,I)
	  ENDDO
	  DO N=1,NP
	    IF(LOST(N).NE.0) CYCLE
	    IF(LSEL(N).EQ.0) CYCLE
	    DO I=0,3
	      TXYS(I,N)=TXYS(I,N)-TXYS0(I)
	    ENDDO
	    CALL MATVEC(R,3,3,TXYS(1,N))
C            put all particles on the line s=0 in beamline coordinate
          C1=TXYS(3,N)/EP(3,N)
	    TXYS(0:2,N)=TXYS(0:2,N)-C1*EP(0:2,N)
	    TXYS(3,N)=0
	    CALL MATVEC(R,3,3,EP(1,N))
	    IF(LSPIN.NE.0) CALL MATVEC(R,3,3,SPIN(1,N))
	  ENDDO
	ELSE
	  DO I=1,3
	    R(I,1:3)=EE(1:3,I)
	  ENDDO
	  DO N=1,NP
	    IF(LOST(N).NE.0) CYCLE
	    IF(LSEL(N).EQ.0) CYCLE
	    TXYS(3,N)=TXYS(3,N)-SSUM
	    CALL MATVEC(R,3,3,TXYS(1,N))
	    CALL MATVEC(R,3,3,EP(1,N))
	    IF(LSPIN.NE.0) CALL MATVEC(R,3,3,SPIN(1,N))
	    DO I=0,3
	      TXYS(I,N)=TXYS(I,N)+TXYS0(I)
	    ENDDO
	  ENDDO
	ENDIF
	RETURN
	END

	SUBROUTINE TRKBLGEO(L,ANG,ROT,SSUM,XYS,EE)
C  Track geometry of beamline
C  Define 3-vector R=XYS. 
C      dR/ds = E3
C      dEj/ds = F (vector prod) Ej  (j=1,2,3)
C         In the bending magnet rotated by ROT,
C         (magnet rotation, not coordinate rotation)
C           F = -(E2*cos(ROT)-E1*sin(ROT))/rho
C   By defining 3-vectors
C       A = cos(ROT)*E1 + sin(ROT)*E2
C       B =-sin(ROT)*E1 + cos(ROT)*E2,
C        dA/ds = E3/rho, dB/ds=0, dE3/ds=-A/rho
C   Solution
C      A = A0*cos(s/rho) + E30*sin(s/rho)
C      E3=-A0*sin(s/rho) + E30*cos(s/rho)
C      R = R0 + rho*[ E30*sin(s/rho) - A0*(1-cos(s/rho))]
C
	IMPLICIT NONE
	REAL(8) L,ANG,ROT,SSUM,XYS(3),EE(3,3)
	INTEGER I
	REAL(8) EA(3),CROT,SROT,CANG,SANG,CANG1,RHO
	IF(ANG.EQ.0) THEN
	  IF(L.NE.0) THEN
          XYS=XYS+L*EE(1:3,3)
	    SSUM=SSUM+L
	  ENDIF
	  RETURN
	ENDIF
	IF(ROT.NE.0) THEN
	  CROT=COS(ROT)
	  SROT=SIN(ROT)
	  EA=EE(1:3,1)
	  EE(1:3,1)=CROT*EA+SROT*EE(1:3,2)
	  EE(1:3,2)=-SROT*EA+CROT*EE(1:3,2)
	ENDIF
	CANG1=2*SIN(ANG/2)**2
	CANG=1-CANG1
	SANG=SIN(ANG)
	RHO=L/ANG
	XYS=XYS+RHO*(SANG*EE(1:3,3)-CANG1*EE(1:3,1))
	EA=EE(1:3,1)
	EE(1:3,1)=CANG*EA+SANG*EE(1:3,3)
	EE(1:3,3)=-SANG*EA+CANG*EE(1:3,3)
	IF(ROT.NE.0) THEN
	  EA=EE(1:3,1)
	  EE(1:3,1)=CROT*EA-SROT*EE(1:3,2)
	  EE(1:3,2)=SROT*EA+CROT*EE(1:3,2)
	ENDIF
	SSUM=SSUM+L
	RETURN
	END
