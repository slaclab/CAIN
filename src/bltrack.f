	SUBROUTINE TRACKMAG(L,ANG,K1,TEDGE,ROT,APERT,IQ0,P0,M0,E0,LOSSMON,
     %   LSPIN,NP,LSEL,KIND,LOST,PNAME,TXYS,EP,SPIN,IRTN)
C  Track many particles along one beamline element
C  Assume the particle coordinates are already in the beamline coordinate.
C  IQ0    Charge sign of the reference particle
C  P00    Reference momentum 
	IMPLICIT NONE
	INTEGER IQ0,KIN(3),NP,KIND(NP),LSEL(NP),LOST(NP),LSPIN,LOSSMON,
     %     IRTN
	REAL(8) L,ANG,K1,TEDGE(2),ROT,APERT(2),P0,M0,E0,
     %      TXYS(0:3,NP),EP(0:3,NP),SPIN(3,NP)
	CHARACTER(*) PNAME(NP)
	INTEGER N,IQ,I

	DO N=1,NP
	  IF(LSEL(N).EQ.0) CYCLE
	  IF(EP(3,N).LE.0) THEN
		  IF(LOSSMON.EQ.0) THEN
			  LOST(N)=1
	    ELSE
	      LOST(N)=2
	    ENDIF
	  ENDIF
	  IF(LOST(N).NE.0) CYCLE
	  IQ=IQ0
	  IF(KIND(N).EQ.2) IQ=-IQ0
	  CALL TRACKMAG1(L,ANG,K1,TEDGE,ROT,APERT,IQ,P0,M0,E0,LSPIN,
     %     TXYS(0,N),EP(0,N),SPIN(1,N),LOSSMON,IRTN)
	  IF(IRTN.GE.100) RETURN
	  IF(IRTN.NE.0) THEN
		  IF(LOSSMON.EQ.0) THEN
			  LOST(N)=1
	    ELSE
	      LOST(N)=2
	    ENDIF
	  ENDIF
	ENDDO
	IRTN=0
	RETURN
	END

	SUBROUTINE TRACKMAG1(L,ANG,K1,TEDGE,ROT,APERT,
     %    IQ,P00,M00,E00,LSPIN,TXYS,EP,SPIN,LOSSMON,IRTN)
C  Track one particle along one beamline element
C  IQ     Particle charge sign relative to the reference particle
C  P00    Reference momentum 
C  EP(3)  must be positive
C  When LOSSMON.ne.0, must give the location of loss.
C  IRTN   0         normal
C         1 to 99   particle lost
C         >=100     serious error
C  Followings are not ready yet
C    *  Combined function bend
C    *  Care of possible large off momentum
C        (very low energy, opposite charge)
C    *  Spin
C
	IMPLICIT NONE
	INTEGER IQ,LSPIN,LOSSMON,IRTN
	REAL(8) L,ANG,K1,TEDGE(2),ROT,APERT(2),
     %     P00,M00,E00,TXYS(0:3),EP(0:3),SPIN(3)
	INTEGER I,J
	REAL(8) C,S,X1,Y1,P1,DP,DP1,DP2,AXIS(3),TH,R(3,3),
     %    CH,SH,C1,XP,YP,QKL,RHO
	REAL(8) ANOM/0.001159652193D0/,EMASS/0.51099906D6/

	IRTN=0
	DO I=1,2
	  IF(APERT(I).NE.0) THEN
	    IF(ABS(TXYS(I)).GE.APERT(I)) IRTN=3
	  ENDIF
	ENDDO
	IF(IRTN.NE.0) RETURN
	IF(ROT.NE.0) THEN
	  C=COS(ROT)
	  S=SIN(ROT)
	  CALL BLXYROT(C,S,TXYS,EP,LSPIN,SPIN)
	ENDIF
	
	IF(K1.NE.0) THEN
	  IF(ANG.NE.0) GOTO 990
C                combined function not ready
	  CALL TRKQUAD(L,K1,IQ,P00,TXYS,EP,LSPIN,SPIN,IRTN)
	  IF(IRTN.NE.0) GOTO 600
	ELSEIF(ANG.NE.0) THEN
	  IF(L.EQ.0) GOTO 990
C                thin lens bend not allowed
        RHO=L/ANG
C      Leading edge
        IF(TEDGE(1).NE.0) THEN
	    CALL TRKQUAD(0D0,-TEDGE(1)/RHO,IQ,P00,TXYS,EP,LSPIN,SPIN,IRTN)
	    IF(IRTN.NE.0) GOTO 600
	  ENDIF
C      Sector-type body
	  CALL TRKBEND(L,ANG,IQ,P00,M00,E00,TXYS,EP,LSPIN,SPIN,IRTN)
	  IF(IRTN.NE.0) GOTO 600
C      Trailing edge
        IF(TEDGE(2).NE.0) THEN
	    CALL TRKQUAD(0D0,-TEDGE(2)/RHO,IQ,P00,TXYS,EP,LSPIN,SPIN,IRTN)
	    IF(IRTN.NE.0) GOTO 600
	  ENDIF
	ELSEIF(L.NE.0) THEN
C  Drift space
	  CALL TRKDRIFT(L,TXYS,EP,LOSSMON,APERT,IRTN)
	  IF(IRTN.NE.0) GOTO 600
	ENDIF
C             Must be rotated back even if the particle is lost
600	IF(ROT.NE.0) THEN
	  CALL BLXYROT(C,-S,TXYS,EP,LSPIN,SPIN)
	ENDIF
	IF(IRTN.NE.0) RETURN
	DO I=1,2
	  IF(APERT(I).NE.0) THEN
	    IF(ABS(TXYS(I)).GE.APERT(I)) IRTN=3
	  ENDIF
	ENDDO
	RETURN
990   IRTN=1000
	RETURN
	END

	SUBROUTINE BLXYROT(C,S,TXYS,EP,LSPIN,SPIN)
C  Coordinate axis rotation.  C=COS(ROT), S=SIN(ROT)
	IMPLICIT NONE
	INTEGER LSPIN
	REAL(8) C,S,TXYS(0:3),EP(0:3),SPIN(3)
	REAL(8) X1
	X1=TXYS(1)
	TXYS(1)=C*X1+S*TXYS(2)
	TXYS(2)=-S*X1+C*TXYS(2)
	X1=EP(1)
	EP(1)=C*X1+S*EP(2)
	EP(2)=-S*X1+C*EP(2)
	IF(LSPIN.NE.0) THEN
	  X1=SPIN(1)
	  SPIN(1)=C*X1+S*SPIN(2)
	  SPIN(2)=-S*X1+C*SPIN(2)
	ENDIF
	RETURN
	END

	SUBROUTINE TRKDRIFT(L,TXYS,EP,LOSSMON,APERT,IRTN)
C  Drift space
C  Assume no ROTATE (for loss position calculation)
	IMPLICIT NONE
	INTEGER LOSSMON,IRTN
	REAL(8) L,TXYS(0:3),EP(0:3),APERT(2)
	INTEGER I
      REAL(8) TXYS1(0:3),C,DS,DS1

	C=L/EP(3)
	TXYS1(0:2)=TXYS(0:2)+EP(0:2)*C
	TXYS1(3)=TXYS(3)+L
	IRTN=0
	IF(LOSSMON.NE.0) THEN
	  DS=L+1.0
	  DO I=1,2
	    IF(APERT(I).NE.0) THEN
	      DS1=-1
	      IF(TXYS1(I).GE.APERT(I)) THEN
	        DS1=(APERT(I)-TXYS(I))/EP(I)*EP(3)
	      ELSEIF(TXYS(I).LE.-APERT(I)) THEN
	        DS1=(-APERT(I)-TXYS(I))/EP(I)*EP(3)
	      ENDIF
	      IF(DS1.GE.0) DS=MIN(DS,DS1)
	    ENDIF
	  ENDDO
	  IF(DS.LE.L) THEN
	    C=DS/EP(3)
	    TXYS1(0:2)=TXYS(0:2)+EP(0:2)*C
	    TXYS1(3)=TXYS(3)+DS
	    IRTN=3
	  ENDIF
	ENDIF
	TXYS=TXYS1
	RETURN
	END

	SUBROUTINE TRKBEND(L,ANG,IQ,P00,M00,E00,TXYS,EP,LSPIN,SPIN,IRTN)
C  Track 1 particle in a sector bend (horizontal bend)
C  Linear approximation or exact solution (circle)
C  Assume L.ne.0 and ANG.ne.0 (not checked)
	IMPLICIT NONE
	INTEGER IQ,LSPIN,IRTN
	REAL(8) L,ANG,P00,M00,E00,TXYS(0:3),EP(0:3),SPIN(3)
	REAL(8) RHO,PABS,P1,DP,S,C,C1,XP,YP,X1,RHO1,F,F1,
     %   A,CA,SA,CA1,SA1,DT,DRHO,OMEGA,SPAXIS(3),SPROT,R(3,3),G1,TH
	REAL(8) ANOM/0.001159652193D0/,EMASS/0.51099906D6/
	LOGICAL LINEAR/.FALSE./

	IRTN=0
	RHO=L/ANG
C          Linear approximation if LIN.NE.0
	IF(LINEAR) THEN
        PABS=SQRT(EP(1)**2+EP(2)**2+EP(3)**2)
	  DP=(PABS-IQ*P00)/P00
	  S=SIN(ANG)
	  C1=2*SIN(ANG/2)**2
	  C=1-C1
	  X1=TXYS(1)
	  XP=EP(1)/EP(3)
	  YP=EP(2)/EP(3)
	  TXYS(1)=C*X1+RHO*S*XP+RHO*C1*DP
	  XP=C*XP+S*DP-S/RHO*X1
        IF(LSPIN.NE.0) THEN
	    G1=EP(0)/EMASS
	    SPAXIS(1:3)=-ANOM*(G1-1)*EP(2)/PABS**2*EP(1:3)
	    SPAXIS(2)=SPAXIS(2)+G1*ANOM+1
	    C1=SQRT(SPAXIS(1)**2+SPAXIS(2)**2+SPAXIS(2)**2)
	    IF(C1.NE.0) THEN
	      SPAXIS=SPAXIS/C1
	      SPROT=C1*IQ*ANG*P00/EP(3)
            CALL ROTMAT(SPROT,SPAXIS,1,3,1,3,R)
	      CALL MATVEC(R,3,3,SPIN)
	    ENDIF
        ENDIF
	  TXYS(2)=TXYS(2)+RHO*S*YP
	  TXYS(0)=TXYS(0)
     %    +(S*X1+RHO*C1*XP+RHO*(ANG-SIN(ANG))*DP+ANG*RHO)*EP(0)/PABS
	  EP(3)=PABS/SQRT(1+XP**2+YP**2)
	  EP(1)=XP*EP(3)
	  EP(2)=YP*EP(3)
	  TXYS(3)=TXYS(3)+L 
	ELSE
	  P1=SQRT(EP(1)**2+EP(3)**2)
	  RHO1=IQ*RHO*P1/P00
	  DRHO=RHO1-RHO
	  OMEGA=P1/(RHO1*EP(0))
	  C=COS(ANG)
	  S=SIN(ANG)
	  A=ATAN(EP(1)/EP(3))
	  CA=COS(A/2)
	  SA=SIN(A/2)
	  CA1=C*CA-S*SA
	  SA1=S*CA+C*SA
	  F=2*SA*CA1+(DRHO-TXYS(1))/RHO1*S
	  IF(ABS(F).GE.1) GOTO 900    !  cannot reach s=end of magnet
	  F1=SQRT(1-F**2)
	  DT=(ANG+A-ASIN(F))/OMEGA
	  IF(LSPIN.NE.0) THEN
          G1=EP(0)/EMASS
	    TH=IQ*P00/EP(0)*DT/RHO
	    C1=(G1-1)*EP(2)/(P1**2+EP(2)**2)
	    SPAXIS(1:3)=-C1*EP(1:3)
	    SPAXIS(2)=SPAXIS(2)+G1
	    C1=SQRT(SPAXIS(1)**2+SPAXIS(2)**2+SPAXIS(2)**2)
	    IF(C1.NE.0) THEN
	      SPAXIS=SPAXIS/C1
	      SPROT=TH*ANOM*C1
            CALL ROTMAT(SPROT,SPAXIS,1,3,1,3,R)
	      CALL MATVEC(R,3,3,SPIN)
	    ENDIF
	    TH=TH-ANG
	    IF(TH.NE.0) THEN
	      SPAXIS=0
	      SPAXIS(2)=1
            CALL ROTMAT(TH,SPAXIS,1,3,1,3,R)
	      CALL MATVEC(R,3,3,SPIN)
	    ENDIF
	  ENDIF
	  TXYS(0)=TXYS(0)+DT
	  TXYS(1)=TXYS(1)*C+DRHO*2*(SIN((A+ANG)/2))**2
     %     +RHO*2*SA*SA1-RHO1*F**2/(1+F1)
	  EP(1)=P1*F
	  EP(3)=P1*F1
	  TXYS(2)=TXYS(2)+EP(2)/EP(0)*DT
	  TXYS(3)=TXYS(3)+L
	ENDIF
	RETURN
900   IRTN=3
      RETURN
	END

	SUBROUTINE TRKQUAD(L,K1,IQ,P00,TXYS,EP,LSPIN,SPIN,IRTN)
C  Track 1 particle in quad
C  Accept L=0 (thin lens)
C  Linear approximation in (x,y,p_x,p_y)
	IMPLICIT NONE
	INTEGER IQ,LSPIN,IRTN
	REAL(8) L,K1,P00,TXYS(0:3),EP(0:3),SPIN(3)
	INTEGER I1,I2,NDIV,IDIV,I
	REAL(8) DP1,DP2,DP,P1,AXIS(3),TH,R(3,3),XP,YP,QKL,C,S,CH,SH,X1,Y1,
     %        X2,Y2,MIJ(3,2),MIJD(3,2),THD,CD,SD,CHD,SHD,XY1(2),XY2(2),
     %        XYP1(2),XYP2(2),SPAXIS(3),SPROT,C1,C2,C3,G1
	REAL(8) ANOM/0.001159652193D0/,EMASS/0.51099906D6/

	IF(L.EQ.0) THEN
	  DP1=-IQ*P00*K1*TXYS(1)
	  DP2=+IQ*P00*K1*TXYS(2)
	  EP(1)=EP(1)+DP1
	  EP(2)=EP(2)+DP2
	  P1=EP(3)**2-DP1*(2*EP(1)-DP1)-DP2*(2*EP(2)-DP2)
	  IF(P1.LE.0) GOTO 900
	  EP(3)=SQRT(P1)
	  IF(LSPIN.NE.0) THEN
	    DP=SQRT(DP1**2+DP2**2)
	    IF(DP.NE.0) THEN
	      AXIS(1)=DP2/DP
	      AXIS(2)=-DP1/DP
	      AXIS(3)=0
	      TH=(1+EP(0)/EMASS*ANOM)/EP(3)*DP
            CALL ROTMAT(TH,AXIS,1,3,1,3,R)
	      CALL MATVEC(R,3,3,SPIN)
	    ENDIF
	  ENDIF
	ELSE
C   Thick lens quad
        P1=SQRT(EP(1)**2+EP(2)**2+EP(3)**2)
	  XP=EP(1)/EP(3)
	  YP=EP(2)/EP(3)
	  QKL=K1*L*IQ*P00/P1
	  TH=SQRT(ABS(QKL))
	  IF(TH.GE.20) GOTO 900
	  C=COS(TH)
	  S=SIN(TH)
	  CH=COSH(TH)
	  SH=SINH(TH)
	  IF(QKL.GE.0) THEN
	    I1=1
	  ELSE
	    I1=2
	  ENDIF
	  I2=3-I1
	  MIJ(1,I1)=C
	  MIJ(2,I1)=L*S/TH
	  MIJ(3,I1)=-TH*S/L
	  MIJ(1,I2)=CH
	  MIJ(2,I2)=L*SH/TH
	  MIJ(3,I2)=TH*SH/L
	  X1=TXYS(1)
	  Y1=TXYS(2)
	  X2=MIJ(1,1)*X1+MIJ(2,1)*XP
	  XP=MIJ(3,1)*X1+MIJ(1,1)*XP
	  Y2=MIJ(1,2)*Y1+MIJ(2,2)*YP
	  YP=MIJ(3,2)*Y1+MIJ(1,2)*YP
	  IF(LSPIN.NE.0) THEN
	    G1=EP(0)/EMASS
	    SPROT=ABS(K1)*L*SQRT(MAX(X1**2,X2**2)+MAX(Y1**2,Y2**2))
     %          *(G1*ANOM+1)
	    NDIV=INT(SQRT(SPROT/0.0001D0))+1
	    IF(NDIV.EQ.1) THEN
	      MIJD=MIJ
	    ELSE
	      THD=TH/NDIV
	      CD=COS(THD)
	      SD=SIN(THD)
	      CHD=COSH(THD)
	      SHD=SINH(THD)
	      MIJD(1,I1)=CD
	      MIJD(2,I1)=L*SD/TH
	      MIJD(3,I1)=-TH*SD/L
	      MIJD(1,I2)=CHD
	      MIJD(2,I2)=L*SHD/TH
	      MIJD(3,I2)=TH*SHD/L
	    ENDIF
	    C1=IQ*P00/P1*K1/NDIV
	    C2=-C1*ANOM*(G1-1)
			C1=C1*(G1*ANOM+1)
	    XY1(1)=TXYS(1)
	    XY1(2)=TXYS(2)
	    XYP1(1)=EP(1)/EP(3)
	    XYP1(2)=EP(2)/EP(3)
	    DO IDIV=1,NDIV
	      DO I=1,2
	        XY2(I)=MIJD(1,I)*XY1(I)+MIJD(2,I)*XYP1(I)
	        XYP2(I)=MIJD(3,I)*XY1(I)+MIJD(1,I)*XYP1(I)
	      ENDDO
	      XY1=(XY1+XY2)/2
	      XYP1=(XYP1+XYP2)/2
	      C3=C2*(XY1(1)*XYP1(2)+XY1(2)*XYP1(1))
	      SPAXIS(1)=C1*XY1(2)+C3*XYP1(1)
	      SPAXIS(2)=C1*XY1(1)+C3*XYP1(2)
	      SPAXIS(3)=C3
	      SPROT=SQRT(SPAXIS(1)**2+SPAXIS(2)**2+SPAXIS(3)**2)
	      SPAXIS=SPAXIS/SPROT
            CALL ROTMAT(SPROT,SPAXIS,1,3,1,3,R)
	      CALL MATVEC(R,3,3,SPIN)
	      XY1=XY2
	      XYP1=XYP2
	    ENDDO
	  ENDIF
	  TXYS(1)=X2
	  TXYS(2)=Y2
	  EP(3)=P1/SQRT(1+XP**2+YP**2)
	  EP(1)=XP*EP(3)
	  EP(2)=YP*EP(3)
	  TXYS(0)=TXYS(0)+L*EP(0)/P1
	  TXYS(3)=TXYS(3)+L
	ENDIF
	IRTN=0
	RETURN
900   IRTN=3
	RETURN
	END
