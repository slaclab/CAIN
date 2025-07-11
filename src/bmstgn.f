      SUBROUTINE BMSTGN(EP,TXYS,FT,T1,PMAX,WENH,
     %    LEL,LEGEN,LPH,
     %     EPPH,TXYSPH,WGTPH,JSPIN,EV,SPIN,STOKES,UPSILN,PROB,IRTN,
     %           n,kindn,isbinn,fld)
C  Generate beamstrahlung photon.
C    Angle distribution is not computed.
C  Input:
C    EP     Electron energy-momentum (eV)
C    TXYS   Electron coordinate (m)
C    FT     Magnitude of the transverse component of Lorentz
C           force in eV/m
C    T1     End time
C    JSPIN  Flag for polarization
C    EV     EV(*,1)  unit vector along transverse acceleration
C                    (transverse component of Lorentz force)
C           EV(*,3)  unit vector along the 3-momentum of
C                    initial electron.
C           EV(*,2)  = EV(*,3) x EV(*,1)
C           EV is needed only when JSPIN is on.
C    SPIN   Initial electron polarization
C    PMAX   Maximum probability per one step.
C    WENH   Event rate enhancement factor.
C  Output:
C      There are several possibilities when WENH><1.
C      When WENH>1, it is possible that a photon is created and
C      stored, but the electron is treated as if no radiation.
C      When WENH<1, it is possible that a photon is radiated
C      but not stored.
C      It is also noted that the electron polarization changes
C      even when no photon is radiated.
C    LEL    0: No change of electron variables.
C           1: Electron variables revised.
C    LEGEN  0: No change of electron generation.
C           1: Electron generation increases by 1.
C    LPH    0: No radiation.
C           1: A photon created but not stored.
C           2: A photon created and stored.
C           The photon generation, if stored, is always
C           (initial electron generation)+1.
C    EPPH   Energy-momentum of the photon
C    TXYSPH Coordinate of the photon
C    STOKES Stokes parameter of the photon (when JSPIN>0)
C           with respect to the basis EV.
C    WGTPH  weight of the final photon (to be multiplied to the
C           initial electron weight.
C         Above four are calculated only when L=3.
C    EP,SPIN   Final electron variables. Revised when L>=1.
C    UPSILN   Upsilon parameter
C    PROB  Probability in the given time interval. (Crude estimation)
C          If this is too large, there is a statistical problem.
C    IRTN   Return code.
C           0: normal
C           100:  Probability exceeds PMAX but calculation done
C           200:  Probability exceeds 1. but calculation done
C           1000:  Probability exceeds 1. No calculation. (disabled 14nov2021)
      IMPLICIT NONE
      INTEGER LEL,LEGEN,LPH,JSPIN,IRTN,n,kindn,isbinn
      REAL*8 EP(0:3),TXYS(0:3),FT,T1,PMAX,WENH,
     %   EPPH(0:3),TXYSPH(0:3),WGTPH,EV(3,3),SPIN(3),STOKES(3),
     %   UPSILN,PROB,fld(3)
      include 'include/ctrlcm.h'
      include 'include/pushcm.h'
      REAL*8 CONST1/1.297210720417891D-02/,
     %       CONST2/9.67287708690521D0/,CONST3/0.01053281803D0/
C         CONST1=(Fine structure constant)*CONST2/[Sqrt(3)*Pi]
C         CONST2=9*2**(-1/3)*Gamma(2/3)
C         CONST3=5/(2*sqrt(3))*(Fine structure constant)
      REAL*8 EMASS/0.51099906D6/,CMPTNWL/3.86159323D-13/
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      INTEGER I
      REAL*8 FT1,DT,P1,P2,Y,Y3,Z,X,X1,XDEN,DXDY,PROB0,G(4),G00,G0,
     %   G1,S1,S2,S3,XI,T2,DT2,ABSP,FU0,FUSP,C0,SUM
      REAL*8 BKI13,FRADU0,FRADSP
      EXTERNAL BKI13,FRADU0,FRADSP
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      INTEGER MPMAX
      PARAMETER (MPMAX=10)
      INTEGER NPMAX/0/
      REAL*8 PMXDAT(3,MPMAX)
      SAVE NPMAX,PMXDAT
      integer n62/0/
      save n62
C
      IRTN=0
      LEL=0
      LEGEN=0
      LPH=0
      DT=T1-TXYS(0)
      IF(DT.LE.0) RETURN
      FT1=FT/EMASS
      UPSILN=CMPTNWL/EMASS*FT1*EP(0)
      XI=1.5D0*UPSILN
      FU0=(1+XI/2D0)**(1D0/3D0)
      PROB0=CONST1*DT*FT1/FU0
      if(mod(n62,2000000).eq.0.or.prob0.ge.1d0) then
          write(msgfl,101)
     &      n,kindn,it,isbinn,
     &          ep(3),ep(0), dt,sqrt(txys(1)**2+txys(2)**2),
     &         txys(1), txys(2), upsiln,
     &         ft1,fu0,fld(1:2),prob0
 101  format(' b2 n= ',i10,' kind(n)= ',i10,' it,is= ',i5,i3,
     &         ' pz= ',1pd10.3, ' En= ',1pd10.3, ' dt= ',1pd10.3,
     &         ' r= ', 1pd10.3,
     &         ' x= ', 1pd10.3, ' y= ', 1pd10.3,
     &         ' ups= ',1pd10.3,
     &         ' ft1= ',1pd10.3,' fu0= ',1pd10.3,
     &         ' fld= ',2(1pe11.3),
     &         ' prob0= ', 1pd10.3)
       endif
      n62=n62+1
      IF(WENH.GT.1) PROB0=PROB0*WENH
      PROB=0.80*PROB0
c     timb 14nov2021      IF(PROB0.GE.1D0) GOTO 900
      if(prob0.gt.1d0) then
         irtn=200
      elseif(prob.gt.pmax) then
         irtn=100
      endif
c      IF(PROB.GT.PMAX) IRTN=100
      P1=RANDCAIN()
      IF(P1.GE.PROB0) GOTO 300
      Y=RANDCAIN()
      Y3=Y**3
      IF(Y3.LE.0.OR.Y3.GE.1) GOTO 300
      XDEN=1-Y3+0.5D0*XI*(1+Y3**2)
      X=XI*Y3/XDEN
      X1=1-X
      Z=X/(XI*X1)
      CALL BMSTFN(Z,G,JSPIN)
      G00=G(1)+X**2/X1*G(2)
      IF(JSPIN.GE.1) THEN
        S2=SPIN(1)*EV(1,2)+SPIN(2)*EV(2,2)+SPIN(3)*EV(3,2)
        G0=G00-S2*X*G(3)
      ELSE
        G0=G00
      ENDIF
      DXDY=3*Y**2/XDEN*(XI+X*(1-XI*Y3))
      G1=G0*DXDY*FU0/(CONST2*UPSILN)
      IF(P1.GE.PROB0*G1) GOTO 300
C  Accept
      IF(WENH.EQ.1) THEN
        LEL=1
        LEGEN=1
        LPH=2
        WGTPH=1
      ELSE
        P2=RANDCAIN()
        IF(WENH.LT.1) THEN
          LEL=1
          LEGEN=1
          IF(P2.GE.WENH) THEN
            LPH=1
          ELSE
            LPH=2
            WGTPH=1/WENH
          ENDIF
        ELSE
          LPH=2
          WGTPH=1/WENH
          IF(P2.LT.1/WENH) THEN
            LEL=1
            LEGEN=1
          ENDIF
        ENDIF
      ENDIF
      ABSP=SQRT(EP(1)**2+EP(2)**2+EP(3)**2)
      IF(LPH.EQ.2) THEN
        T2=TXYS(0)+DT*0.5D0
C        T2 is the time when photon is created
        TXYSPH(0)=T2
        DT2=T2-TXYS(0)
        EPPH(0)=EP(0)*X
        DO 220 I=1,3
          EPPH(I)=EPPH(0)*EP(I)/ABSP
          TXYSPH(I)=TXYS(I)+DT2*EP(I)/EP(0)
C     Photon coordinate is the extrapolation
C     of electron trajectory before emission.
C     Change of electron TXYS not taken into account.
 220    CONTINUE
      ENDIF
      IF(JSPIN.GE.1) THEN
        S3=SPIN(1)*EV(1,3)+SPIN(2)*EV(2,3)+SPIN(3)*EV(3,3)
        IF(LPH.EQ.2) THEN
          S1=SPIN(1)*EV(1,1)+SPIN(2)*EV(2,1)+SPIN(3)*EV(3,1)
          STOKES(1)=X/(1-X)*G(3)*S1/G0
          STOKES(2)=-X*(2-X)/(1-X)*G(2)*S3/G0
          STOKES(3)=(G(2)-X/(1-X)*G(3)*S2)/G0
        ENDIF
        IF(LEL.EQ.1) THEN
          G(4)=BKI13(Z,1)
          DO 240 I=1,3
            SPIN(I)=(G00*SPIN(I)-X/(1-X)*G(3)*EV(I,2)
     %     -X**2/(1-X)*(S3*EV(I,3)*G(4)+(SPIN(I)-S3*EV(I,3))*G(2)))
     %      /G0
 240      CONTINUE
        ENDIF
      ENDIF
      IF(LEL.EQ.1) THEN
        DO 280 I=1,3
          EP(I)=EP(I)*(1-EP(0)*X/ABSP)
 280    CONTINUE
        EP(0)=SQRT(EMASS**2+EP(1)**2+EP(2)**2+EP(3)**2)
        GOTO 400
      ENDIF
C           When LEL=0, still go this way.
C  Rejected case
C     change of polarization vector for non-radiated case
 300  IF(JSPIN.NE.0) THEN
        S2=SPIN(1)*EV(1,2)+SPIN(2)*EV(2,2)+SPIN(3)*EV(3,2)
        FU0=1-CONST3*FT1*DT*FRADU0(UPSILN)
        FUSP=CONST3*FT1*DT*FRADSP(UPSILN)
        C0=FU0+FUSP*S2
        DO 320 I=1,3
          SPIN(I)=(SPIN(I)*FU0+FUSP*EV(I,2))/C0
 320    CONTINUE
        SUM=SPIN(1)**2+SPIN(2)**2+SPIN(3)**2
        IF(SUM.GT.1) THEN
          SUM=1/SQRT(SUM)
          DO 340 I=1,3
            SPIN(I)=SPIN(I)*SUM
 340      CONTINUE
        ENDIF
        LEL=1
      ENDIF
 400  RETURN
 900  IRTN=1000
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE BMSTFN(Z,G,ISPIN)
C  G(1)= integral K(5/3,x) dx  over Z < x < infty
C  G(2)= K(2/3,Z)
C  G(3)= K(1/3,Z)
      IMPLICIT NONE
      INTEGER ISPIN
      REAL*8 Z,G(3)
      REAL*8 Z1,Z2,Z3,C
      REAL*8 BK13
      EXTERNAL BK13
      IF(Z.LE.1.54D0) THEN
        Z1=Z**(2D0/3D0)
        Z2=Z1**2
        Z3=1/Z1
        G(1)=((0.03363782042D0*Z1-0.1134842702D0)*Z2
     %       +0.3944669710D0)*Z2-1.812672515D0
     %       +2.1495282415344901D0*Z3
        G(2)=(((0.04122878139D0*Z1-0.1494040962D0)*Z2
     %       +0.7862616059D0)*Z1-1.258219373D0)*Z1
     %       +1.074647298D0*Z3
      ELSEIF(Z.LE.4.48D0) THEN
        G(1)=((0.09120815010D0*Z-1.229105693D0)*Z+4.442223505D0)
     %      /(((Z-0.6903991322D0)*Z+5.651947051D0)*Z-0.9691386396D0)
        G(2)=((0.08194471311D0*Z-1.112728296D0)*Z+4.052334415D0)
     %      /(((Z-0.6524469236D0)*Z+6.1800441958D0)*Z-0.4915880600D0)
      ELSEIF(Z.LE.165D0) THEN
        C=EXP(-Z)/SQRT(Z)
        G(1)=C*(2.187014852D0+1.253535946D0*Z)/(0.9949036186D0+Z)
        G(2)=C*(0.6120387636D0+1.253322122D0*Z)/(0.3915531539D0+Z)
      ELSE
        G(1)=0
        G(2)=0
      ENDIF
      IF(ISPIN.GE.1) THEN
        IF(Z.LE.165D0) THEN
          G(3)=BK13(Z,1)
        ELSE
          G(3)=0
        ENDIF
      ENDIF
      RETURN
      END
C---------------------------- FRADU0 ---------------------------------
      FUNCTION FRADU0(UPS)
C  FRADU0 = (total radiation rate in quantum theory)/(that in classical)
C  as a function of Upsilon.
C   Accuracy:  Max.relative error < 5.05D-6  (for any Upsilon>=0)
      IMPLICIT NONE
      REAL*8 FRADU0,UPS
      REAL*8 X
      REAL*8 UPS0/1.3261D0/,
     % A1/ 8.0503959320D0/,A2/10.9756518968D0/,A3/ 1.4194297054D0/,
     % A4/ 8.9730711310D0/,A5/15.8341489137D0/,A6/ 4.1313700056D0/,
     % B1/ 1.0118692717D0/,B2/ 2.9309973207D0/,B3/ 1.6930111582D0/,
     % B4/ 2.8972660432D0/,B5/ 2.2315495296D0/,B6/ 1.7387035105D0/
C
      IF(UPS.LE.UPS0) THEN
        FRADU0=(((A3*UPS+A2)*UPS+A1)*UPS+1)/(((A6*UPS+A5)*UPS+A4)*UPS+1)
      ELSE
        X=1/UPS**(1D0/3D0)
        FRADU0=((B3*X+B2)*X+B1)*X/(((B6*X+B5)*X+B4)*X+1)
      ENDIF
      RETURN
      END
C----------------------------------------------------------------------
      FUNCTION FRADSP(UPS)
C  Integral of the spin-dependent term of radiation,
C  normalized by the classical total rate of radiation.
C                 2
C   FRADSP(Y)= ------ * Integral x*dx*K(1/3,(2/3/Y)*x/(1-x)) 
C              5*Pi*Y                      (over 0<x<1)
C  Relative error:  < 0.936E-5 for any Y.
C
      IMPLICIT NONE
      REAL*8 FRADSP,UPS,X
      REAL*8 UPS0/1.06237D0/,A1/2.99997194D-01/,
     %  A2/2.62120367D+00/,A3/3.86895664D-01/,A4/1.56610660D+01/,
     %  A5/5.76471880D+01/,A6/2.64479482D+01/,A7/5.43635258D-01/,
     %  B1/9.91434629D-02/,B2/4.92000917D-04/,B3/1.45454865D+00/,
     %  B4/1.78219471D-03/,B5/3.90751942D-01/,B6/1.88294532D-01/
C
      IF(UPS.LE.UPS0) THEN
        FRADSP=UPS*(A1+UPS*(A2+UPS*A3))
     %             /(1+UPS*(A4+UPS*(A5+UPS*(A6+UPS*A7))))
      ELSE
        X=1/UPS**(1D0/3D0)
        FRADSP=X**2*B1/(1+X*(B2+X*(B3+X*(B4+X*(B5+X*B6)))))
      ENDIF
      RETURN
      END


