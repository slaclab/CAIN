      SUBROUTINE CPRND2(DT,EPH,CHI,XPE,ISPIN,EV,STOKES,SPINPE,
     %     WENH,AVRNP,LPH,LPAIR)
C  Random number generator of coherent pair creation.
C  Generate at most 1 pair for given value of CHI and the time
C  interval DT.
C  Input:
C   DT     time interval times velocity of light (meter)
C   EPH    photon energy (eV)
C   CHI    (k/m)*(B/Bc)
C             k=photon energy,  m=electron rest mass
C             B=magnetic field, Bc=critical field (=4.4 GTesla)
C   ISPIN  Flag for photon polarization
C   EV(3,3)  Orthonormal basis.
C          EV(*,3) unit vector along photon momentum
C          EV(*,1) unit vector along transverse acceleration
C                  for positron
C          EV(*,2)=EV(*,3) x EV(*,1)
C   STOKES(3)   Stokes parameter of the initial photon
C          based on EV.
C  Output:
C   XPE(j)  (j=1,2) Energy fraction of positron(j=1) and electron(j=2).
C           XPE(1)+XPE(2)=1.  Calculated when LPAIR=1.
C   SPINPE(3,2)  Spin vector of positron and electron.
C           Calculated when LPAIR=1 and ISPIN=1.
C   STOKES  Stokes parameter of the final photon. Calculated
C          when a LPH=1 and ISPIN=1.
C   AVRNP  average number of macro-particle pairs in DT.
C          Crude estimation.
C          Can be an over-estimation upto 10 percent.
C          If AVRNP is too large (say, >0.1),
C          the randomness of creation is lost.
C   LPH    0: No pair creation. No change of Stokes parameters.
C          1: Stokes parameters revised.
C          2: Photon eliminated.
C   LPAIR  0: No pair data stored.
C          1: One pair data stored.
C
C  Accuracy of the formulas used.
C   Integral of K(1/3,x)    0.618E-4  (x<1.15),  1.257E-4 (x>1.15)
C   K(2/3,x)                0.246E-4  (x<1.15),  0.0868E-4 (x>1.15)
C   FCP (avr. of above two) 0.370E-4  (x<1.15),  0.477E-4  (x>1.15)
C   WCP(chi)                0.1916E-4 (chi<8/3), 0.3784E-4 (chi>8/3)
C
C  Computing time (average)  (Nov.5.1991, HITAC 380H)
C    5.2 microsec * (number of calls)
C   + 12 microsec * (number of generated pairs)
C
      IMPLICIT NONE
      INTEGER ISPIN,LPH,LPAIR
      REAL*8 DT,EPH,CHI,XPE(2),EV(3,3),STOKES(3),SPINPE(3,2),
     %    WENH,AVRNP
      REAL*8 ETAC/1.15000000D0/
      REAL*8
     % AI1/ 1.81368731D+00/,AI2/-2.52901779D+00/,AI3/ 1.20103599D+00/,
     % AI4/-2.10027009D-01/,AI5/ 6.18331159D-02/,
     % BI1/ 1.25315654D+00/,BI2/ 1.56422865D+00/,BI3/ 1.80782691D+00/,
     % BI4/ 2.64690869D-01/,
     % AK1/ 1.07473773D+00/,AK2/-1.26307803D+00/,AK3/ 7.97483554D-01/,
     % AK4/-1.63916492D-01/,AK5/ 4.92410662D-02/,
     % BK1/ 1.25332502D+00/,BK2/ 8.42029277D-01/,BK3/ 5.75097624D-01/,
     % BK4/-1.62814785D-02/
      REAL*8 CC0/0.367552597D0/,CC1/82.5770D0/,CC2/18.28012D0/,
     %       CC/9.65838774D15/
C       CC0=2/(SQRT(3)*PI)
C       CC1=(16*SQRT(2)/SQRT(27))**3
C       CC2=(7*PI/5*1.5**(-2/3)/GAMMA(5/6)**2)**3
C       CC=(fine str.const.)*(electron mass)**2 in eV/m.
      REAL*8 FMAX/1.35286/
C       FMAX=7*SQRT(PI)/6/GAMMA(5/6)/GAMMA(2/3)
C
      INTEGER I,J
      REAL*8 ETA0,EXETA0,XI3,WCOHP0,WCOHP1,PP0,P0,P,Q,R,C1,D,Q1,
     %   P1,Q12,DETA,DX1,XP,XE,DXDP,ETA,ETA2,ETA23,EXPETA,FNKI13,FBK23,
     %   FBK13,FCOHP,FCP,A,X1,X2,A1,A2,A3
      REAL*8 RANDCAIN,WCPTOT,WCPXI3,BK13
	EXTERNAL RANDCAIN
C
      LPH=0
      LPAIR=0
	AVRNP=0    !   after David Asner  Sep.9.2001
      IF(CHI.LE.0.05) THEN
	  GOTO 500
	ENDIF
      ETA0=8D0/(3D0*CHI)
      EXETA0=EXP(-ETA0)
      WCOHP0=CHI/(CC1+CC2*CHI)**(1D0/3D0)
      XI3=0
      IF(ISPIN.GE.1) XI3=STOKES(3)
      IF(XI3.NE.0) WCOHP0=WCOHP0
     %               -XI3*CHI/(27*CC1+125*CC2*CHI)**(1D0/3D0)
      WCOHP1=EXETA0*WCOHP0
      AVRNP=CC*DT/EPH*WCOHP1
      IF(WENH.GT.1) AVRNP=AVRNP*WENH
      PP0=RANDCAIN()
      IF(PP0.GE.FMAX*AVRNP) GOTO 400
C
      P0=2*RANDCAIN()-1
      P=ABS(P0)
      Q=1-P
      R=RANDCAIN()
      C1=0.5D0/SQRT(ETA0)
      D=1+C1*P*(1+Q)
      IF(D.GE.2) THEN
        Q1=Q/D
        P1=1-Q1
      ELSE
        P1=P*(1+C1*(1+Q))/D
        Q1=1-P1
      ENDIF
      IF(Q1.LE.0.5) THEN
        Q12=Q1**2
        IF(Q12.LE.1D-3) THEN
          DETA=Q12*(1D0+Q12*0.5D0)
        ELSE
          DETA=-LOG(1D0-Q12)
        ENDIF
      ELSE
        DETA=-LOG(P1*(1+Q1))
      ENDIF
      ETA=ETA0+DETA
      DX1=SQRT(DETA/ETA)
      XP=0.5D0*ETA0/(ETA*(1+DX1))
      XE=1D0-XP
      DXDP=0.5*ETA0/DX1/ETA**2*Q1/(P1*(1+Q1)*D)*(1+2*C1*Q*Q1)
      IF(ETA.LE.ETAC) THEN
        ETA2=ETA**2
        ETA23=ETA2**(1D0/3D0)
        FNKI13=AI1+ETA23*(AI2+AI4*ETA2+ETA23*(AI3+AI5*ETA2))
        FBK23=(AK1+ETA2*(AK3+ETA2*AK5))/ETA23
     %                 +(AK2+AK4*ETA2)*ETA23
      ELSE
        EXPETA=EXP(-ETA)
C      Above line used to be EXPETA=EXP(-DETA)
C      Replaced on Mar.4.2002
        FNKI13=EXPETA*(BI1+BI2/ETA)/(1D0+(BI3+BI4/ETA)/ETA)/SQRT(ETA)
        FBK23=EXPETA*(BK1+BK2/ETA)/(1D0+(BK3+BK4/ETA)/ETA)/SQRT(ETA)
      ENDIF
      FCOHP=FNKI13+(XE/XP+XP/XE-XI3)*FBK23
      FCP=FCOHP*DXDP/FMAX*CC0/WCOHP1
C      The last factor in the above line used to be WCOHP0.
C      Replaced by WCOHP1 on Mar.4.2002
      IF(R.GE.FCP) GOTO 400
      LPAIR=1
      IF(WENH.LT.1.AND.R.GE.WENH*FCP) LPAIR=0
      LPH=2
      IF(WENH.GT.1.AND.R.GE.FCP/WENH) LPH=1
      IF(LPAIR.EQ.1) THEN
        IF(P0.GE.0) THEN
          XPE(1)=XP
          XPE(2)=XE
        ELSE
          XPE(1)=XE
          XPE(2)=XP
        ENDIF
        IF(ISPIN.GE.1) THEN
          FBK13=BK13(ETA,1)
          DO 280 J=1,2
            X1=XPE(3-J)
            X2=(XPE(J)-0.5D0)/(XPE(J)*X1)
            DO 260 I=1,3
              SPINPE(I,J)=FBK13*(EV(I,2)/XPE(J)
     %           +(STOKES(1)*EV(I,1)+EV(I,2)*STOKES(3))/X1)
     %           +X2*FBK23*STOKES(2)*EV(I,3)
              SPINPE(I,J)=SPINPE(I,J)/FCOHP
 260        CONTINUE
 280      CONTINUE
        ELSE
          DO 320 J=1,2
            DO 310 I=1,3
              SPINPE(I,J)=0
 310        CONTINUE
 320      CONTINUE
        ENDIF
      ENDIF
      IF(LPH.EQ.2) GOTO 500
C  Revise Stokes parameters
 400  IF(ISPIN.GE.1)  THEN
        A=CC0*CC*DT/EPH/2
        A1=1-A*WCPTOT(CHI,1)
        IF(XI3.EQ.0) THEN
          A2=0
        ELSE
          A2=-A*WCPXI3(CHI,1)
        ENDIF
        A3=A1-XI3*A2
        STOKES(1)=A1*STOKES(1)/A3
        STOKES(2)=A1*STOKES(2)/A3
        STOKES(3)=(A1*STOKES(3)-A2)/A3
        A=STOKES(1)**2+STOKES(2)**2+STOKES(3)**2
        IF(A.GT.1) THEN
          A=1/SQRT(A)
          STOKES(1)=A*STOKES(1)
          STOKES(2)=A*STOKES(2)
          STOKES(3)=A*STOKES(3)
        ENDIF
      ENDIF
      LPH=1
 500  RETURN
      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      FUNCTION WCPXI3(CHI,L)
C
C  WCPXI3(CHI,1) = Integral K(2/3,z/[t*(1-t]) dt  (over 0<t<1)
C                      z=2/(3*CHI)
C  WCPXI3(CHI,2) = (above expression) * exp(4*z)
C
C  L must be 1 or 2. Not checked.
C  Relative error < 2.9E-5 for all CHI.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 WCPXI3,CHI
      REAL*8 CHI0/1.8575D0/,
     %  A1/ 4.131361995D-01/,A2/-5.155073101D+00/,A3/ 7.725498378D+00/,
     %  A4/-2.186155468D+00/,A5/-1.388509433D+00/,A6/ 6.246156719D-01/,
     %  B1/ 4.165083118D-01/,B2/ 3.747380043D-01/,B3/ 1.236586321D+00/,
     %  B4/ 1.444517546D-01/,B5/-9.872234714D-03/
      REAL*8 X,X2,EX
C
      IF(CHI.GE.CHI0) THEN
        X=CHI**(-1D0/3D0)
        X2=X**2
        WCPXI3=A1/X2+X2*(A2+X*A3+X2*(A4+X2*(A5+X2*A6)))
        IF(L.EQ.2) WCPXI3=WCPXI3*EXP(8/(3*CHI))
      ELSE
        WCPXI3=CHI*(B1+CHI*B2)/(1+CHI*(B3+CHI*(B4+CHI*B5)))
        IF(L.EQ.1) THEN
          EX=8/(3*CHI)
          IF(EX.GE.100) THEN
            WCPXI3=0
          ELSE
            WCPXI3=WCPXI3*EXP(-EX)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      FUNCTION WCPTOT(CHI,L)
C  WCPTOT(CHI,1) = Integral Fcp(CHI,t) dt (over 0<t<1)
C       Fcp(CHI,t) = Integral K(1/3,y) dy (over z/[t(1-t)]<y<infty)
C                    + [t/(1-t)+(1-t)/t]*K(2/3,z/[t(1-t)])
C               z=2/(3*CHI)
C  WCPTOT(CHI,2) = (above expression) * exp(4*z)
C  L must be 1 or 2. Not checked.
C  Relative error < 0.8E-5 for all CHI.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 WCPTOT,CHI
      REAL*8 CHI0/4.174D0/,
     %  A1/ 2.065605366D+00/,A2/-3.623067738D+00/,A3/-5.444466484D+00/,
     %  A4/ 1.418088426D+01/,A5/-6.377070424D+00/,A6/-1.054917279D+00/,
     %  B1/ 1.249550950D+00/,B2/ 1.460238927D+00/,B3/ 2.215578228D-01/,
     %  B4/ 1.340047437D+00/,B5/ 3.031352305D-01/,B6/ 4.806054330D-03/
      REAL*8 Y,Y2
C
      IF(CHI.LE.CHI0) THEN
        WCPTOT=((B3*CHI+B2)*CHI+B1)*CHI/(((B6*CHI+B5)*CHI+B4)*CHI+1)
        IF(L.EQ.1) THEN
          IF(CHI.LE.0.01D0) THEN
            WCPTOT=0
          ELSE
            WCPTOT=WCPTOT*EXP(-8D0/(3D0*CHI))
          ENDIF
        ENDIF
      ELSE
        Y=1/CHI**(1D0/3D0)
        Y2=Y**2
        WCPTOT=(((A6*Y2+A5)*Y+A4)*Y+A3)*Y2+A2+A1/Y2
        IF(L.EQ.2) WCPTOT=WCPTOT*EXP(8D0/(3D0*CHI))
      ENDIF
      RETURN
      END
