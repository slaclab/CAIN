C************************ CPRND1 ************************************
      SUBROUTINE CPRND1(CHI,X,IR)
C  Random number generator of coherent pair creation.
C  Generate exactly 1 pair for given value of CHI.
C  Input:
C   CHI    (k/m)*(B/Bc)
C             k=photon energy,  m=electron rest mass
C             B=magnetic field, Bc=critical field (=4.4 GTesla)
C   IR     random number seed.  (odd integer)
C  Output:
C   X      (positron energy)/k.   (0<X<1)
C          This variable is REAL*8 in order to avoid loss of
C          digits for electron energy when X is close to 1.
C
C  Accuracy of the formulas used.
C   Integral of K(1/3,x)    0.618E-4  (x<1.15),  1.257E-4 (x>1.15)
C   K(2/3,x)                0.246E-4  (x<1.15),  0.0868E-4 (x>1.15)
C   FCP (avr. of above two) 0.370E-4  (x<1.15),  0.477E-4  (x>1.15)
C   WCP(chi)                0.1916E-4 (chi<8/3), 0.3784E-4 (chi>8/3)
C
C  Computing time (average)  (Oct.31.1991, HITAC 380H)
C    14 microsec (CHI<1),  20 microsec (CHI=10)
C
      REAL*8 X,XE,P0,PDBLE
      REAL*4
     %  AW1/3.79597945E-01/,AW2/-6.63787258E-01/,AW3/-1.09248367E+00/,
     %  AW4/2.95967401E+00/,AW5/-1.59134492E+00/,
     %  BW1/2.29635264D-01/,BW2/ 2.05865831D-01/,BW3/ 1.54033860D-02/,
     %  BW4/1.06746905D+00/,BW5/ 1.48898063D-01/
      REAL*4 ETAC/1.15000000E0/
      REAL*4
     % AI1/ 1.81368731D+00/,AI2/-2.52901779D+00/,AI3/ 1.20103599D+00/,
     % AI4/-2.10027009D-01/,AI5/ 6.18331159D-02/,
     % BI1/ 1.25315654D+00/,BI2/ 1.56422865D+00/,BI3/ 1.80782691D+00/,
     % BI4/ 2.64690869D-01/,
     % AK1/ 1.07473773D+00/,AK2/-1.26307803D+00/,AK3/ 7.97483554D-01/,
     % AK4/-1.63916492D-01/,AK5/ 4.92410662D-02/,
     % BK1/ 1.25332502D+00/,BK2/ 8.42029277D-01/,BK3/ 5.75097624D-01/,
     % BK4/-1.62814785D-02/
      DATA CC0/0.367552597E0/
C             CC0=2/(SQRT(3)*PI)
      FMAX=1.23693E0
  200 IR=48828125*IR
      P0=DFLOAT(IR)*0.465661287307739D-9
      PDBLE=ABS(P0)
      P=PDBLE
      Q=1-PDBLE
      IR=48828125*IR
      R=ABS(FLOAT(IR))*0.46566129E-09
      ETA0=8E0/(3E0*CHI)
      IF(ETA0.LE.1) THEN
        CHI23=CHI**(0.666666667E0)
        WCOHP=EXP(ETA0)*(CHI23*(AW1+AW5/CHI**2)+AW2+AW3/CHI23+AW4/CHI)
      ELSE
        WCOHP=CHI*(BW1+CHI*(BW2+CHI*BW3))/(1E0+CHI*(BW4+CHI*BW5))
      ENDIF
      CNORM=CC0/WCOHP
      C1=0.5E0/SQRT(ETA0)
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
        IF(Q12.LE.1E-3) THEN
          DETA=Q12*(1E0+Q12*0.5E0)
        ELSE
          DETA=-LOG(1D0-DBLE(Q12))
        ENDIF
      ELSE
        DETA=-LOG(P1*(1+Q1))
      ENDIF
      ETA=ETA0+DETA
      DX1=SQRT(DETA/ETA)
      X=0.5E0*ETA0/(ETA*(1+DX1))
      XE=1D0-X
      DXDP=(1+C1*(1+Q**2))/D**2*(0.5E0*ETA0*Q1/DX1/ETA**2)
      IF(ETA.LE.ETAC) THEN
        ETA2=ETA**2
        ETA23=ETA2**(1D0/3D0)
        EXPETA=EXP(ETA)
        BKI13=EXPETA*(AI1+ETA23*(AI2+AI4*ETA2+ETA23*(AI3+AI5*ETA2)))
        BK23=EXPETA*((AK1+ETA2*(AK3+ETA2*AK5))/ETA23
     %                 +(AK2+AK4*ETA2)*ETA23)
      ELSE
        BKI13=(BI1+BI2/ETA)/(1E0+(BI3+BI4/ETA)/ETA)/SQRT(ETA)
        BK23=(BK1+BK2/ETA)/(1E0+(BK3+BK4/ETA)/ETA)/SQRT(ETA)
      ENDIF
      FCOHP=BKI13+SNGL(XE/X+X/XE)*BK23
      FCP=CNORM*FCOHP*DXDP/FMAX
      IF(R.GT.FCP) GOTO 200
      IF(P0.LT.0) X=XE
      RETURN
      END
