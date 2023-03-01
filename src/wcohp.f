C************************ WCOHP  **************************************
      FUNCTION WCOHP(CHI,L)
C  Total rate of coherent pair creation per unit time in external field
C   W=(alpha*m**2/(sqrt(3)*pi*omega))*Wcp(CHI)
C      alpha=fine structure constant
C      m=electron mass
C      omega=photon energy
C      CHI=(omega/m)*(B/Bcrit)
C      B=magnetic field,  Bcrit=4.4 GTesla.
C   WCP(CHI)=
C       integral dx (I+(x/(1-x)+(1-x)/x)*K(2/3,eta))  from x=0 to 1.
C     I= integral dy K(1/3,y) from y=eta to infinity.
C       eta=(2/3/CHI)/(x*(1-x))
C       K= modified Bessel function.
C
C   L must be 1 or 2.
C   WCOHP(CHI,1)=Wcp(CHI)
C   WCOHP(CHI,2)=EXP(8/(3*CHI))*Wcp(CHI)
C
C   Relative error < 0.1916E-4  for x< 8/3.
C   Relative error < 0.3784E-4  for x> 8/3.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 WCOHP,CHI
      REAL*8
     %  A1/ 2.06554353D+00/,A2/-3.61193072D+00/,A3/-5.94463856D+00/,
     %  A4/ 1.61047645D+01/,A5/-8.65914121D+00/,
     %  B1/ 1.24953689D+00/,B2/ 1.12019794D+00/,B3/ 8.38159552D-02/,
     %  B4/ 1.06746905D+00/,B5/ 1.48898063D-01/
      REAL*8 Y,X23
	INCLUDE 'include/ctrlcm.h'
C
      WCOHP=0
      IF(CHI.LT.0.OR.L.LE.0.OR.L.GE.3) GOTO 900
      Y=0.375D0*CHI
      IF(Y.GT.1) THEN
        X23=CHI**(0.666666666666666667D0)
        WCOHP=X23*(A1+A5/CHI**2)+A2+A3/X23+A4/CHI
        IF(L.EQ.2) WCOHP=WCOHP*EXP(1D0/Y)
      ELSEIF(CHI.EQ.0) THEN
        WCOHP=0
      ELSE
        WCOHP=CHI*(B1+CHI*(B2+CHI*B3))/(1E0+CHI*(B4+CHI*B5))
        IF(L.EQ.1) WCOHP=WCOHP*EXP(-1D0/Y)
      ENDIF
      RETURN
  900 WRITE(MSGFL,910)
  910 FORMAT(' (FUNC.WCOHP) Invalid argument.')
      RETURN
      END
