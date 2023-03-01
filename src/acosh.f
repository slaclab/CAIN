      FUNCTION ACOSH(X)
      IMPLICIT NONE
      REAL*8 ACOSH,X,X1,X2
      IF(X.LE.1D0) THEN
        ACOSH=0D0
C         (no error message for x<1)
      ELSE
        X2=(X-1D0)*(X+1D0)
        X1=SQRT(X2)
        IF(X1.LE.0.0365D0) THEN
          ACOSH=X1*(1D0-X2*(0.1666666666666666667D0-X2*(0.075D0
     %    -X2*(0.044642857142857143D0-X2*0.030381944444444D0))))
        ELSE
          ACOSH=LOG(X+X1)
        ENDIF
      ENDIF
      RETURN
      END
