      FUNCTION ATANH(X)
      IMPLICIT NONE
      REAL*8 ATANH,X,AX,X2
      AX=ABS(X)
      IF(AX.GE.1D0) THEN
        ATANH=0
C          (no error message)
      ELSE
        IF(AX.LE.0.032D0) THEN
          X2=AX*AX
          ATANH=X*(1D0-X2*(0.3333333333333333333D0-X2*(0.2D0
     %     -X2*(0.142857142857142857D0-X2*0.11111111111111D0))))
        ELSE
          ATANH=0.5D0*LOG((1D0+X)/(1D0-X))
        ENDIF
      ENDIF
      RETURN
      END
