      FUNCTION ASINH(X)
      IMPLICIT NONE
      REAL*8 ASINH,X,AX,X2
      AX=ABS(X)
      X2=AX*AX
      IF(AX.LE.0.0365D0) THEN
        ASINH=X*(1D0-X2*(0.1666666666666666667D0-X2*(0.075D0
     %  -X2*(0.044642857142857143D0-X2*0.030381944444444D0))))
      ELSE
        ASINH=LOG(AX+SQRT(1D0+X2))
        IF(X.LT.0) ASINH=-ASINH
      ENDIF
      RETURN
      END
