C******************** FVLIN1 ******************************************
      FUNCTION FVLIN1(F,IC,IR,CO,SI)
      IMPLICIT NONE
      INTEGER IC,IR
      REAL*8 F(IC),CO,SI,FVLIN1
      IF(IC.NE.1) GOTO 100
      FVLIN1=F(1)
      RETURN
  100 IF(IR.GE.3) GOTO 120
      FVLIN1=F(IR)
      RETURN
  120 IF(IR.EQ.4) GOTO 140
      FVLIN1=F(1)**2+F(2)**2
      RETURN
  140 FVLIN1=-F(1)*SI+F(2)*CO
      RETURN
      END
