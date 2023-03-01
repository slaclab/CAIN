      SUBROUTINE ORTHVC(V1,V2,V3,IRTN)
C  Normalize V1
C  Find V2 by Schmidt orthogonalization and normalize.
C  V3= vector product V1*V2
C    IRTN=1:  V1=0
C         2:  V2=0 or V2 is parallel to V1
      IMPLICIT NONE
      INTEGER IRTN
      REAL*8 V1(3),V2(3),V3(3)
      INTEGER I
      REAL*8 V1SUM,V12SUM,V2SUM
      V1SUM=0
      V12SUM=0
      DO 200 I=1,3
        V1SUM=V1SUM+V1(I)**2
        V12SUM=V12SUM+V1(I)*V2(I)
 200  CONTINUE
      IF(V1SUM.EQ.0) THEN
        IRTN=1
        RETURN
      ENDIF
      V1SUM=1/SQRT(V1SUM)
      V12SUM=V12SUM*V1SUM
      V2SUM=0
      DO 220 I=1,3
        V1(I)=V1(I)*V1SUM
        V2(I)=V2(I)-V12SUM*V1(I)
        V2SUM=V2SUM+V2(I)**2
 220  CONTINUE
      IF(V2SUM.EQ.0) THEN
        IRTN=2
        RETURN
      ENDIF
      V2SUM=1/SQRT(V2SUM)
      DO 240 I=1,3
        V2(I)=V2(I)*V2SUM
 240  CONTINUE
      V3(1)=V1(2)*V2(3)-V1(3)*V2(2)
      V3(2)=V1(3)*V2(1)-V1(1)*V2(3)
      V3(3)=V1(1)*V2(2)-V1(2)*V2(1)
      IRTN=0
      RETURN
      END
