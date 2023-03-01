      FUNCTION IFCMD(TEXT,M,CMD)
C  See if the string TEXT is one of CMD(i) (i=1,M).
C  TEXT need not be the full string of CMD. Can be
C  shortened if not ambiguous.
C   IFCMD=i  (1<=i<=M)  identified string number
C         0  no identifiable one
C        -1  ambiguous
      IMPLICIT NONE
      CHARACTER*(*) TEXT,CMD(*)
      INTEGER IFCMD,M,I,K,N
C      INTEGER LEN,MIN
C
      DO 200 I=1,M
        IF(TEXT.EQ.CMD(I)) THEN
          IFCMD=I
          RETURN
        ENDIF
 200  CONTINUE
      N=MIN(LEN(TEXT),LEN(CMD(1)))
      K=0
      DO 300 I=1,M
        IF(TEXT.EQ.CMD(I)(1:N)) THEN
          K=K+1
          IFCMD=I
        ENDIF
 300  CONTINUE
      IF(K.EQ.0) THEN
        IFCMD=0
      ELSEIF(K.NE.1) THEN
        IFCMD=-1
      ENDIF
      RETURN
      END
