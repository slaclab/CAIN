      FUNCTION IDREL(TEXT)
      IMPLICIT NONE
      INTEGER IDREL
      CHARACTER*(*) TEXT
      INTEGER NC,N,I
      CHARACTER*2 CH
C
      NC=LEN(TEXT)
      IDREL=0
      IF(NC.LE.0) RETURN
      N=0
      CH=' '
      DO 200 I=1,NC
        IF(TEXT(I:I).NE.' ') THEN
          IF(N.GE.2) RETURN
          N=N+1
          CH(N:N)=TEXT(I:I)
        ENDIF
 200  CONTINUE
      IF(N.LE.0) RETURN
      IF(CH.EQ.'= ') THEN
        IDREL=1
      ELSEIF(CH.EQ.'<>'.OR.CH.EQ.'><') THEN
        IDREL=2
      ELSEIF(CH.EQ.'< ') THEN
        IDREL=3
      ELSEIF(CH.EQ.'> ') THEN
        IDREL=4
      ELSEIF(CH.EQ.'<='.OR.CH.EQ.'=<') THEN
        IDREL=5
      ELSEIF(CH.EQ.'>='.OR.CH.EQ.'=>') THEN
        IDREL=6
      ENDIF
      RETURN
      END



