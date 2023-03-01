      SUBROUTINE SPCOFF(TEXT,K,NC)
C  Take off space characters
C   K=1   take off only leading spaces
C   K=2            only trailing spaces
C   K=3   both
C   NC    resulting number of characters
C
      CHARACTER*(*) TEXT
      INTEGER K,NC
      INTEGER N,I,J
      IF(K.LE.0.OR.K.GE.4) RETURN
      NC=LEN(TEXT)
      N=NC
      IF(K.EQ.1.OR.K.EQ.3) THEN
        DO 200 I=1,N
          IF(TEXT(I:I).NE.' ') THEN
            IF(I.NE.1) THEN
              DO 180 J=I,N
                TEXT(J-I+1:J-I+1)=TEXT(J:J)
 180          CONTINUE
              NC=N-I+1
            ENDIF
            GOTO 220
          ENDIF
 200    CONTINUE
        NC=0
        RETURN
      ENDIF
 220  N=NC
      IF(K.EQ.2.OR.K.EQ.3) THEN
        DO 240 I=N,1,-1
          IF(TEXT(I:I).NE.' ') THEN
            NC=I
            RETURN
          ENDIF
 240    CONTINUE
        NC=0
        RETURN
      ENDIF
      RETURN
      END
