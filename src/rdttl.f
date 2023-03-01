      SUBROUTINE RDTTL(TEXT,NN,TTL,NC,SEP)
      IMPLICIT NONE
      INTEGER NN,NC(NN)
      CHARACTER*(*) TEXT,TTL(NN),SEP
      INTEGER I,N,L,I0
      CHARACTER*1 C,SEP1
C
      SEP1=';'
      IF(SEP.NE.' ') SEP1=SEP
      L=LEN(TEXT)
      N=0
      I0=0
      DO 300 I=1,L+1
        C=SEP1
        IF(I.LE.L) C=TEXT(I:I)
        IF(C.EQ.SEP1) THEN
          N=N+1
          IF(I0.GE.1.AND.I.GT.I0) THEN
            TTL(N)=TEXT(I0:I-1)
            NC(N)=I-I0
          ELSE
            TTL(N)=' '
            NC(N)=0
          ENDIF
          IF(N.GE.NN) RETURN
          I0=0
        ELSE
          IF(I0.EQ.0) I0=I
        ENDIF
 300  CONTINUE
      DO 320 I=N+1,NN
        TTL(I)=' '
        NC(I)=0
 320  CONTINUE
      RETURN
      END

