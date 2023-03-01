      SUBROUTINE ERMSG1(ERR,N,TEXT)
      IMPLICIT NONE
      CHARACTER*(*) ERR,TEXT
      INTEGER N
      IF(N.EQ.0) THEN
        ERR=TEXT
        N=LEN(TEXT)
      ELSE
        ERR=ERR(1:N)//' '//TEXT
        N=N+1+LEN(TEXT)
      ENDIF
      RETURN
      END
