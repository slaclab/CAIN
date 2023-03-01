      SUBROUTINE EVFCHK(IFUN)
      INTEGER IFUN
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
      INTEGER K
C
      IF(NKFUN.GE.1) THEN
        DO 200 K=1,NKFUN
          IF(KFUN(K).EQ.IFUN) RETURN
 200    CONTINUE
      ENDIF
      NKFUN=NKFUN+1
      KFUN(NKFUN)=IFUN
      RETURN
      END
