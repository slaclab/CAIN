      SUBROUTINE EVPCHK(IPAR)
      INTEGER IPAR
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
      INTEGER K
C
      IF(NKPAR.GE.1) THEN
        DO 200 K=1,NKPAR
          IF(KPAR(K).EQ.IPAR) GOTO 220
 200    CONTINUE
      ENDIF
      NKPAR=NKPAR+1
      KPAR(NKPAR)=IPAR
 220  CONTINUE
      RETURN
      END
