      SUBROUTINE EVENDBC(TYP,N,N00,MP,IP,PL,BOP,ERR)
C  Compile version of EVENDB
C  end of one expression by
C  end-of-file or ) or comma.
      IMPLICIT NONE
      INTEGER TYP,N,N00,MP,IP,PL(3,MP),BOP(*)
      CHARACTER*(*) ERR
	INCLUDE 'include/evtypcod.h'
      IF(TYP.EQ.E_UNARY.OR.TYP.EQ.E_USERF.OR.TYP.EQ.E_BINARY
     %           .OR.TYP.EQ.E_ARRAY) THEN
        ERR='(EVENDBC) No argument following an operator.'
        RETURN
	ELSEIF(TYP.EQ.E_ARRAY) THEN
	  ERR='(EVENDBC) No subscript following an array.'
	  RETURN
      ENDIF
      IF(N.NE.0) THEN
        CALL EVREDCC(N,N00,MP,IP,PL,BOP,0,ERR)
        IF(ERR.NE.' ') RETURN
      ENDIF
      IF(N.NE.0) THEN
        ERR='(EVENDBC) Program error 2'
        RETURN
      ENDIF
      ERR=' '
      RETURN
      END