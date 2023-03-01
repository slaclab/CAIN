      SUBROUTINE EVENDB(TYP,N,X,BOP,ERR)
C  end of one expression by
C  end-of-file or ) or comma.
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER TYP,N,BOP(*)
      TYPE(FLCHTYPE) X(0:*)
      CHARACTER*(*) ERR
	INCLUDE 'include/evtypcod.h'
      IF(TYP.EQ.E_UNARY.OR.TYP.EQ.E_USERF.
     %                  OR.TYP.EQ.E_BINARY) THEN
        ERR='(EVENDB) No argument following an operator.'
        RETURN
	ELSEIF(TYP.EQ.E_ARRAY) THEN
	  ERR='(EVENDB) No subscript following an array.'
	  RETURN
      ENDIF
      IF(N.NE.0) THEN
        CALL EVREDC(N,X,BOP,0,ERR)
        IF(ERR.NE.' ') RETURN
      ENDIF
      IF(N.NE.0) THEN
        ERR='(EVENDB) Program error 2'
        RETURN
      ENDIF
      ERR=' '
      RETURN
      END
