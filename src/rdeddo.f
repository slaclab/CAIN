      SUBROUTINE RDEDDO(LN,LINE,NCHAR,IRTN)
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/ctrlcm.h'
C
      IF(LN(1,2).NE.0) GOTO 910
      IF(NESTLV.EQ.0) GOTO 900
      IF(NEST(NESTLV).NE.NEST_DO) GOTO 900
      IF(NESTRT.EQ.0) THEN
        NESTRT=1
        IRTN=0
      ELSE
	  IF(NESTST(NESTLV).EQ.1.OR.NESTST(NESTLV).EQ.2) THEN
C                                      CYCLE
C              (the case 1 is actually NESTRT=0)
	    NESTRT=1
	    IRTN=0
	  ELSE
C              3: EXIT,  4: out of DO range
          NESTRT=0
          NESTLV=NESTLV-1
          IRTN=1
	  ENDIF
      ENDIF
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.RDEDDO) DO/ENDDO nested incorrectly.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDEDDO) ENDDO accepts no operands. ',
     %   'Missing semicolon?')
      RETURN
      END
