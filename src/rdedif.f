      SUBROUTINE RDEDIF(LN,LINE,NCHAR,IRTN)
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/ctrlcm.h'
C
      IF(LN(1,2).NE.0) GOTO 910
      IF(NESTLV.EQ.0) GOTO 900
      IF(NEST(NESTLV).NE.NEST_IF) GOTO 900
      NESTRT=0
      NESTLV=NESTLV-1
      IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905    FORMAT(' (SUBR.RDEDIF) IF/ENDIF nested incorrectly.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDEDIF) ENDIF accepts no operands. ',
     %   'Missing semicolon?')
      RETURN
      END
