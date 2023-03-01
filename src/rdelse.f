      SUBROUTINE RDELSE(IC,ICMD,NCMD,LN,LINE,NCHAR,IRTN)
      IMPLICIT NONE
      INTEGER IC,NCMD,ICMD(NCMD),LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/cmdnam.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/ctrlcm.h'
      INTEGER IC1
C
      IF(LN(1,2).NE.0) GOTO 910
      IF(NESTLV.LE.0) GOTO 900
      IF(NEST(NESTLV).NE.NEST_IF) GOTO 900
	IF(NESTST(NESTLV).GE.3) GOTO 900
	NESTST(NESTLV)=3
      IF(NESTRT.EQ.0) THEN
        CALL FNDEND(NEST_IF,IC,ICMD,NCMD,IC1,IRTN)
        IF(IRTN.NE.0) GOTO 900
        IC=IC1
      ELSE
        IC=IC+1
        NESTRT=0
      ENDIF
      IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.RDELSE) IF/ENDIF nested incorrectly.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDELSE) ELSE accepts no operands. ',
     %   'Missing semicolon?')
      RETURN
      END
