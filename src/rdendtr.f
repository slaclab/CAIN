      SUBROUTINE RDENDTRANS(LN,LINE,NCHAR,IRTN)
C  IRTN=0: return to TRANSPORT
C       1: continue
	USE BEAMCM
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/pushcm.h'
      INTEGER IFIN
C
      IF(LN(1,2).NE.0) GOTO 920
      IF(NESTLV.LE.0) GOTO 900
      IF(NEST(NESTLV).NE.NEST_TRANS) GOTO 910
      IF(TINI.EQ.TFIN.OR.IT.GE.NT) THEN
        IFIN=1
      ELSE
        IFIN=0
      ENDIF
C-----
	CALL ENDTRANSP(NP,ISBIN,KIND,LOST,PNAME,TXYS,EP,SPIN,IRTN)
	IF(IRTN.EQ.0.OR.IRTN.EQ.1) THEN
	  IFIN=IRTN
      ELSE
	  GOTO 990
	ENDIF
      IF(IFIN.NE.0) GOTO 800
C-----
      NESTRT=1
      IRTN=0
      RETURN
 800  NEST(NESTLV)=0
      NESTLV=NESTLV-1
      INPUSH=0
      IRTN=1
      RETURN
C--------
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.RDENDTRANS) ENDTRANSPORT entered ',
     %  'without TRANSPORT command.')
      GOTO 990
 910  IRTN=1001
      WRITE(MSGFL,915) LOPNAM(NEST(NESTLV))
 915  FORMAT(' (SUBR.RDENDTRANS) ENDTRANSPORT entered ',
     %  'with ',A,' nest unresolved.')
      GOTO 990
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDENDTRANS) ENDTRANSPORT accepts no operands. ',
     %   'Missing semicolon?')
      GOTO 990

 990  RETURN
      END
