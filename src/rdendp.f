      SUBROUTINE RDENDP(LN,LINE,NCHAR,IRTN)
C  IRTN=0: return to PUSH
C       1: continue
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/pushcm.h'
      INTEGER IFIN
      REAL*8 T0,T1
C
      IF(LN(1,2).NE.0) GOTO 920
      IF(NESTLV.LE.0) GOTO 900
      IF(NEST(NESTLV).NE.NEST_PUSH) GOTO 910
      IF(TINI.GT.TFIN) GOTO 980
      IF(TINI.EQ.TFIN.OR.IT.GE.NT) THEN
        IFIN=1
      ELSE
        IFIN=0
      ENDIF
      IF(NT.EQ.0) THEN
        T0=TINI
        T1=TINI
      ELSE
        T0=TINI+(TFIN-TINI)/NT*IT
        IT=IT+1
        T1=TINI+(TFIN-TINI)/NT*IT
      ENDIF
C-----
      CALL ENDPSH(T0,T1,IFIN,IRTN)
      IF(IRTN.NE.0) GOTO 990
      IF(IFIN.NE.0) GOTO 800
C-----
      CALL EVDEFP('Time',T1,IRTN)
      TIMNOW=T1
C
      NESTRT=1
      IRTN=0
      RETURN
 800  NEST(NESTLV)=0
      NESTLV=NESTLV-1
      INPUSH=0
      IRTN=1
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.RDENDP) ENDPUSH entered ',
     %  'without PUSH command.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) LOPNAM(NEST(NESTLV))
 915  FORMAT(' (SUBR.RDENDP) ENDPUSH entered ',
     %  'with ',A,' nest unresolved.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDENDP) ENDPUSH accepts no operands. ',
     %   'Missing semicolon?')
      RETURN
 980  IRTN=1008
      WRITE(MSGFL,985)
 985  FORMAT(' (SUBR.RDENDP) Program error. ',
     %  'TINI>TFIN.  Should not come here.')
      RETURN
 990  RETURN
      END

