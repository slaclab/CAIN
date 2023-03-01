      SUBROUTINE RDCYCEXT(KK,IC,ICMD,NCMD,LN,LINE,NCHAR,IRTN)
C       KK=1: CYCLE, =2: EXIT
      IMPLICIT NONE
      INTEGER KK,IC,NCMD,ICMD(NCMD),LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/cmdnam.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/ctrlcm.h'
	CHARACTER(5) CMD1
      INTEGER IC1,LVL0,K1
C
      CMD1='CYCLE'
      IF(KK.EQ.2) CMD1='EXIT'
      IF(LN(1,2).NE.0) GOTO 910
      IF(NESTLV.LE.0) GOTO 900
      IRTN=0
      LVL0=NESTLV
C  Look for 'ENDDO' at the current nest level or lower.
      DO 300 IC1=IC+1,NCMD
        DO 200 K1=1,MNEST_TYPE
          IF(CMD(ICMD(IC1)).EQ.LOPNAM(K1)) THEN
            IF(NESTLV.EQ.MNEST) GOTO 900
            NESTLV=NESTLV+1
            NEST(NESTLV)=K1
            GOTO 300
          ENDIF
 200    CONTINUE
        DO 240 K1=1,MNEST_TYPE
          IF(CMD(ICMD(IC1)).EQ.ENDNAM(K1)) THEN
	      IF(NEST(NESTLV).NE.K1) GOTO 940
            IF(NESTLV.LE.LVL0) THEN
              IF(K1.EQ.NEST_DO) THEN
	          IF(KK.EQ.1) THEN
                  NESTST(NESTLV)=2
	          ELSE
	            NESTST(NESTLV)=3
	          ENDIF
	          IC=IC1
	          NESTRT=1
	          RETURN
	        ELSEIF(K1.EQ.NEST_PUSH.OR.K1.EQ.NEST_TRANS) THEN
	          GOTO 920
	        ENDIF
	      ENDIF
            NESTLV=NESTLV-1
	      IF(NESTLV.LE.0) EXIT
          ENDIF
	    IF(NESTLV.LE.0) EXIT
 240    CONTINUE
        IF(NESTLV.LE.0) EXIT
 300  CONTINUE
      GOTO 930

 900  IRTN=1000
      WRITE(MSGFL,905) CMD1
 905  FORMAT(' (SUBR.RDCYCEXT) ',A,' must be in a DO loop.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) CMD1
 915  FORMAT(' (SUBR.RDCYCEXT) ',A,' accepts no operands. ',
     %   'Missing semicolon?')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925) CMD1
 925  FORMAT(' (SUBR.RDCYCEXT) Jump by ',A,' breaks '
     %   'the loop of PUSH or TRANSPORT.')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935) CMD1
 935  FORMAT(' (SUBR.RDCYCEXT) ENDDO corresponding to ',A,
     %   ' not found.')
 940  IRTN=1004
      WRITE(MSGFL,945) ENDNAM(K1), LOPNAM(NEST(NESTLV))
 945  FORMAT(' (SUBR.RDCYCEXT) ',A,' does not match with ',A)
      RETURN
      END