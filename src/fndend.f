      SUBROUTINE FNDEND(K,IC,ICMD,NCMD,ICF,IRTN)
C  Find terminal statement for 
C  PUSH(K=1), IF(K=2), DO(K=3), TRANSPORT(K=4)
C  IRTN=0 if ENDPUSH,ENDIF,ENDDO,ENDTRANSPORT found.
C  IRTN=1 if ELSE is found
C  IRTN=2 if ELSEIF is found
C       1000   no end.
      IMPLICIT NONE
      INTEGER K,IC,NCMD,ICMD(NCMD),ICF,IRTN
      INCLUDE 'include/cmdnam.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/ctrlcm.h'
      INTEGER LVL0,IC1,K1
C
      IRTN=0
      LVL0=NESTLV
      DO 300 IC1=IC+1,NCMD
        DO 200 K1=1,MNEST_TYPE
          IF(CMD(ICMD(IC1)).EQ.LOPNAM(K1)) THEN
            IF(NESTLV.EQ.MNEST) GOTO 900
            NESTLV=NESTLV+1
            NEST(NESTLV)=K1
            GOTO 300
          ENDIF
 200    CONTINUE
        IF(CMD(ICMD(IC1)).EQ.'ELSE') THEN
          IF(K.EQ.NEST_IF.AND.NESTLV.EQ.LVL0) THEN
            ICF=IC1
            IRTN=1
            RETURN
          ELSE
            IF(NEST(NESTLV).NE.NEST_IF) GOTO 910
          ENDIF
        ENDIF
        IF(CMD(ICMD(IC1)).EQ.'ELSEIF') THEN
          IF(K.EQ.NEST_IF.AND.NESTLV.EQ.LVL0) THEN
            ICF=IC1
            IRTN=2
            RETURN
          ELSE
            IF(NEST(NESTLV).NE.NEST_IF) GOTO 910
          ENDIF
        ENDIF
        DO 240 K1=1,MNEST_TYPE
          IF(CMD(ICMD(IC1)).EQ.ENDNAM(K1)) THEN
            IF(NESTLV.EQ.LVL0) THEN
              IF(K.EQ.K1) THEN
                ICF=IC1
                RETURN
              ELSE
                GOTO 920
              ENDIF
            ELSE
              IF(NEST(NESTLV).NE.K1) GOTO 920
              NESTLV=NESTLV-1
            ENDIF
          ENDIF
 240    CONTINUE
 300  CONTINUE
      GOTO 930
 900  IRTN=1000
      WRITE(MSGFL,905) MNEST
 905  FORMAT(' (SUBR.FNDEND) Nest level too deep.',/,
     %  '  Total level for PUSH/TRANSPORT/IF/DO must be <=',I3)
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.FNDEND) ELSE does not correspond to IF.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925) ENDNAM(K1)
 925  FORMAT(' (SUBR.FNDEND) Invalid terminal command ',A)
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935) LOPNAM(K)
 935  FORMAT(' (SUBR.FNDEND) Terminal command not found for ',A)
      RETURN
      END
