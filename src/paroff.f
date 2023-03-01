      SUBROUTINE PAROFF(LN,LINE,NCHAR,MSGFL,IRTN)
C  Take off the outermost parenthesis if match.
C  IRTN=0:  No parenthesis eliminated
C       1:  One or more than one pairs eliminated
C   >=1000:  Error.  (If outermost pair is normally eliminated,
C                     IRTN=1 even if the rest contains an error.)
C  Rule:
C    *  i-th (i=odd) and (i+1)th apostrophes always make a pair.
C       Therefore, for eample, 'X('')' is considered to consist
C       of two parts 'X('  and ')'.
C    *  All characters between a pair of apostrophes are
C       considered to be normal characters (i.e., paranthesis
C       are not considered as parenthesis)
C    *  Except for the above ones, parenthesis must much.
C    *  If there are no characters or only blanck characters
C       to the left and to the right of a pair of parenthesis
C       (or apostrophes), that pair is eliminated
C  Output:
C       LN is revised.
C
      IMPLICIT NONE
      INTEGER LN(2,2),NCHAR(*),MSGFL,IRTN
      CHARACTER*(*) LINE(*)
      INTEGER L1,N1,L2,N2,L,N,LI,NI,LF,NF,K,KK,LVL,IN,IRTN1
      CHARACTER*1 CH
      INTEGER MLVL
      PARAMETER (MLVL=10)
      INTEGER KKK(MLVL)
      CHARACTER*1 PP(6)/'(','[','{',')',']','}'/,APOS(2)/"'",'"'/
      INTEGER INAPOS
C
      L1=LN(1,1)
      N1=LN(2,1)
      L2=LN(1,2)
      N2=LN(2,2)
      KK=0
      IRTN=0
 200  IF(L1.GT.L2.OR.(L1.EQ.L2.AND.N1.GE.N2)) GOTO 400
      L=L1
      N=N1
      INAPOS=0
      LVL=0
      IN=0
C       IN=0: before the first pair
C          1: between
C          2: after the first pair
 220  CH=LINE(L)(N:N)
      IF(IN.EQ.2.AND.CH.NE.' ') GOTO 400
      IF(INAPOS.NE.0) THEN
        IF(CH.EQ.APOS(INAPOS)) THEN
          INAPOS=0
C       Following 5 lines eliminated Oct.15.2001 (do not eminate quotes)
c          IF(IN.EQ.1.AND.KK.EQ.4) THEN
c            IN=2
c            LF=L
c            NF=N
c          ENDIF
        ENDIF
      ELSE
        IF(CH.EQ.APOS(1).OR.CH.EQ.APOS(2)) THEN
          IF(CH.EQ.APOS(1)) THEN
			  INAPOS=1
	    ELSE
	      INAPOS=2
	    ENDIF
C       Following 6 lines eliminated Oct.15.2001
c          IF(IN.EQ.0) THEN
c            KK=4
c            IN=1
c            LI=L
c            NI=N
c          ENDIF
        ELSE
          DO 300 K=1,6
            IF(CH.EQ.PP(K)) THEN
              IF(K.LE.3) THEN
C----- opening parenthsis
                IF(LVL.GE.MLVL) THEN
                  IF(IRTN.NE.1) GOTO 900
                  GOTO 400
                ENDIF
                LVL=LVL+1
                KKK(LVL)=K
                IF(IN.EQ.0) THEN
                  KK=K
                  IN=1
                  LI=L
                  NI=N
                ENDIF
              ELSE
C----- closing parenthesis
                IF(LVL.LE.0) THEN
                  IF(IRTN.NE.1) GOTO 910
                  GOTO 400
                ENDIF
                IF(KKK(LVL).NE.K-3) THEN
                  IF(IRTN.NE.1) GOTO 910
                  GOTO 400
                ENDIF
                IF(IN.EQ.1.AND.LVL.EQ.1.AND.K-3.EQ.KK) THEN
                  IN=2
                  LF=L
                  NF=N
                ENDIF
                LVL=LVL-1
              ENDIF
              GOTO 320
            ENDIF
 300      CONTINUE
        ENDIF
      ENDIF
      IF(IN.EQ.0.AND.CH.NE.' ') GOTO 400
 320  CALL NXTPOS(L,N,L2,N2,NCHAR,IRTN1)
      IF(IRTN1.EQ.0) GOTO 220
      IF(LVL.NE.0.OR.IN.EQ.1) GOTO 910
      IF(IN.EQ.0) GOTO 400
      L1=LI
      N1=NI
      L2=LF
      N2=NF
      CALL NXTPOS(L1,N1,LF,NF,NCHAR,IRTN1)
      CALL PRVPOS(L2,N2,LI,NI,NCHAR,IRTN1)
      IRTN=1
      GOTO 200
 400  LN(1,1)=L1
      LN(2,1)=N1
      LN(1,2)=L2
      LN(2,2)=N2
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.PAROFF) Parenthesis level too deep.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.PAROFF) Parenthesis do not match.')
      RETURN
      END

      SUBROUTINE PAROFF2(LN,LINE,NCHAR,MSGFL,IRTN)
C  Take off the outermost parenthesis and apostrophes if match.
C  IRTN=0:  No parenthesis eliminated
C       1:  One or more than one pairs eliminated
C   >=1000:  Error.  (If outermost pair is normally eliminated,
C                     IRTN=1 even if the rest contains an error.)
C  Rule:
C    *  i-th (i=odd) and (i+1)th apostrophes always make a pair.
C       Therefore, for eample, 'X('')' is considered to consist
C       of two parts 'X('  and ')'.
C    *  All characters between a pair of apostrophes are
C       considered to be normal characters (i.e., paranthesis
C       are not considered as parenthesis)
C    *  Except for the above ones, parenthesis must much.
C    *  If there are no characters or only blanck characters
C       to the left and to the right of a pair of parenthesis
C       (or apostrophes), that pair is eliminated
C  Output:
C       LN is revised.
C
      IMPLICIT NONE
      INTEGER LN(2,2),NCHAR(*),MSGFL,IRTN
      CHARACTER*(*) LINE(*)
      INTEGER L1,N1,L2,N2,L,N,LI,NI,LF,NF,K,KK,LVL,IN,IRTN1
      CHARACTER*1 CH
      INTEGER MLVL
      PARAMETER (MLVL=10)
      INTEGER KKK(MLVL)
      CHARACTER*1 PP(6)/'(','[','{',')',']','}'/,APOS(2)/"'",'"'/
      INTEGER INAPOS
C
      L1=LN(1,1)
      N1=LN(2,1)
      L2=LN(1,2)
      N2=LN(2,2)
      KK=0
      IRTN=0
 200  IF(L1.GT.L2.OR.(L1.EQ.L2.AND.N1.GE.N2)) GOTO 400
      L=L1
      N=N1
      INAPOS=0
      LVL=0
      IN=0
C       IN=0: before the first pair
C          1: between
C          2: after the first pair
 220  CH=LINE(L)(N:N)
      IF(IN.EQ.2.AND.CH.NE.' ') GOTO 400
      IF(INAPOS.NE.0) THEN
        IF(CH.EQ.APOS(INAPOS)) THEN
          INAPOS=0
          IF(IN.EQ.1.AND.KK.EQ.4) THEN
            IN=2
            LF=L
            NF=N
          ENDIF
        ENDIF
      ELSE
        IF(CH.EQ.APOS(1).OR.CH.EQ.APOS(2)) THEN
          IF(CH.EQ.APOS(1)) THEN
			  INAPOS=1
	    ELSE
	      INAPOS=2
	    ENDIF
          IF(IN.EQ.0) THEN
            KK=4
            IN=1
            LI=L
            NI=N
          ENDIF
        ELSE
          DO 300 K=1,6
            IF(CH.EQ.PP(K)) THEN
              IF(K.LE.3) THEN
C----- opening parenthsis
                IF(LVL.GE.MLVL) THEN
                  IF(IRTN.NE.1) GOTO 900
                  GOTO 400
                ENDIF
                LVL=LVL+1
                KKK(LVL)=K
                IF(IN.EQ.0) THEN
                  KK=K
                  IN=1
                  LI=L
                  NI=N
                ENDIF
              ELSE
C----- closing parenthesis
                IF(LVL.LE.0) THEN
                  IF(IRTN.NE.1) GOTO 910
                  GOTO 400
                ENDIF
                IF(KKK(LVL).NE.K-3) THEN
                  IF(IRTN.NE.1) GOTO 910
                  GOTO 400
                ENDIF
                IF(IN.EQ.1.AND.LVL.EQ.1.AND.K-3.EQ.KK) THEN
                  IN=2
                  LF=L
                  NF=N
                ENDIF
                LVL=LVL-1
              ENDIF
              GOTO 320
            ENDIF
 300      CONTINUE
        ENDIF
      ENDIF
      IF(IN.EQ.0.AND.CH.NE.' ') GOTO 400
 320  CALL NXTPOS(L,N,L2,N2,NCHAR,IRTN1)
      IF(IRTN1.EQ.0) GOTO 220
      IF(LVL.NE.0.OR.IN.EQ.1) GOTO 910
      IF(IN.EQ.0) GOTO 400
      L1=LI
      N1=NI
      L2=LF
      N2=NF
      CALL NXTPOS(L1,N1,LF,NF,NCHAR,IRTN1)
      CALL PRVPOS(L2,N2,LI,NI,NCHAR,IRTN1)
      IRTN=1
      GOTO 200
 400  LN(1,1)=L1
      LN(2,1)=N1
      LN(1,2)=L2
      LN(2,2)=N2
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.PAROFF2) Parenthesis level too deep.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.PAROFF2) Parenthesis do not match.')
      RETURN
      END
