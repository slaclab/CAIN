      SUBROUTINE NXTBLK(L1,N1,L3,N3,LINE,NCHAR,
     %   LN,REL,L11,N11,IRTN)
C  Find next block separated by comma.
C  search in the range (L1,N1) to (L3,N3).
C  Structure of a block is either
C      X,   or   X = Y,
C     (Change in Aug.29.2001. Take into account only
C      substitution = and ignore relational operators)
C  X does not contain single = (== may exist).
C  Y may contain anything except delimiter comma. 
C  The comma may be missing if the end (L3,N3) is reached.
C  If a block is found,
C    LN(1,1,n) and LN(2,1,n) is the line-column of the
C    start of left-hand-side (n=1) or right-hand-side (n=2).
C    LN(*,2,n) is that for the end. (Does not include the
C    final comma.) 
C    REL is the type of relational operator, i.e.,
C       1:=, 2: <> or ><, 3:<, 4:>,
C       5: =< or <=,  6: => or >=.
C       When REL=0, relational operator is not found.
C       (In this case, LN(*,*,2) is not defined.)
C       !!! now REL=0 or 1 only (Aug.29.2001)
C  If found, the next-to-next block will start from (L11,N11).
C  (If the latter is beyond (L3,N3), L11=0 is returned.
C   In the not-found case L11 is also set to zero.)
C  IRTN=0:  found. (can be an empty block separated by
C           commas.)
C  IRTN=1:  not found.
      IMPLICIT NONE
      INTEGER L1,N1,L3,N3,LN(2,2,2),REL,L11,N11,NCHAR(L3),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER L,N,LVL,END,NCREL,INAPOS,CC,LR,K,IDREL,CATCOD
      CHARACTER*2 RELCH
      CHARACTER*1 APOS(2)/"'",'"'/,CH
      INTEGER MLVL
      PARAMETER (MLVL=20)
      INTEGER KK(MLVL)
      INCLUDE 'include/ctrlcm.h'
C
      L=L1
      N=N1
      END=0
      NCREL=0
      RELCH='  '
      LVL=0
      INAPOS=0
      LR=0
C             LR=0:  before finding l.h.s.
C                1:  l.h.s. started
C                2:  operator found
C                3:  r.h.s. started
      K=0
 200  IF(END.EQ.0) THEN
        CH=LINE(L)(N:N)
      ELSE
        CH=','
      ENDIF
      CC=CATCOD(CH)
      IF(INAPOS.NE.0) THEN
        IF(CH.EQ.APOS(INAPOS)) INAPOS=0      !  apos
C            closing apostrophy
      ELSE
        IF(LVL.EQ.0) THEN 
          IF(CC.EQ.8) THEN     !  comma
            GOTO 400
          ELSEIF(CC.EQ.7) THEN
	      IF(LR.EQ.0) GOTO 900
            IF(LR.EQ.1) LR=2
	      IF(LR.EQ.2) THEN
              IF(NCREL.GE.2) GOTO 900    !   should not come here
              NCREL=NCREL+1
              RELCH(NCREL:NCREL)=CH
	        IF(NCREL.EQ.2) THEN        !   allow '==' '>=' etc in l.h.s.
	          NCREL=0
	          LR=1
	        ENDIF
	      ENDIF
            GOTO 300
          ELSEIF(CC.NE.0) THEN        !  0: blanck
	      IF(LR.EQ.2) THEN
	        IF(RELCH.NE.'=') THEN     !    allow '<' '>' etc in l.h.s.
	          NCREL=0
	          LR=1
	        ENDIF
	      ENDIF
            IF(LR.EQ.0.OR.LR.EQ.2) THEN
              LR=LR+1
              K=K+1
              LN(1,1,K)=L
              LN(2,1,K)=N
            ENDIF
          ENDIF
        ENDIF
        IF(CC.GE.11.AND.CC.LE.13) THEN
C     opening parenthesis
          IF(LVL.GE.MLVL) GOTO 910
          LVL=LVL+1
          KK(LVL)=CC-10
        ELSEIF(CC.GE.21.AND.CC.LE.23) THEN
C     closing parenthesis
          IF(LVL.EQ.0) GOTO 920
          IF(KK(LVL).NE.CC-20) GOTO 930
          LVL=LVL-1
C     opening apostrophy
        ELSEIF(CH.EQ.APOS(1)) THEN
          INAPOS=1
	  ELSEIF(CH.EQ.APOS(2)) THEN
          INAPOS=2
        ENDIF
      ENDIF
 300  IF((LR.EQ.1.OR.LR.EQ.3).AND.CC.NE.0) THEN
        LN(1,2,K)=L
        LN(2,2,K)=N
      ENDIF
      CALL NXTPOS(L,N,L3,N3,NCHAR,END)
      IF(END.EQ.0) GOTO 200
 400  IF(LVL.NE.0) GOTO 940
      IF(INAPOS.NE.0) GOTO 950
      IF(LR.EQ.2) GOTO 960
      IRTN=0
      IF(LR.EQ.0) IRTN=1
      IF(LR.EQ.3) THEN
        REL=IDREL(RELCH(1:NCREL))
      ELSE
        REL=0
      ENDIF
      IF(END.EQ.0) CALL NXTPOS(L,N,L3,N3,NCHAR,END)
      IF(END.EQ.0) THEN
        L11=L
        N11=N
      ELSE
        L11=0
        N11=0
      ENDIF
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905) CH
 905  FORMAT(' (SUBR.NXTBLK) Miss-placed operator "',A,'" in') 
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.NXTBLK) Parenthesis level too deep in')
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.NXTBLK) Too many closing parenthesis.')
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.NXTBLK) Parenthesis pair does not match.')
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.NXTBLK) Closing parenthesis insufficient.')
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
 950  IRTN=1005
      WRITE(MSGFL,955)
 955  FORMAT(' (SUBR.NXTBLK) Missing closing apostrophy.')
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
      RETURN
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.NXTBLK) Missing right-hand-side in')
      CALL PRECHO(L1,N1,L3,N3,LINE,NCHAR)
      RETURN
      END
