      SUBROUTINE CMDBLK(CMD,MOP,OP,MPOS,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
C Read a command block.
C Input
C  CMD      Command name. For message only.
C  MOP      Number of operand keywords.
C  MPOS     Number of positional keywords. They have to be flag-type
C           operands. The separator after them is not mandatory.
C  OP(i) (i=1,MOP)   Keywards.
C           If one of the keywords is 'expression',
C           unidentified keywords (left-hand-side) do not cause an 
C           error message.
C  NFF(i)   Number of parameters for each keyword.
C           0 for flag-type keywords and -1 for flag-type positional
C           keywords. Actually, in this subroutine,
C           only whether NFF is 0 or -1 or >0 makes sense.
C           NFF corresponding to 'expression' must be >=0.
C           (Present version does not allow 'expression' as a
C           positional operand because of a grammatical ambiguity--
C           ----i.e., a blanck character is a delimator in keywords
C           while it is allowed in an expression.)
C           When NFF>0, right-hand-side is expexted, whereas when
C           NFF<=0, no right-hand-side expected. Violation of these
C           rules causes abnormal return. An exception is that, when
C           the keyword is 'expression' with NFF>0 and no right-hand-
C           side found, returns with IRTN=0.
C  MBL      Maximum number of blocks.
C  LN,LINE,NCHAR
C Output
C  NBL      Number of operand blocks found.
C  KOP(i) (i=1,MOP)  0 if the i-th keyword is not specified.
C                    n (>0) if the i-th keyword is specified in the n-th
C           operand block. If there are more than one operand with the
C           same keyword, only the last one is stored in KOP.
C  KBL(j) (j=1,NBL)  If >0, j-th operand block is KBL(j)-th keyword.
C  REL(j) (j=1,MBL)  If the keyword for the j-th block is not flag-type,
C           id of relational operator.
C            1:=, 2:<> or ><, 3:<, 4:>, 5:<= or =<, 6:>= or =>.
C  LNKW(l,k,j) (l=1,2),(k=1,2),(j=1,NBL)
C          Start (k=1) line(l=1) and column(l=2) and end (k=2) line-column
C          of the keyword (left-hand-side) of j-th block.
C  LNBL(l,k,j) (l=1,2),(k=1,2),(j=1,NBL)
C          Start (k=1) line(l=1) and column(l=2) and end (k=2) line-column
C          of the right-hand-side of j-th block.
C  IRTN     Return code.
C            0:  normal
C            1:  no operand specified.
C            2:  positional operand not enough
C           >2:  fatal.
      IMPLICIT NONE
      INTEGER MOP,MPOS,NFF(MOP),MBL,NBL,KOP(MOP),KBL(MBL),REL(MBL),
     %   LNKW(2,2,MBL),LNBL(2,2,MBL),LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) CMD,OP(*),LINE(*)
      INCLUDE 'include/ctrlcm.h'
      INTEGER L,N,L2,N2,L3,N3,I,K,IEXP,IEND
      INTEGER CC,CATCOD,IFCMD,NC,LNX(2,2,2),RELX
      CHARACTER*1 CH
      CHARACTER*2048 TEXT
C
      IRTN=0
      NBL=0
      DO 100 I=1,MOP
        KOP(I)=0
 100  CONTINUE
      L=LN(1,2)
      IF(L.EQ.0.OR.LN(1,2).GT.LN(1,3).OR.
     %  (LN(1,2).EQ.LN(1,3).AND.LN(2,2).GT.LN(2,3))) THEN
        IRTN=1
        RETURN
      ENDIF
      IEXP=0
      DO 120 I=1,MOP
        IF(OP(I).EQ.'expression') IEXP=I
 120  CONTINUE
      IF(IEXP.NE.0) THEN
        IF(NFF(IEXP).LT.0) THEN
          WRITE(MSGFL,140) CMD
 140      FORMAT(' *****(SUBR.CMDBLK) PROGRAM ERROR IN READING ',
     %      A,' *****',/,'  NFF for expression must be <0')
          CALL STOPCAIN(100)
        ENDIF
      ENDIF
      N=LN(2,2)
      L3=LN(1,3)
      N3=LN(2,3)
      IF(MPOS.EQ.0) GOTO 200
C------- Read positional operand
      IEND=0
      NC=0
 160  IF(IEND.NE.0) THEN
C           comes here when end-of-command-block
        CH=' '
      ELSE
        CH=LINE(L)(N:N)
      ENDIF
      CC=CATCOD(CH)
      IF(CC.EQ.0.OR.CC.EQ.8) THEN
C            blanck or comma
        IF(NC.NE.0) THEN
          I=IFCMD(TEXT(1:NC),MOP,OP)
          IF(I.EQ.0) GOTO 900
          IF(I.LT.0) GOTO 910
          IF(NFF(I).NE.-1) GOTO 920
          NBL=NBL+1
          KOP(I)=NBL
          KBL(NBL)=I
          NC=0
          IF(IEND.NE.0) THEN
            IF(NBL.NE.MPOS) IRTN=2
            RETURN
          ENDIF
          IF(NBL.EQ.MPOS) THEN
            CALL NXTPOS(L,N,L3,N3,NCHAR,IEND)
            IF(IEND.NE.0) RETURN
            GOTO 200
          ENDIF
        ENDIF
        IF(IEND.NE.0) RETURN
      ELSEIF(CC.LE.2) THEN
C            alpha-numerical
        IF(NC.GE.16) GOTO 900
        NC=NC+1
        TEXT(NC:NC)=CH
      ELSE
        GOTO 930
      ENDIF
      CALL NXTPOS(L,N,L3,N3,NCHAR,IEND)
      GOTO 160
C---- Read the rest
 200  CALL NXTBLK(L,N,L3,N3,LINE,NCHAR,LNX,RELX,L2,N2,IRTN)
      IF(IRTN.EQ.1) GOTO 400
      IF(IRTN.NE.0) GOTO 990
      CALL BLKREC(LNX(1,1,1),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
      IF(IRTN.NE.0) GOTO 990
      I=IFCMD(TEXT(1:NC),MOP,OP)
      K=2
      IF(IEXP.EQ.0) THEN
        IF(I.EQ.0) GOTO 900
        IF(I.LT.0) GOTO 910
      ELSE
        IF(I.LE.0) THEN
          I=IEXP
          K=1
        ENDIF
      ENDIF
      IF(NFF(I).LE.0.AND.RELX.NE.0) GOTO 940
      IF(NFF(I).GE.1.AND.RELX.EQ.0.AND.I.NE.IEXP) GOTO 950
      NBL=NBL+1
      KOP(I)=NBL
      KBL(NBL)=I
      REL(NBL)=RELX
      LNKW(1,1,NBL)=LNX(1,1,1)
      LNKW(2,1,NBL)=LNX(2,1,1)
      LNKW(1,2,NBL)=LNX(1,2,1)
      LNKW(2,2,NBL)=LNX(2,2,1)
      IF(RELX.NE.0) THEN
        LNBL(1,1,NBL)=LNX(1,1,2)
        LNBL(2,1,NBL)=LNX(2,1,2)
        LNBL(1,2,NBL)=LNX(1,2,2)
        LNBL(2,2,NBL)=LNX(2,2,2)
      ENDIF
 380  IF(L2.NE.0) THEN
        L=L2
        N=N2
        GOTO 200
      ENDIF
 400  IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) CMD,TEXT(1:NC)
 905  FORMAT(' (SUBR.CMDBLK)',A,': Keyword "',A,'" does not exist.')
      GOTO 990
 910  IRTN=1001
      WRITE(MSGFL,915) CMD,TEXT(1:NC)
 915  FORMAT(' (SUBR.CMDBLK)',A,': Keyword "',A,'" is ambiguos.')
      GOTO 990
 920  IRTN=1002
      WRITE(MSGFL,925) CMD,OP(I)
 925  FORMAT(' (SUBR.CMDBLK)',A,': Keyword "',A,'" is not a ',
     %  'positional keyword.')
      GOTO 990
 930  IRTN=1003
      WRITE(MSGFL,935) CMD,CH
 935  FORMAT(' (SUBR.CMDBLK)',A,': Invalid character "',A,'" in the ',
     %  'following block.')
      CALL PRECHO(LN(1,2),LN(2,2),L3,N3,LINE,NCHAR)
      GOTO 990
 940  IRTN=1004
      WRITE(MSGFL,945) CMD,OP(I)
 945  FORMAT(' (SUBR.CMDBLK)',A,': Keyword "',A,'" does not expect ',
     %  'right-hand-side.')
      GOTO 990
 950  IRTN=1005
      WRITE(MSGFL,955) CMD,OP(I)
 955  FORMAT(' (SUBR.CMDBLK)',A,': Keyword "',A,'" requires ',
     %  'right-hand-side.')
      GOTO 990
 990  RETURN
      END
