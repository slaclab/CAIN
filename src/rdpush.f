      SUBROUTINE RDPUSH(IC,LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
      IMPLICIT NONE
      INTEGER IC,LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=1)
      CHARACTER*18 OP(MOP)/'Time'/
      INTEGER NFF(MOP)/3/
	INTEGER IDTIME
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/nestcm.h'
      INTEGER J,I,NF,NC,K,IFIRST,NT
      REAL*8 TT(3)
C
      IRTN=0
      IF(NESTRT.EQ.0) THEN
        IF(NESTLV.NE.0) THEN
          DO 120 I=1,NESTLV-1
            IF(NEST(I).EQ.NEST_PUSH.OR.NEST(I).EQ.NEST_TRANS) GOTO 940
 120      CONTINUE
        ENDIF
        IF(NESTLV.GE.MNEST) GOTO 950
        NESTLV=NESTLV+1
        NEST(NESTLV)=NEST_PUSH
        ICNEST(NESTLV)=IC
      ELSE
        NESTRT=0
        IFIRST=0
        GOTO 500
      ENDIF
      IRTN=0
      IF(LN(1,2).EQ.0) GOTO 900
      CALL CMDBLK('PUSH',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) GOTO 900
      J=1
      DO 180 I=1,MOP
        ID(I)=J
	  IF(OP(I).EQ.'Time') IDTIME=ID(I)
        J=J+MAX(0,NFF(I))
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
C
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          DO 220 K=1,NFF(I)
	      FFF(K)=UNDEF
 220      CONTINUE
          CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
          IF(IRTN.NE.0) GOTO 990
          IF(NF.GT.NFF(I)) GOTO 910
          IF(NF.GE.1) THEN
            DO 240 K=1,NF
              IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240        CONTINUE
          ENDIF
        ENDIF
 300  CONTINUE
      DO 320 I=1,3
	  IF(PAR(IDTIME+I-1).EQ.UNDEF) THEN
	    IF(I.LE.2) GOTO 920
	    TT(I)=0
	  ELSE
          TT(I)=FFF(I)%X
	  ENDIF
 320  CONTINUE
      NT=NINT(TT(3))
      IF(NT.LT.0) GOTO 930
      IF(NT.NE.0.AND.TT(1).GT.TT(2)) GOTO 930
      IFIRST=1
      INPUSH=1
 500  CALL PUSH(IFIRST,NT,TT,IRTN)
      IF(IRTN.NE.0) GOTO 990
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.RDPUSH) No parameter specified.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) OP(I)
 915  FORMAT(' (SUBR.RDPUSH) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDPUSH) Time range not specified.')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDPUSH) Wrong time interval.')
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.RDPUSH) PUSH/TRANSPORT cannot nest.')
      RETURN
 950  IRTN=1008
      WRITE(MSGFL,955) MNEST
 955  FORMAT(' (SUBR.RDPUSH) Nest level too deep. ',/,
     %  ' Sum of PUSH,TRANSPORT,IF,DO nest must be <=',I2)
      RETURN
 990  IRTN=1009
      RETURN
      END
