      SUBROUTINE RDELSEIF(IC,ICMD,NCMD,LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
      IMPLICIT NONE
      INTEGER IC,NCMD,ICMD(NCMD),LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/cmdnam.h'
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/pushcm.h'
      INTEGER MOP
      PARAMETER (MOP=1)
      CHARACTER*12 OP(MOP)/'expression'/
      INTEGER NFF(MOP)/1/
      INTEGER J,K,NC,NC1,IC1
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
C
      IRTN=0
      IF(NESTLV.LE.0) GOTO 960
      IF(NEST(NESTLV).NE.NEST_IF) GOTO 960
	IF(NESTST(NESTLV).GE.3) GOTO 960
	NESTST(NESTLV)=2
      IF(NESTRT.EQ.0) THEN
C        From the line above --> find ENDIF
100     CALL FNDEND(NEST_IF,IC,ICMD,NCMD,IC1,IRTN)
        IF(IRTN.GE.10) GOTO 960
	  IF(IRTN.NE.0) THEN
	    IC=IC1
          GOTO 100
	  ENDIF
        IC=IC1
	  NESTRT=1
	  IRTN=0
	  RETURN
	ENDIF
      IF(LN(1,2).EQ.0) GOTO 930
      CALL CMDBLK('IF',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GE.2) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.NE.1) GOTO 910
      J=1
      CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
      IF(IRTN.NE.0) GOTO 910
	IF(REL(J).EQ.1) THEN
C           comes here in the case of 'IF  x=y'  instead of 'IF x==y'.
	  DO K=NC,1,-1
	    TEXT(K+1:K+1)=TEXT(K:K)
	  ENDDO
	  TEXT(1:1)='('
	  TEXT(NC+2:NC+5)=')==('
	  NC1=NC+6
	  CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,
     %              TEXT(NC1:MCTEXT),NC,IRTN,MSGFL)
        IF(IRTN.NE.0) GOTO 910
	  NC=NC1+NC
	  TEXT(NC:NC)=')'
	ENDIF
	CALL EVAL0(TEXT(1:NC),FC,ERR)
      IF(ERR.NE.' ') GOTO 940
	IF(FC%L.NE.1) GOTO 950
      IF(FC%X.NE.0) THEN
C--- True ---
        IC=IC+1
        NESTRT=0
C--- False ---
	ELSE
        CALL FNDEND(NEST_IF,IC,ICMD,NCMD,IC1,IRTN)
        IF(IRTN.GE.10) GOTO 920
        NESTRT=1
        IC=IC1
	ENDIF
	IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) MNEST
 905  FORMAT(' (SUBR.RDELSEIF) Nest level too deep. ',/,
     %  ' Sum of PUSH,IF,DO nest must be <=',I2)
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDELSEIF) Invalid syntax for IF command.')
      CALL PRECHO(LN(1,1),LN(2,1),LN(1,3),LN(2,3),LINE,NCHAR)
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDELSEIF) Corresponding ENDIF not found.')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDIF) No operand for ELSEIF command.')
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945) ERR
 945  FORMAT(' (SUBR.RDELSEIF) ',A)
      RETURN
 950  IRTN=1005
      WRITE(MSGFL,955) 
 955  FORMAT(' (SUBR.RDELSEIF) Character expression for IF statement.')
      RETURN
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDELSE) IF/ENDIF nested incorrectly.')
      RETURN
 990  IRTN=1009
      RETURN
      END