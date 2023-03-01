      SUBROUTINE SET00(NAM,VAL,TEXT,MSGLV,IRTN)
C Add a variable to variable list or revise variable list
C  Input:  NAM    variable name (<= 16 characters)
C          VAL    value (REAL*8) (for floating)
C          TEXT   character string (for character)
C          MSGLV  message level (can be different from the common
C                 block variable MSGLVL)
C  Output: IRTN=0      new variable defined
C               1      variable value revised
C            1000      reserved parameter name
C            1001      reserved function name
C            1004      rejected by eval
C                      (e.g., too many variables,
C                      invalid variable name, etc)
      IMPLICIT NONE
      CHARACTER*(*) NAM,TEXT
      INTEGER MSGLV,IRTN
      REAL(8) VAL
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/ctrlcm2.h'
      INCLUDE 'include/ctrlcm3.h'
      INTEGER NC,I0,I1,I,II,IRTNEV,IDARR,IDUMMY
      CHARACTER*80 ERR
      CHARACTER*14 MSG
C
	NC=LEN(NAM)
	IF(NC.LE.0) RETURN
	I0=0
	DO I=1,NC
	  IF(NAM(I:I).NE.' ') THEN
	    IF(I0.EQ.0) I0=I
	    I1=I
	  ENDIF
	ENDDO
	IF(I0.EQ.0) RETURN
      DO 220 I=1,MRSVP
        II=I
        IF(NAM(I0:I1).EQ.RSVPAR(I)) GOTO 900
 220  CONTINUE
      DO 230 I=1,MUFN
        II=I
        IF(NAM(I0:I1).EQ.UFNAME(I)) GOTO 910
 230  CONTINUE
	IF(NAM(I0:I0).NE.'$') THEN
        CALL EVDEFP(NAM(I0:I1),VAL,IRTNEV)
        IF(IRTNEV.GE.2) GOTO 940
	ELSE
	  CALL EVDEFARR(NAM(I0:I1),1,0,2,IDUMMY,0D0,IDARR,IRTNEV,ERR)
	  IF(IRTNEV.GE.2) GOTO 940
	  IF(LEN(TEXT).NE.0) CALL EVARRCSET(IDARR,0,IDUMMY,TEXT,ERR)
	ENDIF
      IRTN=IRTNEV
      II=0
      DO 240 I=1,MIPAR
        IF(NAM(I0:I1).EQ.IPARNM(I)) THEN
          IPAR(I)=NINT(VAL)
          II=1
          IF(IPARNM(I).EQ.'MsgFile') THEN
            CALL EVINIT(3,0,' ',MSGFL)
          ENDIF
          IF(IPARNM(I).EQ.'Rand') THEN
            IF(MSGLV.GE.1) WRITE(MSGFL,235) JRAND
 235        FORMAT(' Random number reset. Rand=',I10)
            IF(LRAN11.NE.0) CALL RANINI(JRAND)
          ENDIF
          GOTO 270
        ENDIF
 240  CONTINUE
      DO 250 I=1,MRPAR
        IF(NAM(I0:I1).EQ.RPARNM(I)) THEN
          RPAR(I)=VAL
          GOTO 270
        ENDIF
 250  CONTINUE
C
 270  IF(MSGLV.GE.1) THEN
        IF(IRTNEV.EQ.1) THEN
          MSG='(revised)'
        ELSE
          MSG='(new variable)'
        ENDIF
        IF(II.EQ.0) THEN
          WRITE(MSGFL,280) NAM(I0:I1),VAL,MSG
 280      FORMAT(' SET ',A,'=',1PD15.8,' ; ',A)
        ELSE
          WRITE(MSGFL,290) NAM(I0:I1),NINT(VAL),MSG
 290      FORMAT(' SET ',A,'=',I6,' ; ',A)
        ENDIF
      ENDIF
      RETURN
C
 900  IRTN=1000
      IF(MSGLV.GE.0) THEN
        IF(II.LE.MRSVP1) THEN
          ERR='a reserved function name.'
        ELSEIF(II.LE.MRSVP1+MRSVP2+MRSVP3) THEN
          ERR='a reserved constant.'
        ELSE
          ERR='a running variable.'
        ENDIF
        WRITE(MSGFL,905) NAM(I0:I1),ERR(1:30)
 905    FORMAT(' (SUBR.RDSET) ',A,' is ',A,/,
     %    '    You cannot change it.')
      ENDIF
      RETURN
 910  IRTN=1001
      IF(MSGLV.GE.0) THEN
        ERR='a CAIN function name.'
        WRITE(MSGFL,905) NAM(I0:I1),ERR(1:30)
      ENDIF
      RETURN
 940  IRTN=1004
      IF(MSGLV.GE.0) WRITE(MSGFL,945) NAM(I0:I1)
 945  FORMAT(' (SUBR.RDSET) Error in defining the parameter "',
     %   A,'".')
      RETURN
      END
