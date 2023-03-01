      SUBROUTINE WRBEAM(FILE,LR,KIN,LLOST,FSEL,LBMFMT,INCP,IRTN)
	USE FLCHTYP
	USE BEAMCM
      IMPLICIT NONE
      INTEGER FILE,LR(2),KIN(3),LLOST,LBMFMT,INCP,IRTN
      CHARACTER*(*) FSEL
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/bmfmt.h'
      INTEGER N,I,NC,IRTN1,NP1,ITYPE(3),IDLM
	TYPE(FLCHTYPE) F1
	INTEGER IFLRKN
	CHARACTER*128 TEXT
C
      CALL CPUTIM('WRBEAM',1)
      IF(NP.LE.0) GOTO 900
C       A bit complicated because IFLRKN does not allow
C       .and. between ITYPE(i)'s
C       What is needed is
C          If  INCP       incoh with LOST=0
C              LOST       normal+test with LOST=2
C              INCP+LOST  incp with LOST=2 only
C              nothing    normal+test with LOST=0
C       Check of lost is done after IFLRKN
	ITYPE=0
	IF(INCP.EQ.0) THEN
	  ITYPE(1)=1
	  ITYPE(3)=1
	ELSE
	  ITYPE(2)=1
	ENDIF
	IDLM=0
	IF(FSEL.NE.' ') THEN
	  CALL EVCMPL(FSEL,IDLM,TEXT)
	  IF(TEXT.NE.' ') GOTO 920
	ENDIF
      NP1=0
      DO 300 N=1,NP
	  IF(IFLRKN(N,LR,KIN,0,0,LLOST,ITYPE).EQ.0) GOTO 300
	  IF(IDLM.GE.1) THEN
	    CALL SETVAR(N,IRTN)
          IF(IRTN.NE.0) CYCLE
	    CALL EVLOAD(IDLM,F1,IRTN)
	    IF(IRTN.NE.0.OR.F1%L.NE.1) GOTO 930
	    IF(F1%X.EQ.0) CYCLE
	  ENDIF
        NP1=NP1+1
        IF(LBMFMT.EQ.0) THEN
          IF(NP1.EQ.1) WRITE(FILE,STDTTL)
          WRITE(FILE,STDFMT) KIND(N),GEN(N),PNAME(N),WGT(N),
     %      (TXYS(I,N),I=0,3),(EP(I,N),I=0,3),(SPIN(I,N),I=1,3)
        ELSEIF(LBMFMT.EQ.1) THEN
          IF(NP1.EQ.1) WRITE(FILE,SHTTTL)
          WRITE(FILE,SHTFMT) KIND(N),GEN(N),PNAME(N),WGT(N),
     %      (TXYS(I,N),I=0,3),(EP(I,N),I=0,3),(SPIN(I,N),I=1,3)
        ELSE
          WRITE(FILE,MATHFMT) KIND(N),GEN(N),PNAME(N),WGT(N),
     %      (TXYS(I,N),I=0,3),(EP(I,N),I=0,3),(SPIN(I,N),I=1,3)
        ENDIF
 300  CONTINUE
      IF(MSGLVL.GE.1) THEN
	  IF(NP1.EQ.0) THEN
	    CALL TYPTTL(LR,KIN,LLOST,ITYPE,TEXT,NC)
          WRITE(MSGFL,310) FILE,TEXT(1:NC)
 310      FORMAT(' (SUBR.WRBEAM) Warning: Nothing to write on file#',
     %            I2,' for',/,5X,A)
	    IF(IDLM.GE.1) WRITE(MSGFL,305) FSEL
 305      FORMAT('     Particle selection: ',A)
	  ELSE
	    WRITE(MSGFL,320) NP1,FILE
 320      FORMAT(' ',I6,' particle data written on file#',I2)
        ENDIF
      ENDIF
      IRTN=0
      GOTO 1000
 900  IRTN=1000
      IF(MSGLVL.GE.0) WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.WRBEAM) Warning:Beam is empty. ',
     %   'Ignore PRINT/WRITE BEAM.')
      GOTO 1000
 910  IRTN=1001
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925) TEXT,FSEL
 925  FORMAT(1X,A,/,' (SUBR.WRBEAM) Invalid SELECT function',/,
     %  '    "',A,'"')
      GOTO 1000
 930  IRTN=1003
      WRITE(MSGFL,925) FSEL
 935  FORMAT(1X,A,/,' (SUBR.WRBEAM) Error in evaluating the particle ',
     %   'selection function:',/,'    "',A,'"')
      GOTO 1000
 1000 IF(IDLM.GE.1) CALL EVLMFREE(IDLM)
      CALL CPUTIM('WRBEAM',2)
      RETURN
      END
