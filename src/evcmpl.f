	SUBROUTINE EVCMPL(T,IDLM,ERR)
C  Create a "load module"
C   input
C      T     text of expression
C   output
C      IDLM  load module id (error if IDLM <= 0)
	USE FLCHTYP
	USE EVLMOD
      IMPLICIT NONE
	CHARACTER*(*) T,ERR
	INTEGER IDLM
C	INCLUDE 'include/evlmod.h'
	INTEGER, PARAMETER:: MP=500
      INTEGER PL(3,MP)
      TYPE(FLCHTYPE) PC(MP)
	CHARACTER(MGSTRLM) GSTR3
	INTEGER NGSTR3
	INTEGER I,J,NP,ISTAT
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
      INCLUDE 'include/evalcm.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER IDBG,IFL,IDONE
	COMMON/EVCMPLDBG/IDBG,IFL,IDONE
C       IDBG: debug flag of EVCMPL. If IDBG>0, print load module
C       and its first IDBG use in EVLOAD.
C       There must not be another call to EVCMPL in between.
	IDBG=0
	IFL=OUTFL
	IDONE=0
C
      LCHK=0
	NGSTR3=0
      CALL EVCMPL0(T,MP,NP,PL,PC,GSTR3,NGSTR3,ERR)
	IDLM=0
	IRTNEV=0
      IF(ERR.NE.' '.OR.NP.LE.0) THEN
	  IRTNEV=1
	ELSE
	  DO I=1,MLOAD
	    IF(LOADMOD(I)%NP.LE.0) THEN
	      IDLM=I
	      EXIT
	    ENDIF
	  ENDDO
	  IF(IDLM.EQ.0) THEN
	    IRTNEV=1
	    ERR='Load module stack full.  (EVCMPL)'
	  ELSE
	    ALLOCATE(LOADMOD(IDLM)%PL(3,NP),STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 900
	    ALLOCATE(LOADMOD(IDLM)%PC(NP),STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 900
	    ALLOCATE(LOADMOD(IDLM)%GSTRLM,STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 900
	    LOADMOD(IDLM)%NP=NP
	    DO J=1,NP
	      LOADMOD(IDLM)%PL(1:3,J)=PL(1:3,J)
	      LOADMOD(IDLM)%PC(J)=PC(J)
	    ENDDO
	    LOADMOD(IDLM)%NGSTRLM=NGSTR3
	    LOADMOD(IDLM)%GSTRLM=GSTR3(1:NGSTR3)
	    ERR=' '
	    IF(IDBG.NE.0) CALL PRCMPLLM(T,LOADMOD(IDLM),NP,OUTFL)
	  ENDIF
	ENDIF
      RETURN
900	ERR='Allocation failed    (EVCMPL)'
	IRTNEV=1
	CALL EVLMFREE(IDLM)
	RETURN
      END

	SUBROUTINE PRCMPLLM(T,LM,NP,IFL)
	USE FLCHTYP
	USE ARRAYMOD
	USE EVLMOD
	IMPLICIT NONE
C	INCLUDE 'include/evlmod.h'
	INCLUDE 'include/ctrlcm3.h'
	CHARACTER(*) T
	TYPE(LOADMODULE) LM
	INTEGER NP,IFL
	INTEGER IP,IC,NC,NC1
	CHARACTER(120) TEXT
	CHARACTER(2) BINOP(14)/'+','-','*','/','^',' ',
     %    '==','/=','>','>=','<','<=','&&','||'/
	WRITE(IFL,100) T
100   FORMAT(' -- Load Module for ',A)
	DO IP=1,NP
	  IC=LM%PL(1,IP)
	  WRITE(TEXT,'(I3,1X,I4,1X)') IP,IC
	  NC=9
	  IF(IC.EQ.1) THEN
	    NC1=11
	    WRITE(TEXT(NC+1:NC+NC1),"('Load const ')")
	    NC=NC+NC1
	    IF(LM%PC(IP)%L.EQ.1) THEN
	      NC1=12
	      WRITE(TEXT(NC+1:NC+NC1),'(1PD12.5)') LM%PC(IP)%X
	      NC=NC+NC1
	    ELSE
	      NC1=LM%PC(IP)%C(2)-LM%PC(IP)%C(1)+3
	      WRITE(TEXT(NC+1:NC+NC1),'(A)')
     %        '"'//LM%GSTRLM(LM%PC(IP)%C(1):LM%PC(IP)%C(2))//'"'
	      NC=NC+NC1
	    ENDIF
	    NC1=11
	    WRITE(TEXT(NC+1:NC+NC1),"(' -> X(',I4,')')") LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.EQ.2) THEN
	    NC1=24
	    WRITE(TEXT(NC+1:NC+NC1),"('Load X(',I4,') -> X(',I4,')')")
     %       LM%PL(3,IP),LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.EQ.3) THEN
	    IF(LM%PL(3,IP).GE.0) THEN
	      NC1=30
	      WRITE(TEXT(NC+1:NC+NC1),
     %           "('Load float.par ',I4,' -> X(',I4,')')")
     %           LM%PL(3,IP),LM%PL(2,IP)
	      NC=NC+NC1
	    ELSE
	      NC1=31
	      WRITE(TEXT(NC+1:NC+NC1),
     %          "('Load charac.par ',I4,' -> X(',I4,')')")
     %          -LM%PL(3,IP),LM%PL(2,IP)
	      NC=NC+NC1
	    ENDIF
	  ELSEIF(IC.GE.4.AND.IC.LE.17) THEN
	    NC1=27
	    WRITE(TEXT(NC+1:NC+NC1),
     %      "('X(',I4,')',A,'X(',I4,') -> X(',I4,')')")
     %      LM%PL(2,IP),BINOP(IC-3),LM%PL(3,IP),LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.GE.21.AND.IC.LE.22) THEN
	    NC1=19
	    WRITE(TEXT(NC+1:NC+NC1),"(A1,'X(',I4,') -> X(',I4,')')")
     %       BINOP(IC-20),LM%PL(2,IP),LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.GE.23.AND.IC.LE.45) THEN
	    NC1=28
	    WRITE(TEXT(NC+1:NC+NC1),"(A8,'(X(',I4,')) -> X(',I4,')')")
     %      RSVPAR(IC-22),LM%PL(2,IP),LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.GE.46.AND.IC.LE.47) THEN
	    NC1=36
	    WRITE(TEXT(NC+1:NC+NC1),
     %      "(A8,'(X(',I4,'),X(',I4,')) -> X(',I4,')')")
     %      RSVPAR(IC-22),LM%PL(2,IP),LM%PL(2,IP)+1,LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.GE.48.AND.IC.LE.49) THEN
	    NC1=40
	    WRITE(TEXT(NC+1:NC+NC1),
     %      "(A8,'(X(',I4,'),..,X(',I4,')) -> X(',I4,')')")
     %      RSVPAR(IC-22),LM%PL(2,IP),LM%PL(2,IP)+LM%PL(3,IP)-1,
     %      LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.GE.101.AND.IC.LE.999) THEN
	    NC1=48
	    WRITE(TEXT(NC+1:NC+NC1),
     %      "(A16,'(X(',I4,'),..,X(',I4,')) -> X(',I4,')')")
     %      UFNAME(IC-100),LM%PL(2,IP),LM%PL(2,IP)+LM%PL(3,IP)-1,
     %      LM%PL(2,IP)
	    NC=NC+NC1
	  ELSEIF(IC.GE.1001) THEN
	    NC1=37+ARR(IC-1000)%NC
	    WRITE(TEXT(NC+1:NC+NC1),
     %      "('Array ',A,'(X(',I4,'),..,X(',I4,')) -> X(',I4,')')")
     %      ARR(IC-1000)%NAME(1:ARR(IC-1000)%NC),LM%PL(2,IP),
     %      LM%PL(2,IP)+LM%PL(3,IP)-1,LM%PL(2,IP)
	    NC=NC+NC1
	  ENDIF
	  WRITE(IFL,'(A)') TEXT(1:NC)
	ENDDO
	RETURN
	END

