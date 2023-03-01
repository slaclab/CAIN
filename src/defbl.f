	SUBROUTINE DEFBL(NAM,NEL,B,APERT,II,IRTN)
	USE BEAMLN
	IMPLICIT NONE
	INTEGER NEL,IRTN
	CHARACTER(*) NAM
	CHARACTER(*) B(NEL)
	REAL(8) APERT(2)
      INCLUDE 'include/ctrlcm.h'
	INTEGER NC,I,II,J,ISTAT
	LOGICAL REV
	CHARACTER(MAXMAGNAME) NAM1
	REAL(8) APERT1(2)
	LOGICAL REDEF

	IF(NMAG.LE.0) GOTO 900

	CALL CHKMAGNAME(NAM,NC)
	IF(NC.GT.MAXMAGNAME.OR.NC.LE.0) GOTO 910
	IF(NMAG.GT.0) THEN
	  DO I=1,NMAG
	    IF(NAM.EQ.MAG(I)%NAME) GOTO 920
	  ENDDO
	ENDIF
	II=NBEAMLINE+1
	REDEF=.FALSE.
	IF(NBEAMLINE.GE.1) THEN
	  DO I=1,NBEAMLINE
	    IF(NAM.EQ.BL(I)%NAME) THEN
	      II=I
	      REDEF=.TRUE.
	      EXIT
	    ENDIF
	  ENDDO
	ENDIF
	IF(II.GE.MBEAMLINE) GOTO 930

	IF(REDEF) THEN
	  CALL BLDEALLOC1(II)
	  IF(MSGLVL.GE.1) THEN
	    WRITE(MSGFL,140) NAM(1:NC)
140       FORMAT(' (SUBR.DEFBL) Warning: beamline "',A,'" redefined.')
        ENDIF
	ENDIF

	BL(II)%NAME=NAM
	BL(II)%NC=NC
	BL(II)%NEL=NEL
	BL(II)%APERT=APERT
	BL(II)%NEXP=0
	BL(II)%STOT=0
	BL(II)%LTWISS=.FALSE.
	ALLOCATE(BL(II)%ELID(NEL),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 980
	ALLOCATE(BL(II)%ELTYPE(NEL),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 980

	DO I=1,NEL
	  BL(II)%ELID(I)=0
	  DO J=1,NMAG
	    IF(MAG(J)%NAME.EQ.B(I)) THEN
	      BL(II)%ELTYPE(I)=0
	      BL(II)%ELID(I)=J
	      EXIT
	    ENDIF
	  ENDDO
	  IF(BL(II)%ELID(I).EQ.0.AND.NBEAMLINE.GE.1) THEN
	    IF(B(I)(1:1).EQ.'-') THEN
	      REV=.TRUE.
	      NAM1=B(I)(2:LEN(B(I)))
	    ELSE
	      REV=.FALSE.
	      NAM1=B(I)
	    ENDIF
	    DO J=1,NBEAMLINE
	      IF(BL(J)%NAME.EQ.NAM1) THEN
	        BL(II)%ELTYPE(I)=1
	        IF(REV) BL(II)%ELTYPE(I)=-1
	        BL(II)%ELID(I)=J
	        EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	  IF(BL(II)%ELID(I).EQ.0) GOTO 950
	ENDDO
	IF(II.GT.NBEAMLINE) NBEAMLINE=II
C       Expand beamline
	CALL EXPANDBL(NAM,IRTN)
	IF(IRTN.EQ.102) GOTO 960
	IF(IRTN.EQ.103) GOTO 970
	IF(IRTN.EQ.1000) GOTO 980
	IF(IRTN.NE.0) GOTO 990
C           (this should not happen. Stop with no message)
C  Here, if REDEF, all the beamlines that cite NAM directly
C  or indirectly must be re-expanded.
	IRTN=0

	RETURN
C---- Error before allocation
900   IRTN=1000
	IF(MSGLVL.GE.0) WRITE(MSGFL,905)
905   FORMAT(' (SUBR.DEFBL) No magnets defined prior to command',
     %         ' BEAMLINE.')
      RETURN
910	IRTN=1001
	IF(MSGLVL.GE.0) WRITE(MSGFL,915) NAM
915   FORMAT(' (SUBR.DEFBL) Invalid beamline name ','"',A,'"')
      RETURN
920	IRTN=1002
	IF(MSGLVL.GE.0) WRITE(MSGFL,925) NAM
925   FORMAT(' (SUBR.DEFBL) Beamline name "',A,'" already defined ',
     %             'as a magnet name.')
      RETURN
930	IRTN=1003
	IF(MSGLVL.GE.0) WRITE(MSGFL,935)
935   FORMAT(' (SUBR.DEFBL) Too many beamlines defined.',/,
     %             '    Increase MBEAMLINE by ALLOCATE command.')
      RETURN
C---- Error after allocation
950	IRTN=1005
	IF(MSGLVL.GE.0) WRITE(MSGFL,955) B(I)
955   FORMAT(' (SUBR.DEFBL) Undefined magnet/beamline "',A,'".')
	GOTO 990
960	IRTN=1006
	IF(MSGLVL.GE.0) WRITE(MSGFL,965) NAM(1:NC)
965   FORMAT(' (SUBR.DEFBL) Nesting level of beamline "',
     %       A,'" too deep.')
	GOTO 990
970	IRTN=1007
	IF(MSGLVL.GE.0) WRITE(MSGFL,975) NAM(1:NC)
975   FORMAT(' (SUBR.DEFBL) Beamline "',A,'" contains recursive ',
     %   'citation.')
	GOTO 990
980	IRTN=1008
	IF(MSGLVL.GE.0) WRITE(MSGFL,985)
985   FORMAT(' (SUBR.DEFBL) Memory allocation error.')
	GOTO 990
990   CALL BLDEALLOC1(II)
	IF(II.EQ.NBEAMLINE) NBEAMLINE=NBEAMLINE-1
C        When this beamline is not the last one, the area is not released.
	RETURN
	END
