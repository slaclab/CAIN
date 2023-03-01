      SUBROUTINE RDSET(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=2)
      CHARACTER*12 OP(MOP)/'expression','COMPILE'/
C   When COMPILE is specified, use EVCMPL and EVLOAD instead of EVAL0(for test)
      INTEGER NFF(MOP)/1,0/
      INTEGER NC,J,KK,L,I,ICMPL,IDLM,LC(2),NC1
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
	INTEGER, PARAMETER:: MIND=50
	INTEGER NNIND(MIND),IND(MIND),NIND,NCNAM,IDARR,LFCR,LFCL,
     %     IDUMMY(2,1)
	CHARACTER(16) NAM
	CHARACTER(80) ERR
      INTEGER IRTNEV,IRTN1
      REAL*8 X
	TYPE(FLCHTYPE) FCR
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('SET',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GE.2) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.EQ.0) RETURN
	ICMPL=0
      DO 300 J=1,NBL
	  I=KBL(J)
	  IF(OP(I).EQ.'COMPILE') THEN
	    ICMPL=1
	    CYCLE
	  ENDIF
        IF(REL(J).NE.1) GOTO 900
C           left-hand-side
        CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
        IF(IRTN.NE.0) GOTO 990
	  CALL EVLHS(TEXT(1:NC),KK,NAM,NCNAM,IDARR,
     %                  MIND,NIND,NNIND,IND,ERR)
	  IF(ERR.NE.' ') GOTO 910
	  LFCL=1
	  IF(KK.EQ.0) THEN
C               (name that has not defined yet)
	    IF(TEXT(1:1).EQ.'$') LFCL=2
	  ELSEIF(KK.EQ.3) THEN
	    LFCL=2
	  ENDIF
C           right-hand-side
        CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
        IF(IRTN.NE.0) GOTO 990
	  IF(ICMPL.EQ.0) THEN
          CALL EVAL0(TEXT(1:NC),FCR,ERR)
          IF(ERR.NE.' ') GOTO 902
	  ELSE
	    CALL EVCMPL(TEXT(1:NC),IDLM,ERR)
	    IF(ERR.NE.' ') GOTO 902
	    CALL EVLOAD(IDLM,FCR,IRTN1)
	    CALL EVLMFREE(IDLM)
          IF(IRTN1.NE.0) GOTO 924
	  ENDIF
	  IF(FCR%L.NE.LFCL) THEN
		  IF(LFCL.EQ.1) GOTO 912
	    IF(LFCL.EQ.2) GOTO 914
	  ENDIF
	  IF(LFCL.EQ.1) THEN
	    IF(NIND.EQ.0) THEN
            CALL SET00(NAM(1:NCNAM),FCR%X,'',MSGLVL,IRTNEV)
            IF(IRTNEV.GE.2) GOTO 990
	    ELSE
	      IF(KK.EQ.0) GOTO 904    ! undefined name
	      IF(KK.EQ.1) GOTO 906    ! scalar
	      DO L=1,NIND
	        IF(NNIND(L).NE.1) GOTO 908
	      ENDDO
	      CALL EVARRSET(IDARR,NIND,IND,FCR%X,ERR)
	      IF(ERR.NE.' ') GOTO 910
	    ENDIF
	  ELSE
	    IF(NIND.EQ.0) THEN
	      IF(KK.EQ.0) THEN
	        CALL EVDEFARR(NAM(1:NCNAM),2,0,2,IDUMMY,0D0,IDARR,
     %                 IRTN,ERR)
	        IF(ERR.NE.' ') GOTO 920
	      ENDIF
C       must asign string
	    ELSE
	      IF(KK.EQ.0) GOTO 904    ! undefined name
	      IF(KK.EQ.1) GOTO 906    ! scalar
	      DO L=1,NIND
	        IF(NNIND(L).NE.1) GOTO 908
	      ENDDO
	    ENDIF
C           !!!!! Assume the buffer area GSTR2(FCR%C(1):FCR%C(2)) is still kept
C                 since the last call of EVAL0/EVLOAD.
	    CALL EVARRCSET(IDARR,NIND,IND,
     %             GSTR2(EVALLAST)(FCR%C(1):FCR%C(2)),ERR)
	    IF(ERR.NE.' ') GOTO 910
	  ENDIF
	  IF(MSGLVL.GE.1) THEN
	    CALL ARRAYSTR(NAM(1:NCNAM),NIND,1,1,IND,TEXT,NC)
	    IF(FCR%L.EQ.1) THEN
	      WRITE(MSGFL,260) TEXT(1:NC),FCR%X
 260        FORMAT('  SET ',A,'=',1PD15.8)
          ELSE
	      WRITE(MSGFL,280) TEXT(1:NC),
     %        GSTR2(EVALLAST)(FCR%C(1):FCR%C(2))
 280        FORMAT('  SET ',A,'="',A,'"')
          ENDIF
        ENDIF
 300  CONTINUE
      IRTN=0
      RETURN
C 900  IRTN=1000
C      WRITE(MSGFL,905)
C 905  FORMAT(' (SUBR.RDSET) Invalid syntax in SET command.')
C      CALL PRECHO(LN(1,1),LN(2,1),LN(1,3),LN(2,3),LINE,NCHAR)
C      RETURN
 900  IRTN=1000
      WRITE(MSGFL,901)
 901  FORMAT(' (SUBR.RDSET) Missing "=" sign in SET command.')
      CALL PRECHO(LN(1,1),LN(2,1),LN(1,3),LN(2,3),LINE,NCHAR)
      RETURN
 902  IRTN=1002
      WRITE(MSGFL,903) TEXT(1:NC),ERR
 903  FORMAT(' (SUBR.RDSET) Invalid expression "',A,'"',/,3X,A)
      RETURN
 904  IRTN=1004
      WRITE(MSGFL,905) NAM(1:NCNAM)
 905  FORMAT(' (SUBR.RDSET) Undefined array name: ',/,'   "',A,'"')
	RETURN
 906  IRTN=1000
      WRITE(MSGFL,907) TEXT(1:NC)
 907  FORMAT(' (SUBR.RDSET) Array index on scalar variable: ',/,
     %        '   "',A,'"')
	RETURN
 908  IRTN=1008
      WRITE(MSGFL,909) TEXT(1:NC)
 909  FORMAT(' (SUBR.RDSET) Invalid use of colon in array subscript ',/,
     %        '   "',A,'"')
	RETURN
 910  IRTN=1010
	WRITE(MSGFL,911) ERR,TEXT(1:NC)
 911  FORMAT(A,/,'   "',A,'"')
	RETURN
 912  IRTN=1012
	WRITE(MSGFL,913) TEXT(1:NC)
 913  FORMAT(' (SUBR.RDSET) Substitute character into floating variable'
     %    ,/,'   "',A,'"')
	RETURN
 914  IRTN=1014
	WRITE(MSGFL,915) TEXT(1:NC)
 915  FORMAT(' (SUBR.RDSET) Substitute floating variable into character'
     %    ,/,'   "',A,'"')
	RETURN
 920  IRTN=1020
	WRITE(MSGFL,921) ERR,TEXT(1:NC)
 921  FORMAT(A,/,'   "',A,'"')
	RETURN
 924  IRTN=1024
      WRITE(MSGFL,925) IRTN1,TEXT(1:NC)
 925  FORMAT(' (SUBR.RDSET) Invalid expression. EVLOAD rtn code',I5,/,
     %  '   "',A,'"')
      RETURN
 990  RETURN
      END

