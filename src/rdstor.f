      SUBROUTINE RDSTOR(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE ARRAYMOD
	USE LUMCOM
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=2)
      CHARACTER*12 OP(MOP)/'FILE','LUMINOSITY'/
      INTEGER NFF(MOP)/1,0/
      INTEGER UNIT,NCFN,IOSTAT,LLUM
      CHARACTER*7 STATUS
      CHARACTER*512 FNAME
	CHARACTER(80) ERR
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/lumcom2.h'
      INCLUDE 'include/stdstor.h'
      INTEGER J,NF,I,NC,K
	TYPE(FLCHTYPE) FC
C
      IRTN=0
      CALL CMDBLK('STORE',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
      STATUS=' '
      FNAME=' '
	NCFN=1
	UNIT=0
      LLUM=0
C
      IF(NBL.EQ.0) GOTO 320
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'LUMINOSITY') THEN
            LLUM=1
          ENDIF
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          IF(OP(I).EQ.'FILE') THEN
	      CALL EVAL0(TEXT(1:NC),FC,ERR)
	      IF(ERR.NE.' ') GOTO 960
	      IF(FC%L.EQ.1) THEN
	        UNIT=NINT(FC%X)
	      ELSE
	        FNAME=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
	        NCFN=FC%C(2)-FC%C(1)+1
            ENDIF
	      GOTO 300
          ELSE
            DO 220 K=1,NFF(I)
	        FFF(K)=UNDEF
 220        CONTINUE
            CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
            IF(IRTN.NE.0) GOTO 990
            IF(NF.GT.NFF(I)) GOTO 900
            IF(NF.GE.1) THEN
              DO 240 K=1,NF
                IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 300  CONTINUE
C-- Open file
 320  IF(UNIT.LE.0) THEN
        UNIT=98
        IF(FNAME.EQ.' ') THEN
          IF(LLUM.EQ.0) THEN
            FNAME=STDSTFL
          ELSE
            IF(NLUM.LE.0) GOTO 950
            FNAME=STDSTFLL
          ENDIF
	    CALL SPCOFF(FNAME,3,NCFN)
	  ENDIF
		CALL OPENFL(UNIT,FNAME(1:NCFN),'UNKNOWN',0,NCFN,IRTN)
	  IF(IRTN.NE.0) GOTO 920
        IF(MSGLVL.GE.1) THEN
          IF(LLUM.EQ.0) THEN
            WRITE(MSGFL,410) ':',FNAME(1:NCFN)
 410        FORMAT(' STORE',A,' FILE=',1H',A,1H')
          ELSE
            WRITE(MSGFL,410) ' LUMINOSITY:',FNAME(1:NCFN)
          ENDIF
        ENDIF
      ENDIF
C-- Write
      IF(LLUM.EQ.0) THEN
	  CALL STORVAR(UNIT)
      ELSE
        CALL STORELUM(UNIT)
      ENDIF
C-- Close file
      IF(FNAME.NE.' ') CLOSE(UNIT=UNIT,IOSTAT=IOSTAT)
      IF(IOSTAT.NE.0) GOTO 940
      GOTO 800
C---
 800  IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDSTOR) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDSTOR) Invalid file name.')
      RETURN
 920  IRTN=1002
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945) UNIT,IOSTAT,FNAME(1:NCFN)
 945  FORMAT(' (SUBR.RDSTOR) Close file failed. Unit=',I2,/,
     %   '   IOSTAT=',I5,'  File name=',A)
      RETURN
 950  IRTN=0
      IF(MSGLVL.GE.0) WRITE(MSGFL,955)
 955  FORMAT(' (SUBR.RDSTOR) No luminosity defined. Ignore ',
     %   'STORE LUMINOSITY')
      RETURN
 960  IRTN=1006
      IF(MSGLVL.GE.0) WRITE(MSGFL,965) ERR
 965  FORMAT(' (SUBR.RDSTOR) Invalid expression for file name.',/,
     %  3X,A)
      RETURN
 990  IRTN=1009
      RETURN
      END

	SUBROUTINE STORVAR(UNIT)
	USE ARRAYMOD
	IMPLICIT NONE
	INCLUDE 'include/ctrlcm.h'
	INTEGER UNIT
	INTEGER NPAR,I,IRTN1,N,J,L,NA
	REAL*8 VAL
	CHARACTER*16 NAM
C  Scalar-floating variables
      CALL EVINQ(1,NPAR,NAM,VAL,IRTN1)
C        exclude MsgLevel and MsgFile
	WRITE(UNIT,100) '++++',NPAR-2
 100  FORMAT(A4,I10)
      DO I=1,NPAR
        CALL EVINQ(2,I,NAM,VAL,IRTN1)
	  IF(NAM.EQ.'MsgLevel'.OR.NAM.EQ.'MsgFile') CYCLE
        WRITE(UNIT,200) NAM,VAL
 200    FORMAT(A16,1PD25.17)
        IF(MSGLVL.GE.1) WRITE(MSGFL,220) NAM,VAL
 220    FORMAT(5X,A16,1PD25.17)
      ENDDO
C  Arrays
	IF(NARRAY.EQ.0) RETURN
	NA=0
	DO I=1,NARRAY
	  IF(ARR(I)%TYPE.NE.0) NA=NA+1
	ENDDO
	IF(NA.EQ.0) RETURN
	WRITE(UNIT,100) '----',NA
	DO I=1,NARRAY
	  IF(ARR(I)%TYPE.EQ.0) CYCLE
	  WRITE(UNIT,300) ARR(I)%NAME,ARR(I)%NC,ARR(I)%TYPE,ARR(I)%RANK
 300    FORMAT(A16,3I10)
        N=1
        IF(ARR(I)%RANK.NE.0) THEN
	    DO J=1,ARR(I)%RANK
	      N=N*ARR(I)%DIM(3,J)
	      WRITE(UNIT,320) (ARR(I)%DIM(L,J),L=1,3)
 320        FORMAT(I10)
          ENDDO
        ENDIF
	  IF(ARR(I)%TYPE.EQ.1) THEN
	    WRITE(UNIT,340) (ARR(I)%VAL(J),J=1,N)
 340      FORMAT(1PD25.17)
        ELSE
	    DO J=1,N
	      WRITE(UNIT,360) ARR(I)%LC(2,J)-ARR(I)%LC(1,J)+1,
     %          GSTR(ARR(I)%LC(1,J):ARR(I)%LC(2,J))
 360        FORMAT(I10,A)
          ENDDO
        ENDIF
	ENDDO
	RETURN
	END
