      SUBROUTINE RDBEAMLN(LN,LINE,NCHAR,IRTN)
	USE BEAMLN
	USE READMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=3)

      CHARACTER*13 OP(MOP)/'expression','LINE','APERTURE'/
      INTEGER NFF(MOP)/0,1,2/
      INTEGER IDLINE,IDAPERT
      INTEGER IOP(MOP)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INTEGER J,I,NF,NC,K,NCNAM,NCTXT2,NN,ISTAT,IREP,IN,I0,NC1,II
	REAL(8) APERT(2)
	CHARACTER(1) C
      CHARACTER(32) BLNAM
	CHARACTER(MCTEXT) TEXT2
	CHARACTER(MAXMAGNAME+1), ALLOCATABLE:: NAM(:)	
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('BEAMLINE',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,LNKW,LNBL,
     %    LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
      DO I=1,MOP
        IOP(I)=0
      ENDDO
      J=1
      DO I=1,MOP
        ID(I)=J
        IF(OP(I).EQ.'LINE') IDLINE=ID(I)
	  IF(OP(I).EQ.'APERTURE') IDAPERT=ID(I)
        J=J+MAX(0,NFF(I))
      ENDDO
      DO I=1,J-1
        PAR(I)=UNDEF
      ENDDO
	NGSTRRD=0
	APERT(1)=0
	APERT(2)=0
C
      DO 400 J=1,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
	    IOP(I)=1
          IF(OP(I).EQ.'expression') THEN
	      CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,BLNAM,NCNAM,IRTN,MSGFL)
	      IF(IRTN.NE.0) GOTO 990
CC	      IF(J.NE.1) GOTO 930
	      CALL APSOFF(BLNAM(1:NCNAM),NCNAM)
          ENDIF
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          IF(OP(I).EQ.'LINE') THEN
	      NCTXT2=NC
	      TEXT2=TEXT(1:NC)
          ELSE
            DO K=1,NFF(I)
	        FFF(K)=UNDEF
            ENDDO
            CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
            IF(IRTN.NE.0) GOTO 990
            IF(NF.GT.NFF(I)) GOTO 910
            IF(NF.GE.1) THEN
              DO K=1,NF
                IF(FFF(K).NE.UNDEF) THEN
                  PAR(ID(I)+K-1)=FFF(K)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDIF
 400  CONTINUE
 	IF(IOP(1).EQ.0) GOTO 920
	DO I=1,2
	  IF(PAR(IDAPERT+I-1).NE.UNDEF) APERT(I)=PAR(IDAPERT+I-1)%X
	ENDDO
C       repeated twice. At first to count the number of elements.
	DO IREP=1,2
	  IN=0
	  I0=0
	  NN=0
	  DO I=1,NCTXT2
	    C=TEXT2(I:I)
	    IF(IN.EQ.0) THEN
	      IF(C.EQ.'(') THEN
	        IN=1
	      ELSEIF(C.NE.' ') THEN
		      GOTO 940
	      ENDIF
	    ELSEIF(IN.GE.2) THEN
	      IF(C.NE.' ') GOTO 942
	    ELSE
	      IF(C.EQ.' '.OR.C.EQ.','.OR.C.EQ.')') THEN
	        IF(I0.NE.0) THEN
	          IF(I-I0.GT.MAXMAGNAME+1) GOTO 950
	          NN=NN+1
	          IF(IREP.EQ.2) THEN
	            NAM(NN)=TEXT2(I0:I-1)
	            CALL APSOFF(NAM(NN),NC1)
	          ENDIF
	          I0=0
	        ENDIF
	        IF(C.EQ.')') IN=2
	      ELSE
	        IF(I0.EQ.0) I0=I
	      ENDIF
	    ENDIF
	  ENDDO
	  IF(IN.LE.1) GOTO 944
	  IF(NN.EQ.0) GOTO 946
	  IF(IREP.EQ.1) THEN
	    ALLOCATE(NAM(NN),STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 960
	  ENDIF
	ENDDO
	CALL DEFBL(BLNAM(1:NCNAM),NN,NAM,APERT,II,IRTN)
	DEALLOCATE(NAM,STAT=ISTAT)
	IF(IRTN.NE.0) GOTO 990
	IF(MSGLVL.GE.1) CALL RDBMLN1(II,MSGFL)
	        
 800  IRTN=0
      GOTO 1000
C---
C 900  IRTN=1000
C      WRITE(MSGFL,905) TEXT(1:NC)
C 905  FORMAT(' (SUBR.RDBEAMLN) Invalid operand "',A,'".')
C      GOTO 1000
 910  IRTN=1010
      WRITE(MSGFL,915) OP(I)
 915  FORMAT(' (SUBR.RDBEAMLN) Too many numbers for ',
     %  'operand "',A,'".')
      GOTO 1000
 920  IRTN=1020
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDBEAMLN) Beamline name not specified. ')
      GOTO 1000
 930  IRTN=1030
      WRITE(MSGFL,935) BLNAM(1:NCNAM)
 935  FORMAT(' (SUBR.RDBEAMLN) Invalid operand "',A,'". ',/,
     %    '   Must be the first operand if it is a beamline name.')
      GOTO 1000
 940  IRTN=1040
      WRITE(MSGFL,941) C
 941  FORMAT(' (SUBR.RDBEAMLN) Invalid beamline element sequence.',/,
     %   '   Non-blanck character "',A,'" before "(".')
      GOTO 1000
 942  IRTN=1042
      WRITE(MSGFL,943) C
 943  FORMAT(' (SUBR.RDBEAMLN) Invalid beamline element sequence.',/,
     %   '   Non-blanck character "',A,'" after ")".')
      GOTO 1000
 944  IRTN=1044
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.RDBEAMLN) Invalid beamline element sequence.',/,
     %   '   No closing ")".')
      GOTO 1000
 946  IRTN=1046
      WRITE(MSGFL,947)
 947  FORMAT(' (SUBR.RDBEAMLN) Invalid beamline element sequence.',/,
     %   '   No elements.')
      GOTO 1000
 950  IRTN=1050
      WRITE(MSGFL,955) TEXT2(I0:I-1)
 955  FORMAT(' (SUBR.RDBEAMLN) Too long element name.')
      GOTO 1000
 960  IRTN=1060
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDBEAMLN) Allocation failed. Too many elements?')
      GOTO 1000
 990  IRTN=1090
      GOTO 1000
 1000 DEALLOCATE(NAM,STAT=ISTAT)
	RETURN
      END

	SUBROUTINE RDBMLN1(IDBL,IFL)
	USE BEAMLN
	INTEGER IDBL,IFL
	INTEGER NEL,IBL,I,J,N,NC,NC0,NC1,M,MM
	INTEGER MC
	PARAMETER (MC=(MAXMAGNAME+2)*8)
	CHARACTER(MC) TEXT
	REAL(8) APERT1(2)

	NEL=BL(IDBL)%NEL
	NN=BL(IDBL)%NEXP
	NC=0
	IBL=0
	DO I=1,NEL
	  J=BL(IDBL)%ELID(I)
	  IF(BL(IDBL)%ELTYPE(I).EQ.0) THEN
	    NC=MAX(NC,MAG(J)%NC)
	  ELSE
	    NC0=MAX(NC,BL(J)%NC)
	    IF(BL(IDBL)%ELTYPE(I).LT.0) NC0=NC0+1
	    NC=MAX(NC,NC0)
	    IBL=1
	  ENDIF
	ENDDO
	DO N=1,NN
	  J=BL(IDBL)%MAGID(N)
	  NC=MAX(NC,MAG(J)%NC)
	ENDDO
	NC1=NC+1
	MM=MC/NC1
	WRITE(IFL,200) BL(IDBL)%NAME,BL(IDBL)%STOT,NEL
200   FORMAT(' ++++  Beamline "',A,'" ++++',/,
     %   '    Total length=',0PF10.5,' m',/,
     %   '  Configuration:  number of elements=',I4)
	M=0
	DO I=1,NEL
	  J=BL(IDBL)%ELID(I)
	  M=M+1
	  IF(BL(IDBL)%ELTYPE(I).EQ.0) THEN
	    TEXT(NC1*(M-1)+1:NC1*M)=MAG(J)%NAME(1:MAG(J)%NC)
	  ELSEIF(BL(IDBL)%ELTYPE(I).GT.0) THEN
	    TEXT(NC1*(M-1)+1:NC1*M)=BL(J)%NAME(1:BL(J)%NC)
	  ELSE
	    TEXT(NC1*(M-1)+1:NC1*M)='-'//BL(J)%NAME(1:BL(J)%NC)
	  ENDIF
	  IF(M.GE.MM) THEN
	    WRITE(IFL,'(4X,A)') TEXT(1:NC1*M)
	    M=0
	  ENDIF
	ENDDO
	IF(M.GE.1) WRITE(IFL,'(4X,A)') TEXT(1:NC1*M)
	IF(IBL.EQ.0) RETURN
	WRITE(IFL,220) NN
220   FORMAT('  Expanded form:  number of magnets=',I4)
	M=0
	DO N=1,NN
	  J=BL(IDBL)%MAGID(N)
	  M=M+1
	  TEXT(NC1*(M-1)+1:NC1*M)=MAG(J)%NAME(1:MAG(J)%NC)
	  IF(M.GE.MM) THEN
	    WRITE(IFL,'(4X,A)') TEXT(1:NC1*M)
	    M=0
	  ENDIF
	ENDDO
	IF(M.GE.1) WRITE(IFL,'(4X,A)') TEXT(1:NC1*M)

	RETURN
	END