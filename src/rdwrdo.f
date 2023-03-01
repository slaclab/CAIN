      SUBROUTINE RDWRDO(LN00,LINE,NCHAR,CMD1,MXX,NXX,LXX,XX,
     %    CHBUF,NCHBUF,MSGFL,IRTN)
	USE FLCHTYP
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER LN00(2,2),NCHAR(*),MXX,NXX,LXX(2,MXX),NCHBUF,MSGFL,IRTN
      CHARACTER*(*) LINE(*),CMD1,CHBUF
      TYPE(FLCHTYPE) XX(MXX)
      CHARACTER*10 OPX/'expression'/
      CHARACTER*1024 TEXT
      INTEGER MBLX,MLVL
      PARAMETER (MBLX=50,MLVL=5)
      INTEGER NBLX(MLVL),KBLX(MBLX,MLVL),RELX(MBLX,MLVL),
     %   LNKWX(2,2,MBLX,MLVL),LNBLX(2,2,MBLX,MLVL),KOPX(MLVL),
     %   LNX(2,3),J,IREP1(MLVL),JI(MLVL),IDI(2,MLVL),III(3,MLVL),
     %   IIF(MLVL),II(MLVL),LVL,NC,NC1,I,IRTN1,MCHBUF,K
	INTEGER, PARAMETER:: MIND=50
	INTEGER NNIND(MIND),IND(MIND),NIND,NCNAM,KK,N
	CHARACTER(16) NAM
	CHARACTER(80) ERR
	TYPE(FLCHTYPE) FC
	INCLUDE 'include/nestcm.h'
C
      LNX(1,2)=LN00(1,1)
      LNX(2,2)=LN00(2,1)
      LNX(1,3)=LN00(1,2)
      LNX(2,3)=LN00(2,2)
      CALL PAROFF(LNX(1,2),LINE,NCHAR,MSGFL,IRTN1)
      IF(IRTN1.GE.1000) THEN
        IRTN=1009
        GOTO 990
      ENDIF
	MCHBUF=LEN(CHBUF)
      LVL=1
 200  CALL CMDBLK(CMD1,1,OPX,0,1,MBLX,NBLX(LVL),KOPX(LVL),KBLX(1,LVL),
     %   RELX(1,LVL),LNKWX(1,1,1,LVL),LNBLX(1,1,1,LVL),
     %   LNX,LINE,NCHAR,IRTN)
      IF(IRTN.GE.2) GOTO 990
      IF(NBLX(LVL).EQ.0) GOTO 360
C  See if the block is a do-sequence or not
      JI(LVL)=0
      DO 220 J=1,NBLX(LVL)
        IF(RELX(J,LVL).NE.0) THEN
          IF(RELX(J,LVL).NE.1) GOTO 950
          IF(JI(LVL).NE.0) GOTO 900
          IF(J.EQ.1) GOTO 900
          IF(NBLX(LVL).LE.J.OR.NBLX(LVL).GT.J+2) GOTO 910
          CALL BLKREC(LNKWX(1,1,J,LVL),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
	    CALL EVLHS(TEXT(1:NC),KK,NAM,NCNAM,IDI(1,LVL),
     %                  MIND,NIND,NNIND,IND,ERR)
	    IF(ERR.NE.' ') GOTO 912
	    IF(KK.EQ.0) THEN
	      IF(TEXT(1:1).EQ.'$') GOTO 914
	      IF(NIND.NE.0) GOTO 916
            CALL EVDEFP1(NAM(1:NCNAM),0D0,IDI(1,LVL),IRTN)
	      IF(IRTN.GE.2) THEN
	        CALL SET00(NAM(1:NCNAM),0D0,'',0,IRTN)  ! for error message
	        GOTO 990
	      ENDIF
	      IDI(2,LVL)=0
	    ELSEIF(KK.EQ.1) THEN
	      IDI(2,LVL)=0
	    ELSEIF(KK.EQ.2) THEN
	      IF(NIND.NE.ARR(IDI(1,LVL))%RANK) GOTO 918
	      DO K=1,NIND
	        IF(NNIND(K).NE.1) GOTO 918
	      ENDDO
	      CALL ARRIND2N(IND,NIND,ARR(IDI(1,LVL))%DIM,N,IRTN)
	      IF(IRTN.NE.0) GOTO 920
	      IDI(2,LVL)=N
	    ELSEIF(KK.EQ.3) THEN
	      GOTO 914
	    ENDIF
	    IF(NESTLV.GE.1) THEN
	      DO I=1,NESTLV
	        IF(NEST(I).EQ.NEST_DO) THEN
	          IF(IDDOVAR(1,I).EQ.IDI(1,LVL)
     %               .AND.IDDOVAR(2,I).EQ.IDI(2,LVL)) GOTO 964
	        ENDIF
	      ENDDO
	    ENDIF
          IF(LVL.NE.1) THEN
            DO 210 I=1,LVL-1
              IF(IDI(1,LVL).EQ.IDI(1,I).AND.IDI(2,LVL).EQ.IDI(2,I)) 
     %               GOTO 960
 210        CONTINUE
          ENDIF
          IF(IRTN.GE.2) GOTO 990
          JI(LVL)=J
        ENDIF
 220  CONTINUE
      III(3,LVL)=1
      IF(JI(LVL).NE.0) THEN
C  Find initial, final and step of do variable
        DO 240 I=JI(LVL),NBLX(LVL)
          IF(I.EQ.JI(LVL)) THEN
            CALL BLKREC(LNBLX(1,1,I,LVL),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          ELSE
            CALL BLKREC(LNKWX(1,1,I,LVL),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          ENDIF
          IF(IRTN.NE.0) GOTO 990
          CALL EVAL0(TEXT(1:NC),FC,ERR)
          IF(ERR.NE.' ') GOTO 970
	    IF(FC%L.NE.1) THEN
	      ERR='Invalid non-floating number'
	      GOTO 970
	    ENDIF
          III(I-JI(LVL)+1,LVL)=NINT(FC%X)
 240    CONTINUE
        IF(III(3,LVL).EQ.0) GOTO 926
        IF((III(1,LVL).LT.III(2,LVL).AND.III(3,LVL).LT.0).OR.
     %     (III(1,LVL).GT.III(2,LVL).AND.III(3,LVL).LT.0)) GOTO 360
        IIF(LVL)=JI(LVL)-1
      ELSE
        III(1,LVL)=1
        III(2,LVL)=1
        IIF(LVL)=NBLX(LVL)
      ENDIF
      IREP1(LVL)=III(1,LVL)
 250  IF(JI(LVL).NE.0) THEN
        IF(IDI(2,LVL).EQ.0) THEN
          CALL EVDEFP2(1,IDI(1,LVL),DFLOAT(IREP1(LVL)))
	  ELSE
	    ARR(IDI(1,LVL))%VAL(IDI(2,LVL))=IREP1(LVL)
	  ENDIF
      ENDIF
      II(LVL)=1
 260  LNX(1,2)=LNKWX(1,1,II(LVL),LVL)
      LNX(2,2)=LNKWX(2,1,II(LVL),LVL)
      LNX(1,3)=LNKWX(1,2,II(LVL),LVL)
      LNX(2,3)=LNKWX(2,2,II(LVL),LVL)  
      CALL PAROFF(LNX(1,2),LINE,NCHAR,MSGFL,IRTN1)
      IF(IRTN1.GE.1000) GOTO 990
      IF(IRTN1.NE.0) THEN
        IF(LVL.GE.MLVL) GOTO 940
        LVL=LVL+1
        GOTO 200
      ELSE
        CALL BLKREC(LNKWX(1,1,II(LVL),LVL),LINE,NCHAR,TEXT,
     %       NC,IRTN,MSGFL)
        IF(IRTN.NE.0) GOTO 990
        IF(NC.GE.1) THEN
          IF(NXX.GE.MXX) GOTO 930
          NXX=NXX+1
	    IF(NCHBUF+NC.GT.MCHBUF) GOTO 984
	    LXX(1,NXX)=NCHBUF+1
	    NCHBUF=NCHBUF+NC
	    LXX(2,NXX)=NCHBUF
	    CHBUF(LXX(1,NXX):LXX(2,NXX))=TEXT(1:NC)
          CALL EVAL0(TEXT(1:NC),XX(NXX),ERR)
          IF(ERR.NE.' ') GOTO 980
	    IF(XX(NXX)%L.EQ.2) THEN
	      NC1=XX(NXX)%C(2)-XX(NXX)%C(1)+1
	      IF(NCHBUF+NC1.GT.MCHBUF) GOTO 984
	      CHBUF(NCHBUF+1:NCHBUF+NC1)
     %             =GSTR2(EVALLAST)(XX(NXX)%C(1):XX(NXX)%C(2))
	      XX(NXX)%C(1)=NCHBUF+1
	      NCHBUF=NCHBUF+NC1
	      XX(NXX)%C(2)=NCHBUF
	    ENDIF
        ENDIF
      ENDIF
 300  II(LVL)=II(LVL)+1
      IF(II(LVL).LE.IIF(LVL)) GOTO 260
      IREP1(LVL)=IREP1(LVL)+III(3,LVL)
      IF((III(3,LVL).GT.0.AND.IREP1(LVL).LE.III(2,LVL)).OR.
     %     (III(3,LVL).LT.0.AND.IREP1(LVL).GE.III(2,LVL))) GOTO 250
 360  LVL=LVL-1
      IF(LVL.GE.1) GOTO 300
      IRTN=0
      RETURN

 900  IRTN=1000
      WRITE(MSGFL,901) CMD1
 901  FORMAT(' (SUBR.RDWRDO) Invalid syntax of do-type ',A,':')
      CALL PRECHO(LN00(1,1),LN00(2,1),LN00(1,2),LN00(2,2),LINE,NCHAR)
      GOTO 990
 910  IRTN=1010
      WRITE(MSGFL,911) CMD1
 911  FORMAT(' (SUBR.RDWRDO) Invalid syntax of do-type ',A,':')
      CALL PRECHO(LNX(1,2),LNX(2,2),LNX(1,3),LNX(2,3),LINE,NCHAR)
 912  IRTN=1012
      WRITE(MSGFL,913) TEXT(1:NC),ERR
 913  FORMAT(' (SUBR.RDWRDO) Invalid syntax of do-type ',A,/,3X,A)
      GOTO 990
 914  IRTN=1014
      WRITE(MSGFL,915) TEXT(1:NC)
 915  FORMAT(' (SUBR.RDWRDO) Character type variable ',A,' invalid ',
     %  'as do control')
      GOTO 990
 916  IRTN=1016
      WRITE(MSGFL,917) TEXT(1:NC)
 917  FORMAT(' (SUBR.RDWRDO) Array ',A,' not declared.')
      GOTO 990
 918  IRTN=1018
      WRITE(MSGFL,919) TEXT(1:NC)
 919  FORMAT(' (SUBR.RDWRDO) Array ',A,' rank mismatch.')
      GOTO 990
 920  IRTN=1020
      WRITE(MSGFL,921) TEXT(1:NC)
 921  FORMAT(' (SUBR.RDWRDO) Invalid subscript range:',A)
      GOTO 990
	
 926  IRTN=1026
      WRITE(MSGFL,927) CMD1
 927  FORMAT(' (SUBR.RDWRDO) Do increment=0 in do-type ',A,'.')
      GOTO 990
 930  IRTN=1030
      WRITE(MSGFL,935) MXX
 935  FORMAT(' (SUBR.RDWRDO) Too many parameters to be printed.',/,
     %   '    You can write upto',I6,
     %   ' numbers by one WRITE/PRINT command.')
      GOTO 990
 940  IRTN=1040
      WRITE(MSGFL,945) CMD1
 945  FORMAT(' (SUBR.RDWRDO) Do nest level for ',A,' too deep.')
      GOTO 990
 950  IRTN=1050
      WRITE(MSGFL,955) CMD1
 955  FORMAT(' (SUBR.RDWRDO) Invalid relational operator in ',A)
      GOTO 990
 960  IRTN=1060
      WRITE(MSGFL,962) CMD1
 962  FORMAT(' (SUBR.RDWRDO) Duplicate do-control variable in ',A)
      GOTO 990
 964  IRTN=1064
      WRITE(MSGFL,965) CMD1
 965  FORMAT(' (SUBR.RDWRDO) DO loop variable in ',A,' already used.')
      GOTO 990
 970  IRTN=1070
      WRITE(MSGFL,975) TEXT(1:NC),ERR
 975  FORMAT(' (SUBR.RDWRDO) Expression error "',A,'" ',/,3X,A)
      GOTO 990
 980  IRTN=1080
      WRITE(MSGFL,982) TEXT(1:NC),ERR
 982  FORMAT(' (SUBR.RDWRDO) Expression error "',A,'" ',/,3X,A)
      GOTO 990
 984  IRTN=1084
      WRITE(MSGFL,985)
 985  FORMAT(' (SUBR.RDWRDO) Sorry. Character buffer full for writing ',
     %   'expression.',/,'   Consult the programmer.')
      GOTO 990
 990  RETURN
      END

      
