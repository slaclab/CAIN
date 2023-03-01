      SUBROUTINE PLFUNC(FILE,NEW,PARNAM,RANGE,NDIV,LOGX,
     %   FUNX,FUNY,
     %   XYMM,LOGHV,NLMOD,SMOD,TITLE,TITLEX,TITLEY,ICOLOR,MSGFL,IRTN)
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER FILE,NEW,LOGHV(2),LOGX,NLMOD,NDIV,ICOLOR,MSGFL,IRTN
      CHARACTER*(*) PARNAM,FUNX,FUNY,TITLE,TITLEX,TITLEY
      REAL*8 XYMM(2,2),RANGE(2),SMOD(*)
      INTEGER I,N,NC0(2,3),NC(3),NN,IRTN1
      REAL*8 XYM(2,2),XY1,DX,X
	TYPE(FLCHTYPE) F1,F2
      REAL*8 XYW0(2,2)/0.0,13.0,0.0,10.0/,XYW(2,2)/0.0,10.8,0.0,10.0/
      CHARACTER*256 TTL(2,3)
      CHARACTER*1 SEP/';'/
      CHARACTER*256 TEXT
      INTEGER MPOINT
      PARAMETER (MPOINT=5001)
      INTEGER IDLM(2)
      REAL*8 WORK(2,MPOINT)
C
      IF(LOGX.NE.0.AND.(RANGE(1).LE.0.OR.RANGE(2).LE.0)) GOTO 900
      CALL SET00(PARNAM,0D0,'',0,IRTN1)
      IF(IRTN1.GE.2) GOTO 990
      CALL EVCMPL(FUNX,IDLM(1),TEXT)
      IF(TEXT(1:60).NE.' ') GOTO 910
      CALL EVCMPL(FUNY,IDLM(2),TEXT)
      IF(TEXT(1:60).NE.' ') GOTO 920
      CALL RDTTL(TITLE,2,TTL(1,1),NC0(1,1),SEP)
      CALL RDTTL(TITLEX,2,TTL(1,2),NC0(1,2),SEP)
      CALL RDTTL(TITLEY,2,TTL(1,3),NC0(1,3),SEP)
      DO 200 I=1,3
        NC(I)=NC0(1,I)
 200  CONTINUE
C
      N=MIN(NDIV,MPOINT-1)
      IF(N.LE.0) N=100
      IF(LOGX.EQ.0) THEN
        DX=(RANGE(2)-RANGE(1))/N
      ELSE
        DX=LOG(RANGE(2)/RANGE(1))/N
      ENDIF
      NN=0
      DO 220 I=0,N
        IF(LOGX.EQ.0) THEN
          X=RANGE(1)+DX*I
        ELSE
          X=RANGE(1)*EXP(DX*I)
        ENDIF
        CALL SET00(PARNAM,X,'',0,IRTN1)
        IF(IRTN1.GE.2) GOTO 220
        CALL EVLOAD(IDLM(1),F1,IRTN1)
        IF(IRTN1.EQ.0.AND.F1%L.EQ.1) THEN
          CALL EVLOAD(IDLM(2),F2,IRTN1)
          IF(IRTN1.EQ.0.AND.F2%L.EQ.1) THEN
            NN=NN+1
            IF(LOGHV(1).EQ.0.AND.ABS(F1%X).LE.1D-28) F1%X=0
            IF(LOGHV(2).EQ.0.AND.ABS(F2%X).LE.1D-28) F2%X=0
            WORK(1,NN)=F1%X
            WORK(2,NN)=F2%X
          ENDIF
        ENDIF
 220  CONTINUE
      IF(NN.LE.1) RETURN
      DO 280 I=1,2
      IF(XYMM(1,I).EQ.UNDEF%X.OR.XYMM(2,I).EQ.UNDEF%X) THEN
        XYM(1,I)=1D60
        XYM(2,I)=-1D60
        DO 240 N=1,NN
          XYM(1,I)=MIN(XYM(1,I),WORK(I,N))
          XYM(2,I)=MAX(XYM(2,I),WORK(I,N))
 240    CONTINUE
        IF(XYM(1,I).EQ.XYM(2,I)) THEN
          IF(XYM(1,I).EQ.0) THEN
            XYM(1,I)=-1
            XYM(2,I)=1
          ELSE
            XYM(1,I)=XYM(1,I)-0.5*ABS(XYM(1,I))
            XYM(2,I)=XYM(2,I)+0.5*ABS(XYM(2,I))
          ENDIF
        ELSE
          XY1=XYM(2,I)-XYM(1,I)
          XYM(1,I)=XYM(1,I)-0.05*XY1
          XYM(2,I)=XYM(2,I)+0.05*XY1
        ENDIF
        IF(LOGHV(I).GE.1) THEN
          IF(XYM(2,I).LE.0) GOTO 940
          XYM(1,I)=MAX(XYM(1,I),XYM(2,I)/1D3)
        ENDIF
        XYMM(1,I)=XYM(1,I)
        XYMM(2,I)=XYM(2,I)
      ELSE
        IF(LOGHV(I).GE.1.AND.
     %       (XYMM(1,I).LE.0.OR.XYMM(2,I).LE.0)) GOTO 930
        XYM(1,I)=XYMM(1,I)
        XYM(2,I)=XYMM(2,I)
      ENDIF
 280  CONTINUE
      CALL SCAT(FILE,NEW,NLMOD,SMOD,ICOLOR,NN,WORK,XYW,XYM,LOGHV,TTL,NC)
      RETURN
C
 900  WRITE(MSGFL,905)
 905  FORMAT(' (SUBE.PLFUNC) Non-positive value in log-scale ',
     %    'primary parameter.')
      GOTO 990
 910  WRITE(MSGFL,915) FUNX,TEXT(1:60)
 915  FORMAT(' (SUBR.PLFUNC) Invalid function "',A,'".',/,5X,A)
      GOTO 1000
 920  WRITE(MSGFL,925) FUNY,TEXT(1:60)
 925  FORMAT(' (SUBR.PLFUNC) Invalid function "',A,'".',/,5X,A)
      GOTO 1000
 930  WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.PLFUNC) Nonpositive axis range for log scale ',
     %   'scatter plot.')
      GOTO 1000
 940  WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.PLFUNC) All data nonpositive for log scale ',
     %   'scatter plot.')
 990  IRTN=990
      RETURN
1000	IRTN=1000
	IF(IDLM(2).GE.1) CALL EVLMFREE(IDLM(2))
	IF(IDLM(2).GE.1) CALL EVLMFREE(IDLM(2))
	RETURN
      END
