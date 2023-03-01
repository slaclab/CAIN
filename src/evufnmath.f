	RECURSIVE SUBROUTINE EVUFNMATH(FNAME,K1,X,NV,Y,
     %         IRTN,IERARG,ERRMSG)
C  Special functions
C        'BesJ','DBesJ',
C        'BesK','DBesK','BesK13','BesK23','BesKi13','BesKi53',
C        'FuncBS','FuncCP','IntFCP'
C  IRTN=0  normal
C       1  warning (ERRMSG)
C     101  Wrong number of arguments
C     102  Range of the IERARG-th argument invalid
C     103  Type (float/char) of the IERARG-th argument mismatch
C     200  Others. (ERRMSG)
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER K1,NV,IRTN,IERARG
      TYPE(FLCHTYPE) Y,X(NV)
      CHARACTER*(*) FNAME,ERRMSG
	INTEGER I,N
	REAL(8) X1,X2,DUMMY
	REAL(8) BESK,BK13,BK23,BKI13,BKI53,WCOHP
	EXTERNAL BESJDJ
      INTEGER LIBRTN,LIBMSGLVL
      CHARACTER*80 LIBMSG
      COMMON/LIBCOM/LIBRTN,LIBMSGLVL
      COMMON/LIBCOM2/LIBMSG

      GOTO (510, 510, 520, 520,   530,   530,   530,    530,
C          BesJ DBesJ BesK DBesK BesK13 BesK23 BesKi13 BesKi53 
     %     550,  560,  580), K1
C        FuncBS FuncCP IntFCP
      GOTO 800
 510  DO I=1,2
        IF(X(I)%L.NE.1) THEN
	    IERARG=I
	    IRTN=103
	    RETURN
	  ENDIF
	ENDDO
	N=NINT(X(1)%X)
      IF(ABS(N-X(1)%X).GE.1D-10) GOTO 900
      IF(FNAME.EQ.'BesJ') THEN
        CALL BESJDJ(N,X(2)%X,Y%X,DUMMY)
      ELSE
        CALL BESJDJ(N,X(2)%X,DUMMY,Y%X)
      ENDIF
      GOTO 800
 520  IF(NV.LE.1.OR.NV.GE.4) GOTO 910
      IF(NV.EQ.3) THEN
	  IF(X(3)%L.NE.1) THEN
	    IERARG=3
	    IRTN=103
	    RETURN
	  ENDIF
        N=NINT(X(3)%X)
        IF(N.LE.0.OR.N.GE.3) GOTO 920
      ELSE
        N=1
      ENDIF
      LIBMSGLVL=-1
      IF(FNAME.EQ.'BesK') THEN
        Y%X=BESK(X(1)%X,X(2)%X,N)
      ELSE
        Y%X=BESK(X(1)%X-1,X(2)%X,N)
        IF(LIBRTN.LT.100) THEN
          Y%X=-0.5*(Y%X+BESK(X(1)%X+1,X(2)%X,N))
        ENDIF
      ENDIF
      IF(LIBRTN.GE.100) THEN
        ERRMSG=LIBMSG
        GOTO 990
      ENDIF
      GOTO 800
 530  IF(NV.LE.0.OR.NV.GE.3) GOTO 930
      IF(NV.EQ.1) THEN
	  IF(X(1)%L.NE.1) THEN
	    IERARG=1
	    IRTN=103
	    RETURN
	  ENDIF
        N=1
      ELSE
	  DO I=1,2
          IF(X(I)%L.NE.1) THEN
	      IERARG=I
	      IRTN=103
	      RETURN
	    ENDIF
	  ENDDO
        N=NINT(X(2)%X)
        IF(N.NE.1.AND.N.NE.2) GOTO 950
      ENDIF
      IF(FNAME.EQ.'BesK13') THEN
        Y%X=BK13(X(1)%X,N)
      ELSEIF(FNAME.EQ.'BesK23') THEN
        Y%X=BK23(X(1)%X,N)
      ELSEIF(FNAME.EQ.'BesKi13') THEN
        Y%X=BKI13(X(1)%X,N)
      ELSEIF(FNAME.EQ.'BesKi53') THEN
        Y%X=BKI53(X(1)%X,N)
      ENDIF
      GOTO 800
 550  DO I=1,2
        IF(X(I)%L.NE.1) THEN
	    IERARG=I
	    IRTN=103
	    RETURN
	  ENDIF
	ENDDO
	IF(X(1)%X.LE.0.OR.X(1)%X.GT.1.OR.X(2)%X.LE.0) GOTO 940
      IF(X(1)%X.EQ.1) THEN
        Y%X=0
      ELSE
        X1=1-X(1)%X
        X2=X(1)%X/X1/(1.5D0*X(2)%X)
        IF(X2.GE.160) THEN
          Y%X=0
        ELSE
          Y%X=EXP(-X2)*(BKI53(X2,2)+X(1)%X**2/X1*BK23(X2,2))
        ENDIF
      ENDIF
      GOTO 800
 560  IF(X(1)%L.NE.1.OR.X(2)%L.NE.1) GOTO 930
	IF(X(1)%X.LT.0.OR.X(1)%X.GT.1.OR.X(2)%X.LT.0) GOTO 960
      IF(X(1)%X.EQ.0.OR.X(1)%X.EQ.1.OR.X(2)%X.EQ.0) THEN
        Y%X=0
      ELSE
        X1=1-X(1)%X
        X2=1/(X(1)%X*X1*1.5D0*X(2)%X)
        IF(X2.GE.160) THEN
          Y%X=0
        ELSE
          Y%X=EXP(-X2)*(BKI13(X2,2)+(X(1)%X/X1+X1/X(1)%X)*BK23(X2,2))
        ENDIF
      ENDIF
      GOTO 800
 580  IF(NV.LE.0.OR.NV.GE.3) GOTO 970
	IF(X(1)%L.NE.1) THEN
	  IERARG=1
	  IRTN=103
	  RETURN
	ENDIF
      IF(NV.EQ.2) THEN
	  IF(X(2)%L.NE.1) THEN
	    IERARG=2
	    IRTN=103
	    RETURN
	  ENDIF
        N=NINT(X(2)%X)
        IF(N.LE.0.OR.N.GE.3) GOTO 972
      ELSE
        N=1
      ENDIF
      IF(X(1)%X.LT.0) GOTO 974
      Y%X=WCOHP(X(1)%X,N)
	GOTO 800

800   IRTN=0
	RETURN

900	ERRMSG='1st arg. of BesJ/DBesJ must be an integer.'
      GOTO 990
910   ERRMSG='BesK/DBesK must have 2 or 3 args.'
      GOTO 990
920	ERRMSG='3rd arg of BesK/DBesK must be 1 or 2.'
      GOTO 990
930   ERRMSG='BesK13/BesK23/BesKi13/BesKi53 must have 1 or 2 args.'
      GOTO 990
940	ERRMSG='Wrong arg of FuncBS'
      GOTO 990
950	ERRMSG='2nd arg of BesK13/BesK23/BesKi13/BesKi53 must be'//
     %             ' 1 or 2.'
      GOTO 990
960	ERRMSG='Wrong arg of FuncCP'
      GOTO 990
970   ERRMSG='IntFCP must have 1 or 2 args.'
      GOTO 990
972	ERRMSG='2nd arg of IntFCP must be 1 or 2.'
      GOTO 990
974	ERRMSG='1st arg of IntFCP must be >0.'
      GOTO 990
990   IRTN=200
	RETURN
	END
