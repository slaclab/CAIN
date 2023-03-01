	RECURSIVE SUBROUTINE EVUFNBS(FNAME,K,X,NV,GSTRX,Y,
     %        IRTN,IERARG,ERRMSG)
C--Beam statistics functions
C        'NParticle','NMacro',
C        'AvrT','AvrX','AvrY','AvrS',
C        'AvrEn','AvrPx','AvrPy','AvrPs',
C        'AvrSx','AvrSy','AvrSs','AvrXi1','AvrXi2','AvrXi3',
C        'SigT','SigX','SigY','SigS',
C        'SigEn','SigPx','SigPy','SigPs',
C        'SigSx','SigSy','SigSs','SigXi1','SigXi2','SigXi3',
C        'BeamMatrix'
C  IRTN=0  normal
C       1  warning (ERRMSG)
C     101  Wrong number of arguments
C     102  Range of the IERARG-th argument invalid
C     103  Type (float/char) of the IERARG-th argument mismatch
C     200  Others. (ERRMSG)
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER K,NV,IRTN,IERARG
      TYPE(FLCHTYPE) Y,X(NV)
      CHARACTER*(*) FNAME,GSTRX,ERRMSG
	INTEGER NV1,LR,KIN,K1,K2,K1B,I
	REAL(8) BMFUNC
C--Beam statistics functions
	IRTN=0
      NV1=0
      IF(FNAME.EQ.'BeamMatrix') NV1=2   ! (BeamMatrix)
      IF(NV.LE.NV1+1.OR.NV.GE.NV1+4) THEN
	  IRTN=101
	  RETURN
	ENDIF
	DO I=1,2
	  IF(X(NV1+I)%L.NE.1) THEN
	    IERARG=NV1+I
	    IRTN=103
	    RETURN
	  ENDIF
	ENDDO
C  right-left
      LR=NINT(X(NV1+1)%X)
      IF(LR.LE.0.OR.LR.GE.4) THEN
        IERARG=NV1+1
        IRTN=102
	  RETURN
      ENDIF
C  photon-electron-positron
      KIN=NINT(X(NV1+2)%X)
      IF(KIN.LE.0.OR.KIN.GE.4) THEN
        IERARG=NV1+2
        IRTN=102
	  RETURN
      ENDIF
      K1B=0
      IF(NV1.NE.0) THEN
	  DO I=1,2
	    IF(X(I)%L.NE.1) THEN
	      IERARG=I
	      IRTN=103
	      RETURN
	    ENDIF
	  ENDDO
        K1=NINT(X(1)%X)
        K1B=NINT(X(2)%X)
        K2=3
        IF(K1.LE.0.OR.K1.GE.9) THEN
          IERARG=1
          IRTN=102
	    RETURN
        ENDIF
        IF(K1B.LE.0.OR.K1B.GE.9) THEN
          IERARG=2
          IRTN=102
	    RETURN
        ENDIF
      ELSEIF(K.LE.2) THEN
        K1=0
        K2=K
      ELSE
C          14 is the number of Avr's
        K1=MOD(K-3,14)+1
        K2=(K-3)/14+1
      ENDIF
C         K1=0: number of particles, 1: Avr, 2: Sig
      IF(NV.EQ.NV1+2) THEN
        Y%X=BMFUNC(LR,KIN,K1,K1B,K2,' ',IRTN)
      ELSE
c        IF(SMESH.LE.0) THEN
c	    ERRMSG=FNAME//
c     %    ' cannot be evaluated because Smesh is undefined.'
c          GOTO 990
c	  ENDIF
        IF(X(3)%L.NE.2) THEN
	    IERARG=3
	    IRTN=103
	    RETURN
	  ENDIF
        Y%X=BMFUNC(LR,KIN,K1,K1B,K2,GSTRX(X(3)%C(1):X(3)%C(2)),IRTN)
      ENDIF
      IF(IRTN.NE.0) THEN
	  IF(FNAME.NE.'NParticle'.AND.FNAME.NE.'NMacro') THEN
	    ERRMSG='Warning: No particles for function '//FNAME
     %                      //' Value 0 returned.'
	    IRTN=1
	  ENDIF
	ENDIF
	RETURN
	END