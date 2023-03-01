	RECURSIVE SUBROUTINE EVUFNBL(FNAME,KK,K1,X,NV,GSTRX,Y,
     %           IRTN,IERARG,ERRMSG)
C  Beamline Functions
C     KK=1: 'Beta','Alpha','Eta','Etaprime','Nu'
C        Arguments for the above functions are (ixy,mag,bl)
C          ixy = 1 (x) or 2 (y)
C          mag = magnet id (integer) or manget name (character)
C          bl  = beamline name (character)
C            (mag and bl may be omitted in some cases)
C     KK=2: 'TMAT'
C        Argument  (i,j,mag,bl)
C           1<= i,j <=6
C           mag and bl are the same as above
C  IRTN=0  normal
C       1  warning (ERRMSG)
C     101  Wrong number of arguments
C     102  Range of the IERARG-th argument invalid
C     103  Type (float/char) of the IERARG-th argument mismatch
C     200  Others. (ERRMSG)
	USE FLCHTYP
	USE BEAMLN
	USE MATCHMOD
      IMPLICIT NONE
      INTEGER KK,K1,NV,IRTN,IERARG
      TYPE(FLCHTYPE) Y,X(NV)
      CHARACTER*(*) FNAME,GSTRX,ERRMSG
	INCLUDE 'include/transpcm.h'
	INTEGER IXY,ITW,IDMG,IDBL1,ITM(2),I,IM,IB
	INTEGER NV0(2)/1,2/   !   minimum number of arguments for KK=1,2
	REAL(8) APERT1(2)

	IF(NV.LT.NV0(KK).OR.NV.GT.NV0(KK)+2) THEN
	  IRTN=101
	  RETURN
	ENDIF
	DO I=1,NV0(KK)
	  IF(X(I)%L.NE.1) THEN
	    IERARG=I
	    IRTN=103
	    RETURN
	  ENDIF
	  IF(KK.EQ.1) THEN
	    IXY=NINT(X(I)%X)
	    IF(IXY.NE.1.AND.IXY.NE.2) THEN
	      IERARG=I
	      IRTN=102
	      RETURN
	    ENDIF
	    ITW=K1+5*(IXY-1)
	  ELSE
	    ITM(I)=NINT(X(I)%X)
	    IF(ITM(I).LT.1.OR.ITM(I).GT.6) THEN
	      IERARG=I
	      IRTN=102
	      RETURN
	    ENDIF
	  ENDIF
	ENDDO
	IM=NV0(KK)+1    !  arg. for magnet id/name
	IB=NV0(KK)+2    !  arg. for beamline name
	IF(NV.LE.NV0(KK)) THEN
	  IF(NSTEPTRANS.GE.0) THEN
		  IDBL1=IDBLTRANS
		ELSE
		  GOTO 952
	  ENDIF
	ELSEIF(NV.LE.IM) THEN
        IF(NSTEPTRANS.GE.0) THEN
	    IDBL1=IDBLTRANS
		ELSEIF(IDBLMATCH.GT.0) THEN
	    IDBL1=IDBLMATCH
	  ELSE
		  GOTO 953
	  ENDIF
	ELSE
	  IF(X(IB)%L.EQ.1) THEN
	    IDBL1=NINT(X(IB)%X)
	    IF(IDBL1.LE.0.OR.IDBL1.GT.NBEAMLINE) GOTO 954
	  ELSE
	    CALL STARTBL(GSTRX(X(IB)%C(1):X(IB)%C(2)),IDBL1,APERT1)
	    IF(IDBL1.LE.0) GOTO 955
	  ENDIF
	ENDIF
	IF(.NOT.BL(IDBL1)%LTWISS) GOTO 956
	IF(NV.GE.IM) THEN
	  IF(X(IM)%L.EQ.1) THEN
	    IDMG=MAX(0,MIN(BL(IDBL1)%NEXP,NINT(X(IM)%X)))
	  ELSE
	    CALL GETMAGID(GSTRX(X(IM)%C(1):X(IM)%C(2)),IDBL1,IDMG)
	    IF(IDMG.LE.0) GOTO 958
	  ENDIF
	ELSE
	  IDMG=NSTEPTRANS
	ENDIF
	IF(KK.EQ.1) THEN
	  Y%X=BL(IDBL1)%TWISS(ITW,IDMG)
	ELSE
	  Y%X=BL(IDBL1)%TMAT(ITM(1),ITM(2),IDMG)
	ENDIF
	IRTN=0
	RETURN

 952  ERRMSG='2nd arg of '//FNAME//' can be omitted '//
     %  'only in TRANSPORT loop'
	GOTO 990
 953  ERRMSG='3rd arg of '//FNAME//' can be omitted '//
     %  'only in TRANSPORT loop or MATCHING'
	GOTO 990
 954  ERRMSG='Wrong beamline ID number for '//FNAME
 955  ERRMSG='Beamline "'//GSTRX(X(IB)%C(1):X(IB)%C(2))//'" not fonud.'
	GOTO 990
 956  ERRMSG='Optics not ready for beamline "'
     %    //GSTRX(X(IB)%C(1):X(IB)%C(2))//'".'
	GOTO 990
 958  ERRMSG='Magnet '//GSTRX(X(IM)%C(1):X(IM)%C(2))//' not found '//
     %  'in the beamline '//GSTRX(X(IB)%C(1):X(IB)%C(2))//'.'
      GOTO 990
 990  IRTN=200
	RETURN
	END