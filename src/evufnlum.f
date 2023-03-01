	RECURSIVE SUBROUTINE EVUFNLUM(FNAME,K1,X,NV,Y,
     %  IRTN,IERARG,ERRMSG)
C--Luminosity functions
C        'Lum','LumH','LumP',
C        'LumW','LumWbin','LumWbinEdge','LumWH','LumWP',
C        'LumEE','LumEEbin','LumEEbinEdge','LumEEH','LumEEP',
C  IRTN=0  normal
C       1  warning (ERRMSG)
C     101  Wrong number of arguments
C     102  Range of the IERARG-th argument invalid
C     103  Type (float/char) of the IERARG-th argument mismatch
C     200  Others. (ERRMSG)
	USE FLCHTYP
	USE LUMCOM
      IMPLICIT NONE
      INTEGER K1,NV,IRTN,IERARG
      TYPE(FLCHTYPE) Y,X(NV)
      CHARACTER*(*) FNAME,ERRMSG
	INCLUDE 'include/lumcom2.h'
	INTEGER I,IL,NX(6)
	REAL(8) WBIN
	CHARACTER(2) LL
      INTEGER NARG(13)/2,3,4,3,3,3,4,5,4,4,4,5,6/
C           same as in setcnst.f

      IF(NLUM.LE.0) GOTO 900
      DO I=1,NARG(K1)
	  IF(X(I)%L.NE.1) THEN
	    IERARG=I
	    IRTN=103
	    RETURN
	  ENDIF
        NX(I)=NINT(X(I)%X)
      ENDDO
      DO I=1,NLUM
        IL=I
        IF(KLUM(1,I).EQ.NX(1).AND.KLUM(2,I).EQ.NX(2)) GOTO 330
      ENDDO
	GOTO 932

 330  IF(FNAME.EQ.'Lum') THEN
        Y%X=LUM(0,IL)
      ELSEIF(FNAME.EQ.'LumH') THEN
        IF(NX(3).LT.0.OR.NX(3).GT.4) GOTO 924
        IF(LHEL(1,IL).LE.0) GOTO 912
        Y%X=LUM(NX(3),IL)
      ELSEIF(FNAME.EQ.'LumP') THEN
        IF(NX(3).LT.0.OR.NX(3).GT.3) GOTO 926
        IF(NX(4).LT.0.OR.NX(4).GT.3) GOTO 926
        IF(LHEL(2,IL).LE.0) GOTO 914
        Y%X=LUM(5+NX(3)+4*NX(4),IL)
      ELSEIF(K1.LT.9) THEN
C          9 corresponds to LumEE
        IF(NBNWLM(IL).LE.0) THEN
          LL=' W'
          GOTO 910
        ENDIF
        IF(FNAME.EQ.'LumW') THEN
          IF(NX(3).LE.0.OR.NX(3).GT.NBNWLM(IL)) GOTO 916
          Y%X=DLUM(IPWLUM(0,IL)+NX(3)-1)
        ELSEIF(FNAME.EQ.'LumWbin') THEN
          IF(NX(3).EQ.0) THEN
            Y%X=NBNWLM(IL)
          ELSE
            IF(NX(3).LT.0.OR.NX(3).GT.NBNWLM(IL)) GOTO 916
            IF(IPBINW(IL).EQ.0) THEN
              WBIN=(WMMLUM(2,IL)-WMMLUM(1,IL))/NBNWLM(IL)
              Y%X=WMMLUM(1,IL)+WBIN*(NX(3)-0.5D0)
            ELSE
              Y%X=(DLUM(IPBINW(IL)+NX(3)-1)
     %              +DLUM(IPBINW(IL)+NX(3)))/2
            ENDIF
          ENDIF
        ELSEIF(FNAME.EQ.'LumWbinEdge') THEN
          IF(NX(3).LT.0.OR.NX(3).GT.NBNWLM(IL)) GOTO 916
          IF(IPBINW(IL).EQ.0) THEN
            WBIN=(WMMLUM(2,IL)-WMMLUM(1,IL))/NBNWLM(IL)
            Y%X=WMMLUM(1,IL)+WBIN*NX(3)
          ELSE
            Y%X=DLUM(IPBINW(IL)+NX(3))
          ENDIF
        ELSEIF(FNAME.EQ.'LumWH') THEN
          IF(LHEL(1,IL).LE.0) GOTO 912
          IF(NX(3).LE.0.OR.NX(3).GT.NBNWLM(IL)) GOTO 916
          IF(NX(4).LT.0.OR.NX(4).GT.4) GOTO 924
          IF(NX(4).EQ.0) THEN
            Y%X=DLUM(IPWLUM(0,IL)+NX(3)-1)
          ELSE
            Y%X=DLUM(IPWLUM(1,IL)+(NX(4)-1)*NBNWLM(IL)+NX(3)-1)
          ENDIF
        ELSEIF(FNAME.EQ.'LumWP') THEN
          IF(LHEL(2,IL).LE.0) GOTO 914
          IF(NX(3).LE.0.OR.NX(3).GT.NBNWLM(IL)) GOTO 916
          IF(NX(4).LT.0.OR.NX(4).GT.3) GOTO 926
          IF(NX(5).LT.0.OR.NX(5).GT.3) GOTO 926
          Y%X=DLUM(IPWLUM(2,IL)+NX(3)-1+NBNWLM(IL)*(NX(4)+4*NX(5)))
        ENDIF
      ELSE
        IF(NBNELM(1,IL).LE.0) THEN
          LL='EE'
          GOTO 910
        ENDIF
        IF(FNAME.EQ.'LumEE') THEN
          IF(NX(3).LE.0.OR.NX(3).GT.NBNELM(1,IL).OR.
     %       NX(4).LE.0.OR.NX(4).GT.NBNELM(2,IL)) GOTO 920
          Y%X=DLUM(IPELUM(0,IL)+NX(3)-1+NBNELM(1,IL)*(NX(4)-1))
        ELSEIF(FNAME.EQ.'LumEEbin') THEN
          IF(NX(3).LE.0.OR.NX(3).GE.3) GOTO 948
          IF(NX(4).EQ.0) THEN
            Y%X=NBNELM(NX(3),IL)
          ELSE
            IF(NX(4).LT.0.OR.NX(4).GT.NBNELM(NX(3),IL)) GOTO 950
            IF(IPBINE(NX(3),IL).EQ.0) THEN
              WBIN=(EMMLUM(2,NX(3),IL)-EMMLUM(1,NX(3),IL))
     %             /NBNELM(NX(3),IL)
              Y%X=EMMLUM(1,NX(3),IL)+WBIN*(NX(4)-0.5D0)
            ELSE
              Y%X=(DLUM(IPBINE(NX(3),IL)+NX(4)-1)
     %              +DLUM(IPBINE(NX(3),IL)+NX(4)))/2
            ENDIF
          ENDIF
        ELSEIF(FNAME.EQ.'LumEEbinEdge') THEN
          IF(NX(3).LE.0.OR.NX(3).GE.3) GOTO 948
          IF(NX(4).LT.0.OR.NX(4).GT.NBNELM(NX(3),IL)) GOTO 950
          IF(IPBINE(NX(3),IL).EQ.0) THEN
            WBIN=(EMMLUM(2,NX(3),IL)-EMMLUM(1,NX(3),IL))
     %           /NBNELM(NX(3),IL)
            Y%X=EMMLUM(1,NX(3),IL)+WBIN*NX(4)
          ELSE
            Y%X=DLUM(IPBINE(NX(3),IL)+NX(4))
          ENDIF
        ELSEIF(FNAME.EQ.'LumEEH') THEN
          IF(LHEL(1,IL).LE.0) GOTO 912
          IF(NX(3).LE.0.OR.NX(3).GT.NBNELM(1,IL).OR.
     %       NX(4).LE.0.OR.NX(4).GT.NBNELM(2,IL)) GOTO 920
          IF(NX(5).LT.0.OR.NX(5).GT.4) GOTO 924
          IF(NX(5).EQ.0) THEN
            Y%X=DLUM(IPELUM(0,IL)+NX(3)-1
     %           +NBNELM(1,IL)*(NX(4)-1))
          ELSE
            Y%X=DLUM(IPELUM(1,IL)+NX(3)-1
     %           +NBNELM(1,IL)*(NX(4)-1+NBNELM(2,IL)*(NX(5)-1)))
          ENDIF
        ELSEIF(FNAME.EQ.'LumEEP') THEN
          IF(LHEL(2,IL).LE.0) GOTO 914
          IF(NX(3).LE.0.OR.NX(3).GT.NBNELM(1,IL).OR.
     %       NX(4).LE.0.OR.NX(4).GT.NBNELM(2,IL)) GOTO 920
          IF(NX(5).LT.0.OR.NX(5).GT.3) GOTO 926
          IF(NX(6).LT.0.OR.NX(6).GT.3) GOTO 926
          Y%X=DLUM(IPELUM(2,IL)+NX(3)-1
     %        +NBNELM(1,IL)*(NX(4)-1
     %        +NBNELM(2,IL)*(NX(5)+4*NX(6))))
        ENDIF
      ENDIF
	IRTN=0
	RETURN

 900	ERRMSG='Luminosity not defined'
      GOTO 990
 910  WRITE(ERRMSG,911) LL,NX(1),NX(2)
 911  FORMAT('Differential ',A,'-Luminosity(',I2,',',I2,') undefined.')
      GOTO 990
 912  WRITE(ERRMSG,913) NX(1),NX(2)
 913  FORMAT('Helicity Luminosity(',I2,',',I2,') undefined.')
      GOTO 990
 914  WRITE(ERRMSG,915) NX(1),NX(2)
 915  FORMAT('Polarization Luminosity(',I2,',',I2,') undefined.')
      GOTO 990
 916  WRITE(ERRMSG,917) NX(3),FNAME
 917  FORMAT('Bin#',I4,' is out of luminosity bin range in function ',A)
      GOTO 990
 920  WRITE(ERRMSG,921) NX(3),NX(4),FNAME
 921  FORMAT('Bin#(',2I4,') is out of luminosity bin range in ',
     % 'function ',A)
      GOTO 990
 924  ERRMSG='Invalid helicity argument for luminosity function '
     %       //FNAME
      GOTO 990
 926  ERRMSG='Invalid polarization argument for luminosity function '
     %       //FNAME
      GOTO 990
 932	WRITE(ERRMSG,933) NX(1),NX(2)
 933  FORMAT('Luminosity(',I2,',',I2,') undefined.')
	GOTO 990
 948  ERRMSG='3rd argument of LumEEbin/LumEEbinEdge must be 1 or 2'
      GOTO 990
 950  WRITE(ERRMSG,951) NX(4)
 951  FORMAT('Bin#',I4,' is out of luminosity bin range.')
      GOTO 990

 990  IRTN=200
	RETURN
	END
