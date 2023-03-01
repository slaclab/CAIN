	SUBROUTINE LSRWARN(IRTN)
C   Warning about consistency between LASER and LASERQED.
	USE LASRDATA
	IMPLICIT NONE
	INTEGER IRTN
	INCLUDE 'include/lasrcm.h'
	INCLUDE 'include/nllsrcm.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER LSR
	
	IRTN=0
	IF(NLSR.LE.0) RETURN
	IF(NPHCP.LT.0.AND.NPHBW.LT.0) THEN
	  WRITE(MSGFL,180)
180     FORMAT(' *** Warning ***',/,'    LASER defined but LASERQED ',
     %    ' not defined yet.')
	  IRTN=1
	  RETURN
	ENDIF
	DO 300 LSR=1,NLSR
	  IF(NPHCP.GE.1) THEN
	    IF(LSRQEDPOL(1).EQ.1) THEN
	      IF(STKSLS(1,LSR).NE.0.OR.STKSLS(3,LSR).NE.0.OR.
     %         ABS(STKSLS(2,LSR)).LE.0.9999D0) THEN
	        IRTN=1
              WRITE(MSGFL,200)
200           FORMAT(' *** Warning ***',/,'    Non-linear Compton ',
     %         /,'  is ready for 100% circularly polarized laser only.')
	      ENDIF
	    ELSE
	      IF(STKSLS(2,LSR).NE.0.OR.
     %        STKSLS(1,LSR)**2+STKSLS(3,LSR)**2.LE.0.9998D0) THEN
	        IRTN=1
              WRITE(MSGFL,210)
210           FORMAT(' *** Warning ***',/,'    Non-linear Compton ',
     %         /,'  is ready for 100% linearly polarized laser only.')
	      ENDIF
	    ENDIF
	    IF(TDL(1,LSR).GE.1.0001D0.OR.TDL(2,LSR).GE.1.0001D0) THEN
            WRITE(MSGFL,220)
	      IRTN=1
220         FORMAT(' *** Warning ***',/,'    Non-linear QED ready for ',
     %        'diffraction limit laser (TDL=1) only.')
	    ENDIF
	  ENDIF
	  IF(NPHBW.GE.1) THEN
	    IF(STKSLS(1,LSR).NE.0.OR.STKSLS(3,LSR).NE.0.OR.
     %       ABS(STKSLS(2,LSR)).LE.0.9999D0) THEN
	      IRTN=1
            WRITE(MSGFL,240)
240         FORMAT(' *** Warning ***',/,'    Non-linear Breit-Wheeler ',
     %        /,'   is ready for 100% circularly polarized laser only.')
	    ENDIF
	    IF(TDL(1,LSR).GE.1.0001D0.OR.TDL(2,LSR).GE.1.0001D0) THEN
            WRITE(MSGFL,260)
	      IRTN=1
260         FORMAT(' *** Warning ***',/,'    Non-linear QED ready for ',
     %        'diffraction limit laser (TDL=1) only.')
	    ENDIF
	  ENDIF
300   CONTINUE
      RETURN
	END