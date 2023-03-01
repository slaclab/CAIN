	SUBROUTINE ALLOCMEM(K,IRTN)
C  K=0: initialize,  K=1: allocate,  K=2: deallocate
      use NLCPLPTAB
	IMPLICIT NONE
	INTEGER K,IRTN
	INCLUDE 'include/alloccm.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER ISTAT

	IF(K.EQ.0) THEN
C          called from initlz.f
	  MP1=MPDFT
	  MVPH1=MVPHDFT
	  MPNEW1=0
	  MMAG1=MMAGDFT
	  MBEAMLINE1=MBEAMLINEDFT
	  MBBXY1=MBBXYDFT
	  MLUMMESH1=MLUMMESHDFT
	  IRTN=0
	  IALLOC=0
	  IRTN=0
	  RETURN
	ELSEIF(K.EQ.2) THEN
C          called from stopcain.f
	  IF(IALLOC.GE.1) THEN
	    CALL BEAMDEALLOC
	    CALL BLDEALLOC
	    CALL BBDEALLOC
	    CALL LUMDEALLOC
	    CALL DEALLOCNLCPLST
	  ENDIF
	  IRTN=0
	  RETURN
	ENDIF
C          called from aamain.f or rdalloc.f
	IF(IALLOC.NE.0) GOTO 920
	IALLOC=1
C       note IALLOC must be set even if some allocations fail.
	CALL BEAMALLOC(MP1,MVPH1,MPNEW1,ISTAT)
	CALL BLALLOC(MMAG1,MBEAMLINE1,ISTAT)
	CALL BBALLOC(MBBXY1,ISTAT)
	CALL LUMALLOC(MLUMMESH1,ISTAT)
	IF(ISTAT.NE.0) GOTO 900
	IF(MSGLVL.GE.1) THEN
	  WRITE(MSGFL,300)
300     FORMAT(' --- Memory allocation ---')
	  WRITE(MSGFL,310) MP1,MPNEW1,MVPH1,MMAG1,MBEAMLINE1,MBBXY1,
     %     MLUMMESH1
310     FORMAT(' beamcm.f',T15,'MP=',I7,'  MPNEW=',I7,'  MVPH=',I7,/,
     %         ' beamln.f',T15,'MMAG=',I7,'  MBEAMLINE=',I7,/,
     %         ' bbcom.f & bbpkcm.f', T23,'MBBXY=',I7,/,
     %         ' lumcom.f',T15,'MDD=',I7)
      ENDIF
	IRTN=0
	RETURN
900	IRTN=102
	WRITE(MSGFL,910) MP1,MPNEW1,MVPH1,MMAG1,MBEAMLINE1
910	FORMAT(' *** Fatal error: Memory allocation failed.',/,
     %  '  MP=',I8,'  MPNEW=',I7,'  MVPH=',I7,/,
     %  '  MMAG=',I8,'  MBEAMLINE=',I8)
	GOTO 990
920	IRTN=104
	WRITE(MSGFL,930)
930   FORMAT(' *** ALLOCATE called twice.')
	GOTO 990
990   RETURN
	END
