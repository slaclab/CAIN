      SUBROUTINE BBFLD(T,IS,SS,IRTN)
	USE BEAMCM
	USE BBCOM
      IMPLICIT NONE
      INTEGER IS,IRTN
      REAL*8 T,SS(2)
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/ctrlcm.h'
      include 'include/pushcm.h'
      INTEGER I
      INTEGER NCALL/0/
C
      CALL CPUTIM('BBFLD',1)
      NCALL=NCALL+1
      CALL BBMESH(T,IS,IRTN)
      IF(IRTN.NE.0) GOTO 1000
C       (IRTN=1: bb-field need not be calculated)
c      write(msgfl,103) it,is,ss(1),ss(2),0.5*sum(ss),t
 103  format(' bbfld about to call bbbin  it,is= ',i5,i3
     &    ' ss= ',2(1pd11.3),' <ss>= ',(1pd11.3),' t= ',(1pd11.3))
      CALL BBBIN(T,IS,ss)
      CALL BBFKER(NXY,BBDXY,XYMIN,NMOM,LBBEL,BBR00,BBEL,BBU00,IRTN)
      IF(IRTN.NE.0) GOTO 900
      CALL BBFPSN(NXY(1),NXY(2),BBQ)
c      print *, " bbfld nbbpl= ",nbbpl,
c     &            " ss= ",ss," t= ",t
      IF(NBBPL.GE.1) THEN
        DO 220 I=1,NBBPL
          IF(SBBPL(I).GE.SS(1).AND.SBBPL(I).LT.SS(2)) THEN
c             print *, " bbfld i= ",i," sbbpl(i)= ",sbbpl(i),
c     &            " ss= ",ss," t= ",t
             CALL PLBBFL(T,SS,IS)
            GOTO 400
          ENDIF
 220    CONTINUE
      ENDIF
 400  IRTN=0
      GOTO 1000
C
 900  IF(IRTN.EQ.1000) THEN
        WRITE(MSGFL,902)
 902    FORMAT(' (SUBR.BBFLD) BBFKER failed because of too large ',
     %    'dimension.')
      ELSEIF(IRTN.EQ.1001) THEN
        WRITE(MSGFL,904)
 904    FORMAT(' (SUBR.BBFLD) BBFKER failed because bin y/x ratio ',
     %    'is different in beam#1 and #2.')
      ENDIF
      GOTO 1000
 1000 CALL CPUTIM('BBFLD',2)
      RETURN
      END
