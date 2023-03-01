      SUBROUTINE TSTPST(MSGFL)
	USE BEAMCM
      IMPLICIT NONE
      INTEGER MSGFL
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/tstpcm.h'
      INTEGER N,I
C
      IF(NTSTP.LE.0.OR.IWRNTS.NE.0) RETURN
      DO 180 N=1,NP
        IF(PNAME(N).EQ.'    '.OR.LOST(N).NE.0) GOTO 180
        IF(NTSTDT.EQ.MTSTP) THEN
          WRITE(MSGFL,120)
 120      FORMAT(' +++ Warning: Buffer for test particles full +++')
          IWRNTS=1
          RETURN
        ENDIF
        NTSTDT=NTSTDT+1
        TSTPNM2(NTSTDT)=PNAME(N)
        DO 140 I=0,3
          TSTPDT(I+1,NTSTDT)=TXYS(I,N)
          TSTPDT(I+5,NTSTDT)=EP(I,N)
 140    CONTINUE
        DO 150 I=1,3
          TSTPDT(I+8,NTSTDT)=SPIN(I,N)
 150    CONTINUE
 180  CONTINUE
      RETURN
      END
