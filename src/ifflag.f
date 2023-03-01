      FUNCTION IFFLAG(FLG)
C  0: when flag FLG is OFF,  1: when ON,
C  -1:  when flag FLG does not exist.
      IMPLICIT NONE
      INTEGER IFFLAG
      CHARACTER*(*) FLG
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/ctrlcm2.h'
      INTEGER I
C
      DO 200 I=1,MFLAG
        IF(FLAGNM(2+I).EQ.FLG) THEN
          IFFLAG=IFLAG(I)
          RETURN
        ENDIF
 200  CONTINUE
      IFFLAG=-1
      RETURN
      END
