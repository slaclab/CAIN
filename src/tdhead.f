      SUBROUTINE TDHEAD(FILE)
      IMPLICIT NONE
      INTEGER FILE
      INCLUDE 'include/headcm.h'
      CHARACTER*120 TTL(2)
      INTEGER NC(2),NC1
C
      WRITE(FILE,200) JOBTIM,VERSION
 200  FORMAT(' TITLE 10.00 9.80 SIZE 1.3 ',2H'',/,
     %  ' MORE ',1H',A,1X,A,1H')
      IF(JOBTTL.NE.' ') THEN
        TTL(2)=' '
        CALL RDTTL(JOBTTL,2,TTL,NC,';')
        NC1=MIN(NC(1),60)
        IF(NC1.GE.1) THEN
          WRITE(FILE,220) TTL(1)(1:NC1)
 220      FORMAT(' TITLE 0.20 9.80 SIZE 1.3 ',2H'',/,
     %     ' MORE ',1H',A,1H')
          IF(NC(2).GE.1) THEN
            WRITE(FILE,240) TTL(2)(1:NC1)
 240        FORMAT(' CASE ',1H',A,1H')
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END

