      SUBROUTINE PRCMD(IC,CMDLN,NCMD,LINE,NCHAR,NCHAR2)
C  Print echo for IC-th command
C  including the preceeding (whole) comment lines
C  and following comments within the line.
      IMPLICIT NONE
      INTEGER IC,NCMD,CMDLN(2,4,NCMD),NCHAR(*),NCHAR2(*)
      CHARACTER*(*) LINE(*)
      INTEGER L1,N1,L2,N2,L,NN1,NN2
      INCLUDE 'include/ctrlcm.h'
C  Possible preceeding comment lines.
      L1=CMDLN(1,1,IC)
      N1=CMDLN(2,1,IC)
 180  IF(L1.NE.1) THEN
        IF(NCHAR(L1-1).EQ.0) THEN
          IF(IC.NE.1) THEN
            IF(CMDLN(1,4,IC-1).GE.L1-1) GOTO 200
          ENDIF
          L1=L1-1
          N1=1
          GOTO 180
        ENDIF
      ENDIF
C  Possible following comments
 200  L2=CMDLN(1,4,IC)
      N2=CMDLN(2,4,IC)
      IF(IC.NE.NCMD) THEN
        IF(CMDLN(1,1,IC+1).EQ.L2) GOTO 220
      ENDIF
      N2=NCHAR2(L2)
 220  DO 260 L=L1,L2
        IF(L.EQ.L1) THEN
          NN1=N1
          IF(L.EQ.L2) THEN
            NN2=N2
          ELSE
            NN2=NCHAR2(L)
          ENDIF
        ELSE
          NN1=1
          IF(L.EQ.L2) THEN
            NN2=N2
          ELSE
            NN2=NCHAR2(L)
          ENDIF
        ENDIF
        WRITE(MSGFL,'(A)') LINE(L)(NN1:NN2)
 260  CONTINUE
      RETURN
      END
