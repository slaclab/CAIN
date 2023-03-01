      SUBROUTINE EVINQ(FLG,ID,NAM,VAL,IRTN)
C  Inquire number of defined variables, their names and values.
C   FLG=1:  inquire total number of defined variables
C      Output:  ID    Total number
C   FLG=2:  inquire ID-th variable
C      Input:   ID    variable id.
C      Output:  NAM   name
C               VAL   value (REAL*8)
C               IRTN  1:  ID <=0 or > (total number)
C                     2:  character length of NAM is too short
C   FLG=3:  inquire variable by the name
C      Input:   NAM   variable name
C      Output:  ID
C               VAL
C               IRTN  variable undefined
C
      IMPLICIT NONE
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INTEGER FLG,ID,IRTN
      CHARACTER*(*) NAM
      REAL*8 VAL
      INTEGER NC,I
C
      IF(FLG.LE.0.OR.FLG.GE.4) GOTO 900
      IRTN=0
      IF(FLG.EQ.1) THEN
        ID=NPAR
      ELSEIF(FLG.EQ.2) THEN
        IF(ID.LE.0.OR.ID.GT.NPAR) THEN
          IRTN=1
        ELSE
          NC=LEN(NAM)
          IF(NC.LT.MCHAR) THEN
            DO 200 I=NC+1,MCHAR
              IF(NAMPAR(ID)(I:I).NE.' ') THEN
                IRTN=2
                RETURN
              ENDIF
 200        CONTINUE
          ENDIF
          NAM=NAMPAR(ID)
          VAL=VPAR(ID)
        ENDIF
      ELSE
        DO 240 I=1,NPAR
          IF(NAM.EQ.NAMPAR(I)) THEN
            ID=I
            VAL=VPAR(I)
            RETURN
          ENDIF
 240    CONTINUE
        IRTN=1
      ENDIF
      RETURN
 900  IRTN=1000
      RETURN
      END
