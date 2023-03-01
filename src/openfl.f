      SUBROUTINE OPENFL(FILE,FILENAME,STATUS,LAPPEND,NC,IRTN)
C  Open the file FILENAME
C  On return the string length of FILENAME with apostrophes taken off
C  is stored in NC.
C  A temp file will be opened if FILENAME=' '
      IMPLICIT NONE
      CHARACTER*(*) FILENAME,STATUS
      INTEGER FILE,LAPPEND,NC,IRTN
      INCLUDE 'include/ctrlcm.h'
      INTEGER IOS
      CHARACTER*10 ACCESS
C
      IF(FILE.LE.0.OR.FILE.GE.100) GOTO 910
      ACCESS='SEQUENTIAL'
      IF(LAPPEND.NE.0) ACCESS='APPEND'
      IF(FILENAME.EQ.' ') THEN
        OPEN(FILE,ACCESS=ACCESS,STATUS=STATUS,IOSTAT=IOS)
        IF(IRTN.NE.0) GOTO 920
      ELSE
        CALL SPCOFF(FILENAME,3,NC)
        OPEN(FILE,FILE=FILENAME(1:NC),ACCESS=ACCESS,STATUS=STATUS,
     %       IOSTAT=IOS)
        IF(IOS.NE.0) GOTO 930
      ENDIF
      IRTN=0
      RETURN
 910  IRTN=1001
      IF(MSGLVL.GE.0) WRITE(MSGFL,915) FILE
 915  FORMAT(' (SUBR.OPEN) Invalid file reference number ',I5)
      RETURN
 920  IRTN=1002
      IF(MSGLVL.GE.0) WRITE(MSGFL,925) FILE,ACCESS,STATUS,IOS
 925  FORMAT(' (SUBR.OPENFL) File open error.',/,
     %  '  OPEN(',I2,' ACCESS=',',STATUS=',A,',IOSTAT=IOS)',/,
     %  '  Return code IOS=',I5)
      RETURN
 930  IRTN=1003
      IF(MSGLVL.GE.0) WRITE(MSGFL,935) FILE,FILENAME(1:NC),
     %      STATUS,IOS
 935  FORMAT(' (SUBR.OPENFL) File open error.',/,
     %  '  OPEN(',I2,',FILE=''',A,''',',/,
     %  '       STATUS=',A,/,
     %  '       IOSTAT=IOS)',/,
     %  '  Return code IOS=',I5)
      RETURN
      END
