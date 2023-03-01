      SUBROUTINE BMFILE(NP1,FILE,LNAMEL,IRTN)
C  Readin beam file
C  File format
C   If LNAMEL=0,
C      use standard format defined by STDFMT in bmfmt.h
C   Otherwise,
C      use namelist format
C  Read stops when one of the following conditions is
C  satisfied:
C   (1)  Number of data = NP1   (if NP1>0)
C   (2)  When 3 characters 'END' from the head of a file
C        line is detected
C   (3)  End-of-file
C   (4)  Array storage limit. (error)
C  If the first character of a line is '!',
C  the line is ignored.
      IMPLICIT NONE
      INTEGER NP1,FILE,LNAMEL,IRTN
      INCLUDE 'include/bmfmt.h'
      INCLUDE 'include/ctrlcm.h'
      CHARACTER*(LENSTDFMT) TEXT
      INTEGER KIND,GEN
      LOGICAL END,SKIP
      REAL*8 WGT,TXYS(0:3),EP(0:3),SPIN(3)
      CHARACTER*4 PNAME
      NAMELIST/BEAMIN/KIND,GEN,PNAME,WGT,TXYS,EP,SPIN,END,SKIP
      INTEGER ISB/0/,LBBF/0/
      REAL*8 BFL(3,2)/0,0,0,0,0,0/
      INTEGER I,N0,NP,NC,IRTN1,LFMT
      INTEGER GETNP
      EXTERNAL GETNP
C
      CALL CPUTIM('BMFILE',1)
      LFMT=0
      N0=0
 200  IF(LNAMEL.EQ.0) THEN
        READ(FILE,'(A)',END=500) TEXT
        IF(TEXT(1:6).EQ.'!K   G') LFMT=0
        IF(TEXT(1:6).EQ.'!K G N') LFMT=1
        IF(TEXT(1:1).EQ.'!') GOTO 200
        IF(TEXT(1:2).EQ.'(*') GOTO 200
        IF(TEXT(1:3).EQ.'END') GOTO 500
        IF(TEXT(1:1).EQ.'{') THEN
          READ(TEXT,MATHFMT) KIND,GEN,PNAME,WGT,
     %      (TXYS(I),I=0,3),(EP(I),I=0,3),(SPIN(I),I=1,3)
        ELSE
          IF(LFMT.EQ.0) THEN
            READ(TEXT,STDFMT) KIND,GEN,PNAME,WGT,
     %        (TXYS(I),I=0,3),(EP(I),I=0,3),(SPIN(I),I=1,3)
          ELSE
            READ(TEXT,SHTFMT) KIND,GEN,PNAME,WGT,
     %        (TXYS(I),I=0,3),(EP(I),I=0,3),(SPIN(I),I=1,3)
          ENDIF
        ENDIF
	  IF(PNAME.NE.'    '.AND.PNAME(1:1).NE.'I'
     %    .AND.PNAME(1:1).NE.'T') GOTO 910
        SKIP=.FALSE.
      ELSE
        END=.FALSE.
        SKIP=.FALSE.
        DO 320 I=0,3
          TXYS(I)=0
          EP(I)=0
 320    CONTINUE
        EP(3)=1.0
        DO 330 I=1,3
          SPIN(I)=0
 330    CONTINUE
        WGT=0
        PNAME='    '
        KIND=2
        GEN=1
        READ(FILE,BEAMIN,END=500)
        IF(END) GOTO 500
      ENDIF
      IF(.NOT.SKIP) THEN
        IF(PNAME(1:1).EQ.'T') THEN
           CALL ADDTSTP(KIND,PNAME,TXYS,EP(1),SPIN,IRTN1)
           if(irtn1.ne.0) print *, " bmfile addstp irtn1= ", irtn1
        ELSE
          CALL ADDONE(0,KIND,GEN,PNAME,ISB,WGT,TXYS,EP,SPIN,
     %       LBBF,BFL,IRTN1)
           if(irtn1.ne.0) print *, " bmfile addone irtn1= ", irtn1
        ENDIF
        IF(IRTN1.NE.0) GOTO 900
        N0=N0+1
      ENDIF
      IF(NP1.LE.0.OR.N0.LT.NP1) GOTO 200
 500  IRTN=0
      NP=GETNP()
      IF(MSGLVL.GE.1) THEN
        WRITE(MSGFL,520) N0,NP
 520    FORMAT(' +++ Beam file.',I8,' particles read in.',/,
     %    '       Total number of stored particles =',I8)
        WRITE(MSGFL,540)
 540    FORMAT('   --- Statistics of the file data ---')
        CALL BMSTAT(NP-N0+1,NP,1,0,0,MSGFL)
        CALL BMSTAT(NP-N0+1,NP,1,1,0,MSGFL)
      ENDIF
      GOTO 1000
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.BMFILE) Storage limit when reading ',
     %   'a beam file.')
      GOTO 1000
 910  IRTN=1010
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.BMFILE) Invalid particle name in file.')
      GOTO 1000
 1000 CALL CPUTIM('BMFILE',2)
      RETURN
      END
