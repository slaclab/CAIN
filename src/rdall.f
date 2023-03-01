      SUBROUTINE RDALL(LINE,MLINE,NLINE,NCHAR,NCHAR2,
     %   ICMD,MCMD,NCMD,CMDLN,IRTN)
C  Readin all the input data
C   *  All data after ! within the line is considered
C      to be a comment unless ! is within an apostrophy
C      pair.
C   *  Apostrophes have to close within a line.
C     (Change Sep.2001)  Aopstrophes can either be ' or ".
C        " or ' can freely be inserted in the pair of ' or " respectively.
C        The rule that, in a " ", "" is understood as one " still apllies
C        but this facxt can be ignored in this subroutine.
C   *  Data after the command END is not read in.
C
C   NLINE:  Number of lines stored.
C   LINE(L):  Contents of L-th line
C   NCHAR(L): number of characters in the data part
C             (Comment part excluded.)
C   NCHAR2(L): Number of characters (Comment part
C              included.)
C   NCMD:   Number of commands
C   ICMD(k):  Command id of k-th command
C   CMDLN(i,j,k):  line number (i=1) and column number (i=2)
C             of the start (j=1), start of operands (j=2),
C             end of body (j=3) and final ';' (j=4) 
C             of k-th command .
C
      IMPLICIT NONE
      INTEGER MLINE,NLINE,NCHAR(MLINE),NCHAR2(MLINE),
     %     MCMD,ICMD(MCMD),NCMD,CMDLN(2,4,MCMD),IRTN
      CHARACTER*(*) LINE(MLINE)
      INTEGER MCHAR,L,N,L0,N0,L1,N1,L2,N2,L3,N3,L4,N4,
     %   INAPOS,I,I1,I2,IC,EOF,IFCMD
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/cmdnam.h'
      CHARACTER*1 APOS(2)/"'",'"'/,CH
      CHARACTER*16 CMDNAM
C
      MCHAR=LEN(LINE(1))
      NLINE=0
      NCMD=0
      EOF=0
      L0=0
      N0=1
 100  READ(RDFL,'(A)',END=400) LINE(MLINE)
      DO 200 I=MCHAR,1,-1
        I2=I
        IF(LINE(MLINE)(I:I).NE.' ') GOTO 220
 200  CONTINUE
C        ignore blanck lines.
      GOTO 100
 220  IF(NLINE.GE.MLINE) GOTO 900
      NLINE=NLINE+1
      IF(NLINE.NE.MLINE) LINE(NLINE)=LINE(MLINE)
      NCHAR2(NLINE)=I2
      I1=0
      INAPOS=0
      DO 240 I=1,I2
        CH=LINE(NLINE)(I:I)
	  IF(INAPOS.EQ.0) THEN
	    IF(CH.EQ.APOS(1)) THEN
	      INAPOS=1
	    ELSEIF(CH.EQ.APOS(2)) THEN
	      INAPOS=2
	    ELSE
	      IF(CH.EQ.'!') GOTO 260
	    ENDIF
	  ELSE
	    IF(CH.EQ.APOS(INAPOS)) INAPOS=0    
	  ENDIF
c        IF(CH.EQ.APOS) THEN
c          INAPOS=1-INAPOS
c        ELSEIF(INAPOS.EQ.0) THEN
c          IF(CH.EQ.'!') GOTO 260
c        ENDIF
        IF(CH.NE.' ') I1=I
 240  CONTINUE
C 260  IF(INAPOS.NE.0) GOTO 910
C      NCHAR(NLINE)=I1
 260  NCHAR(NLINE)=I1
      IF(INAPOS.NE.0) GOTO 910
      IF(I1.EQ.0) GOTO 100
      IF(L0.EQ.0) THEN
        L0=NLINE
        N0=1
      ENDIF
 300  CALL FINDCH(L0,N0,NLINE,NCHAR(NLINE),LINE,NCHAR,';',
     %     L2,N2,IRTN)
      IF(IRTN.NE.0) GOTO 100
      L4=L2
      N4=N2
 310  L=L0
      N=N0
      I=0
      CMDNAM=' '
 320  L1=L
      N1=N
      IF(LINE(L)(N:N).EQ.';') THEN
        GOTO 360
      ELSEIF(LINE(L)(N:N).EQ.' ') THEN
        IF(I.NE.0) GOTO 360
      ELSE
        IF(I.GE.16) GOTO 920
        I=I+1
        CMDNAM(I:I)=LINE(L)(N:N)
        IF(I.EQ.1) THEN
          L0=L
          N0=N
        ENDIF
      ENDIF
      CALL NXTPOS(L,N,L2,N2,NCHAR,IRTN)
      IF(IRTN.EQ.0) GOTO 320
 360  L3=L2
      N3=N2
      IF(I.NE.0) THEN
        IC=IFCMD(CMDNAM(1:I),MMCMD,CMD)
        IF(IC.EQ.0) GOTO 920
        IF(IC.LT.0) GOTO 930
        IF(NCMD.GE.MCMD) GOTO 940
        NCMD=NCMD+1
        ICMD(NCMD)=IC
        CMDLN(1,1,NCMD)=L0
        CMDLN(2,1,NCMD)=N0
        IF(EOF.EQ.0) CALL PRVPOS(L2,N2,L0,N0,NCHAR,IRTN)
        CALL NXTPOS(L1,N1,NLINE,NCHAR(NLINE),NCHAR,IRTN)
        IF(IRTN.NE.0.OR.L1.GT.L2.OR.
     %       (L1.EQ.L2.AND.N1.GT.N2)) THEN
          L1=0
          N1=0
        ENDIF
        CMDLN(1,2,NCMD)=L1
        CMDLN(2,2,NCMD)=N1
        CMDLN(1,3,NCMD)=L2
        CMDLN(2,3,NCMD)=N2
        IF(L4.LT.L2.OR.(L4.EQ.L2.AND.N4.LT.N2)) THEN
          L4=L2
          N4=N2
        ENDIF
        CMDLN(1,4,NCMD)=L4
        CMDLN(2,4,NCMD)=N4
	  IF(OPACCEPT(IC).EQ.0.AND.L1.NE.0) GOTO 950
        IF(CMDNAM.EQ.'END') GOTO 500
      ENDIF
      IF(EOF.NE.0) GOTO 500
      L0=L3
      N0=N3
      CALL NXTPOS(L0,N0,NLINE,NCHAR(NLINE),NCHAR,IRTN)
      IF(IRTN.NE.0) THEN
        L0=0
        GOTO 100
      ENDIF
      GOTO 300
 400  EOF=1
      IF(L0.NE.0) THEN
        L2=NLINE
        N2=NCHAR(NLINE)
        IF(L0.LT.L2.OR.(L0.EQ.L2.AND.N0.LE.N2)) GOTO 310
      ENDIF
 500  CLOSE(RDFL)
	CALL CHECKNEST(NCMD,ICMD,CMDLN,LINE,IRTN)
	IF(IRTN.NE.0) GOTO 990
	IRTN=0
      RETURN

C
 900  IRTN=1000
      WRITE(MSGFL,905) MLINE
 905  FORMAT(' (SUBR.RDALL) Too many data lines. ',
     %   'Must be <=',I5,' lines.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) LINE(NLINE)(1:NCHAR(NLINE))
 915  FORMAT(' (SUBR.RDALL) Apostrophy does not ',
     %   'close within the line ',/,'"',A,'"')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925) CMDNAM(1:I) 
 925  FORMAT(' (SUBR.RDALL) Invalid command ',
     %   '"',A,'"')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935) CMDNAM(1:I) 
 935  FORMAT(' (SUBR.RDALL) Command name ',
     %   '"',A,'" ambiguous.')
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945) MCMD
 945  FORMAT(' (SUBR.RDALL) Too many commands. ',
     %   'Must be <=',I4)
      RETURN
 950  IRTN=1005
      WRITE(MSGFL,955) CMD(IC)(1:NCCMD(IC))
C       used to be CMD(ICMD(IC))(1:NCCMD(ICMD(IC)))
C       Corrected on Mar.13.2002
 955  FORMAT(' (SUBR.RDALL) Command ',A,' does not accept operands.',/,
     %   '     Missing semicolon?')
      RETURN

 990  IRTN=1009
	RETURN
      END
