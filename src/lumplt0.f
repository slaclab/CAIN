      SUBROUTINE LUMPLT0(FILE,LOGV,IHEL,KIN,LUM,VLUM,NBN,BIN,
     %   DLUM,PERHVAR,ICOLOR,IRTN)
      IMPLICIT NONE
      INTEGER FILE,LOGV,IHEL,KIN(2),NBN,PERHVAR,ICOLOR,IRTN
      REAL*8 LUM,VLUM,BIN(0:NBN),DLUM(NBN)
	INCLUDE 'include/topdraw.h'
      INTEGER I,IM,IEL,IEL0,L,IUNITX
      REAL*8 FMIN,FMAX,FMAX1,F1,SUM,UNITX,UNITL,UNITL0
      CHARACTER*4 LKIN(3)/'G   ','e2-3','e2+3'/,
     %           LKINC(3)/'G   ',' X X',' X X'/
      INTEGER NCKIN(3)/1,4,4/
      CHARACTER*2 LHEL(4)/'++','-+','+-','--'/
      CHARACTER*3 LUNITX(5)/'eV ','keV','MeV','GeV','TeV'/
      CHARACTER*3 LX
C
      CALL FMAXBN(NBN,DLUM,IM,FMAX,I,FMAX1,PERHVAR,BIN)
      IRTN=1
      IF(FMAX.LE.0) RETURN
      IUNITX=MAX(1,MIN(5,INT(LOG10(BIN(NBN))/3)+1))
      UNITX=10D0**(3*(IUNITX-1))
      LX='bin'
      IF(PERHVAR.NE.0) LX=LUNITX(IUNITX)
      IF(PERHVAR.NE.0) THEN
        FMAX=FMAX*UNITX
        FMAX1=FMAX1*UNITX
      ENDIF
c      IF(LOGV.LE.0.AND.FMAX1.LE.FMAX/5) THEN
      IF(FMAX1.LE.FMAX/50000) THEN
        FMAX=FMAX1
      ELSE
        IM=0
      ENDIF
C        IEL: Power of 10 for vertical scale
C        IEL0:  for printing total luminosity
      IEL=INT(LOG10(FMAX))
      IF(LOGV.LE.0) THEN
        FMAX=1.05D0*FMAX
        FMIN=0
      ELSE
        FMAX=1.5D0*FMAX
        FMIN=1D60
c        FMIN=DLUM(1)
        IF(PERHVAR.NE.0) FMIN=DLUM(1)/(BIN(1)-BIN(0))
        DO 180 I=1,NBN
          F1=DLUM(I)
          IF(PERHVAR.NE.0) F1=F1/(BIN(I)-BIN(I-1))
          if(f1.gt.0.) FMIN=MIN(FMIN,F1)
 180    CONTINUE
c        FMIN=MAX(FMIN,FMAX/1D4)
        FMIN=FMIN/1.5
      ENDIF
      UNITL=10D0**IEL
      SUM=0
      DO 200 I=1,NBN
        SUM=SUM+DLUM(I)
 200  CONTINUE
      IEL0=0
      IF(SUM.GT.0) IEL0=INT(LOG10(SUM))
      UNITL0=10D0**IEL0
      WRITE(FILE,220)
 220  FORMAT(' NEWFRAME; SET FONT DUPLEX')
      CALL TDHEAD(FILE)
      print *, " lumplt0 point 001"
      WRITE(FILE,240) BIN(0)/UNITX,BIN(NBN)/UNITX,FMIN/UNITL,FMAX/UNITL,
     %  (LKIN(KIN(L))(1:NCKIN(KIN(L))),L=1,2),
     %  (LKINC(KIN(L))(1:NCKIN(KIN(L))),L=1,2),LUNITX(IUNITX),NBN
 240  FORMAT(
     %  ' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %  ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4,/,
     %  ' TITLE TOP SIZE 2.0 ',2H'',/,
     %  ' MORE ',1H','Luminosity Spectrum (',A,',',A,')',1H',/,
     %  ' CASE ',1H','                     ',A,' ',A,' ',1H',/,
     %  ' TITLE BOTTOM ',1H','W0cm1  (',A, ')',1H',/,
     %  '         CASE ',1H',' X  X   ',3X,' ',1H',/,
     %  ' TITLE 1.0 7.0 ANGLE 90 SIZE 1.4 ',1H','N0bin1=',I7,1H',/,
     %  '                            CASE ',1H',' X   X ',7X,1H')
      IF(IHEL.NE.0) THEN
        WRITE(FILE,260) LHEL(IHEL)
 260    FORMAT(' TITLE 9.0 9.3 SIZE 1.6 ',2H'',/,
     %    ' MORE ',1H','Helicity (',A,')',1H')
      ENDIF
      IF(IEL.EQ.0) THEN
        WRITE(FILE,270) LX
 270    FORMAT(' TITLE LEFT SIZE 1.8 ',2H'',/,
     %  ' MORE ',1H','dL/dW  (/cm223/s/',A3,')',1H',/,
     %  ' CASE ',1H','           X X   ',3X,' ',1H')
      ELSE
        WRITE(FILE,280) IEL,LX
 280    FORMAT(' TITLE LEFT SIZE 1.8 ',2H'',/,
     %  ' MORE ',1H','dL/dW  (102',I2,'3 /cm223/s/',A3,')',1H',/,
     %  ' CASE ',1H','          X',2X,'X    X X   ',3X,' ',1H')
      ENDIF
      IF(LOGV.GE.1) WRITE(FILE,310)
 310  FORMAT(' SET SCALE Y LOG')
	WRITE(FILE,312) 'WHITE'
 312  FORMAT(' SET COLOR ',A)
	WRITE(FILE,314)
 314  FORMAT(' PLOT AXIS')
	IF(ICOLOR.GE.1.AND.ICOLOR.LE.MCOLR) THEN
	  WRITE(FILE,312) COLOR(ICOLOR)
	ENDIF
      IF(PERHVAR.EQ.0) THEN
        WRITE(FILE,320) (0.5*(BIN(I-1)+BIN(I))/UNITX,DLUM(I)/UNITL,
     %     I=1,NBN)
      ELSE
        WRITE(FILE,320) (0.5*(BIN(I-1)+BIN(I))/UNITX,
     %    DLUM(I)/(BIN(I)-BIN(I-1))*UNITX/UNITL,I=1,NBN)
      ENDIF
 320  FORMAT(3(1PD12.5,D13.5,';'))
      WRITE(FILE,340)
 340  FORMAT(' HISTOGRAM')
      WRITE(FILE,312) 'WHITE'
      IF(IHEL.EQ.0) THEN
        WRITE(FILE,400) LUM/UNITL0,SQRT(VLUM)/UNITL0,SUM/UNITL0,IEL0
 400    FORMAT(' TITLE 3.0 0.5 SIZE 1.4 ',2H'',/,
     %   ' MORE ',1H','Total luminosity',0PF9.3,
     %                       '+',0PF7.3,'(stat.1S) ',
     %                  1H',/,
     %   ' CASE ',1H','                ',9X, 
     %                       'M',7X,    '       G  ',
     %                  1H',/,
     %   ' MORE ',1H','  plotted range',0PF9.3,' X102',I2,'3/cm223/s',
     %                  1H',/,
     %   ' CASE ',1H','               ',9X,    ' M  X',2X,'X   X X  ',
     %                  1H')
      ELSE
        WRITE(FILE,410) LUM/UNITL0,SUM/UNITL0,IEL0
 410    FORMAT(' TITLE 3.0 0.5 SIZE 1.4 ',2H'',/,
     %   ' MORE ',1H','Total luminosity',0PF9.3,1H',/,
     %   ' CASE ',1H','                ',9X,    1H',/,
     %   ' MORE ',1H','  plotted range',0PF9.3,' X102',I2,'3/cm223/s',
     %                  1H',/,
     %   ' CASE ',1H','               ',9X,    ' M  X',2X,'X   X X  ',
     %                  1H')
      ENDIF
      IF(IM.NE.0) THEN
        IF(PERHVAR.EQ.0) THEN
          WRITE(FILE,420) BIN(IM-1)/UNITX,BIN(IM)/UNITX,
     %     DLUM(IM)/UNITL,IEL,LX
        ELSE
          WRITE(FILE,420) BIN(IM-1)/UNITX,BIN(IM)/UNITX,
     %     DLUM(IM)/(BIN(IM)-BIN(IM-1))*UNITX/UNITL,IEL,LX
        ENDIF
 420    FORMAT(' TITLE 3.0 0.85 SIZE 1.4 ',2H'',/,
     %   ' MORE ',1H','L(',0PF7.2,'<W<',0PF7.2,')=',0PF8.3,1H',/,
     %   ' CASE ',1H','  ',7X,    '   ',7X,    '  ',8X,1H',/,
     %   ' MORE ',1H','X102',I2,'3/cm223/s/',A3,1H',/,
     %   ' CASE ',1H','M  X',2X,'X   X X   ',3X,1H')
      ENDIF
	WRITE(FILE,314)
      IRTN=0
      RETURN
      END
