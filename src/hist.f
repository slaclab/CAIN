      SUBROUTINE HIST(FILE,LNEW,LM,ICOLOR,NBN,XBIN,FBIN,LERR,ERR,
     %   XYW,XYMM,LOGHV,TTL,NC)
C  Plot TopDrawer histogram on file FILE.
C  LNEW: flag for new plot.
C  LM  : line mode
      IMPLICIT NONE
      INTEGER FILE,LNEW,LM,ICOLOR,NBN,LERR,LOGHV(2),NC(3)
      REAL*8 XBIN(0:NBN),FBIN(NBN),ERR(NBN),XYW(2,2),XYMM(2,2)
      CHARACTER*(*) TTL(2,3)
      REAL*8 CSIZE,UNITF,FM,XLGSCL
      REAL*8 XL,XH,YL,YH
      INTEGER I,NC1,NC2,NEX
      INCLUDE 'include/topdraw.h'
C
      XL=XYW(1,1)+1.5
      XH=XYW(2,1)-0.2
      YL=XYW(1,2)+1.5
      YH=XYW(2,2)-1.0
      UNITF=1
      NEX=0
      IF(LOGHV(2).LE.0) THEN
        FM=MAX(ABS(XYMM(1,2)),ABS(XYMM(2,2)))
        NEX=LOG10(FM)
        IF(ABS(NEX).GE.3) THEN
          UNITF=10D0**NEX
        ELSE
          NEX=0
        ENDIF
      ENDIF
      IF(LNEW.GE.1) THEN
        WRITE(FILE,200)
 200    FORMAT(' NEWFRAME; SET FONT DUPLEX')
        CALL TDHEAD(FILE)
        WRITE(FILE,300) XL,XH,YL,YH
 300    FORMAT(
     %       ' SET WINDOW X FROM ',F7.3,' TO ',F7.3,/,
     %       ' SET WINDOW Y FROM ',F7.3,' TO ',F7.3)
        WRITE(FILE,310) XYMM(1,1),XYMM(2,1),
     %        XYMM(1,2)/UNITF,XYMM(2,2)/UNITF
 310    FORMAT(' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %       ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4)
        IF(LOGHV(1).GE.1) WRITE(FILE,315) 'X'
        IF(LOGHV(2).GE.1) WRITE(FILE,315) 'Y'
 315    FORMAT(' SET SCALE ',A,' LOG')
      ENDIF
      IF(LERR.LE.0) THEN
        WRITE(FILE,320) ' '
      ELSE
        WRITE(FILE,320) 'DY'
      ENDIF
 320  FORMAT(' SET ORDER X Y ',A)
      IF(NEX.NE.0) WRITE(FILE,330) XL-0.8,YH,NEX
 330  FORMAT(' TITLE ',2F7.3,' SIZE 2.0 ',2H'',/,
     %  ' MORE ',1H','102',I2,'3',1H',/,
     %  ' CASE ',1H','  X',2X,'X',1H')
      DO 380 I=1,3
        IF(NC(I).LE.0) GOTO 380
        CSIZE=MIN(2.0D0,130D0/NC(I))
        IF(I.EQ.1) THEN
          WRITE(FILE,340) XYW(1,1)+1.0,XYW(2,2)-0.4,' ',CSIZE
 340      FORMAT(' TITLE ',2F7.3,1X,A,' SIZE ',F5.3,' ',2H'')
        ELSEIF(I.EQ.2) THEN
          WRITE(FILE,340) (XL+XH)/2,XYW(1,2)+0.4,'CENTER',CSIZE
        ELSE
          WRITE(FILE,340) XYW(1,1)+0.4,(YL+YH)/2,
     %          'CENTER ANGLE 90',CSIZE
        ENDIF
        NC2=0
 360    NC1=NC2+1
        NC2=MIN(NC(I),NC1+60)
        WRITE(FILE,370) TTL(1,I)(NC1:NC2),TTL(2,I)(NC1:NC2)
 370    FORMAT(' MORE ',1H',A,1H',/,' CASE ',1H',A,1H')
        IF(NC2.LT.NC(I)) GOTO 360
 380  CONTINUE
	WRITE(FILE,390) 'WHITE'
 390  FORMAT(' SET COLOR ',A)
      WRITE(FILE,395)
 395  FORMAT(' PLOT AXIS')
C
	IF(ICOLOR.GE.1.AND.ICOLOR.LE.MCOLR) THEN
	  WRITE(FILE,390) COLOR(ICOLOR)
	ENDIF
      WRITE(FILE,400) PATTRN(MOD(LM-1,MLMODE)+1)      
 400  FORMAT(' SET PATTERN ',A)
      IF(LERR.EQ.0) THEN
        WRITE(FILE,410) (XLGSCL(LOGHV(1),XBIN(I-1),XBIN(I)),
     %    FBIN(I)/UNITF,I=1,NBN)
 410    FORMAT(2(1PD11.4,D12.4,';'))
      ELSE
        WRITE(FILE,420) (XLGSCL(LOGHV(1),XBIN(I-1),XBIN(I)),
     %    FBIN(I)/UNITF,ERR(I)/UNITF,I=1,NBN)
 420    FORMAT(2(1PD11.4,2D12.4,';'))
      ENDIF
      WRITE(FILE,440)
 440  FORMAT(' HISTOGRAM PATTERNED')
C   It seems TopDrawer in ahsad does not understand error bars.
      IF(LERR.GE.1) CALL HSTBAR(FILE,NBN,XBIN,FBIN,ERR,UNITF,
     %     XYMM,LOGHV(1))
	WRITE(FILE,390) 'WHITE'
      RETURN
      END
      FUNCTION XLGSCL(L,X1,X2)
      IMPLICIT NONE
      INTEGER L
      REAL*8 XLGSCL,X1,X2
      IF(L.LE.0) THEN
        XLGSCL=0.5*(X1+X2)
      ELSE
        IF(X1.LE.0.OR.X2.LE.0) THEN
          XLGSCL=0.5*(X1+X2)
        ELSE
          XLGSCL=SQRT(X1*X2)
        ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE HSTBAR(FILE,NBN,XBIN,FBIN,ERR,UNITF,XYMM,LOGH)
      IMPLICIT NONE
      INTEGER FILE,NBN,LOGH
      REAL*8 XBIN(0:NBN),FBIN(NBN),ERR(NBN),UNITF,XYMM(2,2)
      INTEGER I,N
      REAL*8 X(6),Y(6),YMIN,XLGSCL
C
      YMIN=0
      IF(LOGH.NE.0) YMIN=1D-60
      DO 500 I=1,NBN
        X(1)=XLGSCL(LOGH,XBIN(I-1),0.5*(XBIN(I-1)+XBIN(I)))
        X(2)=XLGSCL(LOGH,0.5*(XBIN(I-1)+XBIN(I)),XBIN(I))
        X(3)=XLGSCL(LOGH,XBIN(I-1),XBIN(I))
        X(4)=X(3)
        X(5)=X(1)
        X(6)=X(2)
        Y(1)=FBIN(I)+ERR(I)
        Y(2)=Y(1)
        Y(3)=Y(1)
        Y(4)=MAX(YMIN,FBIN(I)-ERR(I))
        Y(5)=Y(4)
        Y(6)=Y(4)
        WRITE(FILE,420) (X(N),Y(N)/UNITF,N=1,6)
        WRITE(FILE,440)
 420    FORMAT(3(1PD11.4,D12.4,';'))
 440    FORMAT(' JOIN 1')
 500  CONTINUE
      RETURN
      END



