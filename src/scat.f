      SUBROUTINE SCAT(FILE,LNEW,IM,SMOD,ICOLOR,NN,FF,XYW,XYM,LOGHV,
     %     TTL,NC)
C  Scatter plot or Curve
C  LNEW:  flag for new page
C  IM  :  line mode. Points if IM=0
C         IM=0 :  points (SMOD not used)
C         IM>0 :  use (SMOD(i),i=1,IM)
C         IM<0 :  use predefined PATTRN(-IM) (SMOD not used)
      IMPLICIT NONE
      INTEGER FILE,LNEW,IM,ICOLOR,NN,LOGHV(2),NC(3)
      REAL*8 FF(2,NN),XYW(2,2),XYM(2,2),SMOD(*)
      CHARACTER*256 TTL(2,3)
      INTEGER I,N,N1,N2,II,NC1,NC2,J,NIN,NOUT,IM1
      REAL*8 XL,XH,YL,YH,CSIZE
      INCLUDE 'include/topdraw.h'
C
      XL=XYW(1,1)+1.5
      XH=XYW(2,1)-0.2
      YL=XYW(1,2)+1.5
      YH=XYW(2,2)-1.0
      IF(LNEW.GE.1) THEN
        WRITE(FILE,200)
 200    FORMAT(' NEWFRAME; SET FONT DUPLEX')
        CALL TDHEAD(FILE)
        WRITE(FILE,300) XL,XH,YL,YH
 300    FORMAT(
     %       ' SET WINDOW X FROM ',F7.3,' TO ',F7.3,/,
     %       ' SET WINDOW Y FROM ',F7.3,' TO ',F7.3)
        WRITE(FILE,310) XYM(1,1),XYM(2,1),XYM(1,2),XYM(2,2)
 310    FORMAT(' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %       ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4,/,
     %       ' SET ORDER X Y; SET STORAGE X Y')
        IF(LOGHV(1).GE.1) WRITE(FILE,315) 'X'
        IF(LOGHV(2).GE.1) WRITE(FILE,315) 'Y'
 315    FORMAT(' SET SCALE ',A,' LOG')
      ENDIF
      IF(IM.EQ.0) THEN
        II=1
        WRITE(FILE,320)
 320    FORMAT(' SET SYMBOL 1O SIZE 0.1')
      ELSEIF(IM.EQ.1) THEN
        WRITE(FILE,330) 100.0
      ELSEIF(IM.GT.1) THEN
        WRITE(FILE,330) (SMOD(I),I=1,IM)
 330    FORMAT(' SET PATTERN ',9(F6.3,:,1X),/,(5X,10(F6.3,:,1X)))
        II=0
	ELSE
	  IM1=-IM
	  IM1=MOD(IM1-1,MLMODE)+1
	  WRITE(FILE,335) PATTRN(IM1)
 335    FORMAT(' SET PATTERN ',A)
        II=0
      ENDIF
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

	IF(ICOLOR.GE.1.AND.ICOLOR.LE.MCOLR) THEN
	  WRITE(FILE,390) COLOR(ICOLOR)
	ENDIF
      N2=1-II
 400  N1=N2+II
      N2=MIN(N1+1000,NN)
      WRITE(FILE,410) (FF(1,N),FF(2,N),N=N1,N2)
 410  FORMAT(3(1PD11.4,D12.4,';'))
      IF(IM.EQ.0) THEN
        WRITE(FILE,450)
 450    FORMAT(' PLOT')
      ELSE
        WRITE(FILE,460)
 460    FORMAT(' JOIN 1 PATTERNED')
      ENDIF
      IF(N2.LT.NN) GOTO 400
	WRITE(FILE,390) 'WHITE'
C
      IF(IM.NE.0) RETURN
      NOUT=0
      DO 520 N=1,NN
        DO 510 J=1,2
          IF(FF(J,N).LT.XYM(1,J).OR.FF(J,N).GT.XYM(2,J)) THEN
            NOUT=NOUT+1
            GOTO 520
          ENDIF
 510    CONTINUE
 520  CONTINUE
      NIN=NN-NOUT
      WRITE(FILE,540) XYW(2,1)-1.5,XYW(1,2)+0.8,' in',NIN,
     %                XYW(2,1)-1.5,XYW(1,2)+0.45,'out',NOUT
 540  FORMAT(' TITLE',2F7.3,' SIZE 1.5 ',
     %     1H',' points ',A,'side  ',I7,1H')
      RETURN
      END
