      SUBROUTINE PLBBFL(T,SS,IS)
	USE BEAMCM
	USE BBCOM
      IMPLICIT NONE
      INTEGER IS
      REAL*8 T,SS(2)
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/cnstcm.h'
      INCLUDE 'include/topdraw.h'
C
      INTEGER I,N,L,IXY,J,I00,NNXY(2),NDXY(2),NOUT,IOUT,NN,
     %    NARW,KARW(2),K0ARW(2)
      REAL*8 XL00/0.0/,XH00/13.0/,YL00/0.0/,YH00/10.0/
      REAL*8 XL0,XH0,YL0,YH0,XL,XH,YL,YH,XL1,XH1,YL1,YH1,XB1,YB1
      REAL*8 XYMM(2,2),XYMM1(2,2),FAC(2),FACQ,FACE,XYW,XYC(2),
     %    FF(2),XY(2),DTH,CHU0,SHU0,FFA
      REAL*8 UNIT/1D-6/,BBPHI
      CHARACTER*5 LLRR(2)/'Right','Left'/
      CHARACTER*47 JFMT/
     %  '(13H SET PATTERN ,A,19H ;JOIN 1 PATTERNED ,:,A)'/
      INTEGER MBUF,NBUF
      PARAMETER (MBUF=1000)
      REAL*8 XX(MBUF),YY(MBUF)
      REAL*8 PI/3.14159 26535 89793 238D0/
      real*8 sc(2)/0.2,1.0/
      save sc
C
      CALL CPUTIM('PLBBFL',1)
      XL0=XL00+0.01
      XH0=XH00-0.01
      YL0=YL00+1.0
      YH0=YH00-1.0
C  Total number of points including outside mesh region
C  Total region is about 1.5 times mesh region
C    (Must not be larger than 2.0 because of the work area size BBWORK)
      DO 140 IXY=1,2
        NDXY(IXY)=2*NINT((NXY(IXY)*1.5D0-NXY(IXY))/2D0)
        NNXY(IXY)=NXY(IXY)+2*NDXY(IXY)
        FAC(IXY)=DFLOAT(NNXY(IXY))/NXY(IXY)
 140  CONTINUE
      print *, " plbbfl write 160 iflbpl= ",iflbpl
      WRITE(IFLBPL,160)
 160  FORMAT(' NEWFRAME; SET FONT DUPLEX')
      CALL TDHEAD(IFLBPL)
      print *, " plbbfl write 200 iflbpl= ",iflbpl
      WRITE(IFLBPL,200) T,(SS(2)+SS(1))/2,(SS(2)-SS(1))/2,
     %  XL0+1.0,YH0+0.65,XL0+8.0,YH0+0.6,
     %  T,(SS(2)+SS(1))/2,(SS(2)-SS(1))/2
 200  FORMAT(
     % '(  Beam-Beam field T=',1PD12.5,'  S=',1PD12.5,'+-',D12.5,/,
     % ' TITLE ',0P2F7.3,' SIZE 1.8 ',2H'',/,
     % ' MORE ',1H','Charge Distribution and Beam Electric Field',1H',/,
     % ' TITLE ',0P2F7.3,' SIZE 1.4 ',2H'',/,
     % ' MORE ',1H','T=',3PF8.4,'mm  S=',3PF8.4,'+',3PF8.4,'mm',1H',/,
     % ' CASE ',1H','  ',8X,    '      ',8X,    'M',8X,    '  ',1H',/,
     % ' SET SYMBOL 1O SIZE 0.1')
      DO 600 L=1,2
        IF(BBFLG(L).EQ.0) GOTO 600
C        window for whole region including outside mesh region
        XL=XL0+(XH0-XL0)/2*(L-1)+1.0
        XH=XL0+(XH0-XL0)/2*L
        YL=(YL0+YH0)/2+0.4
        YH=YH0
C        window for mesh region
        XB1=(XH-XL)/NXY(1)/FAC(1)
        YB1=(YH-YL)/NXY(2)/FAC(2)
        XL1=(XL+XH)/2-XB1*NXY(1)/2
        XH1=(XL+XH)/2+XB1*NXY(1)/2
        YL1=(YL+YH)/2-YB1*NXY(2)/2
        YH1=(YL+YH)/2+YB1*NXY(2)/2
        DO 210 IXY=1,2
          XYW=BBDXY(IXY,L)*NXY(IXY)/2
          XYC(IXY)=XYMIN(IXY,L)+XYW
          XYMM1(1,IXY)=(XYC(IXY)-XYW)/UNIT
          XYMM1(2,IXY)=(XYC(IXY)+XYW)/UNIT
          XYMM(1,IXY)=sc(l)*(XYC(IXY)-XYW*FAC(IXY))/UNIT
          XYMM(2,IXY)=sc(l)*(XYC(IXY)+XYW*FAC(IXY))/UNIT
 210    CONTINUE
        print *, " plbbfl write 220 iflbpl= ",iflbpl
        WRITE(IFLBPL,220) LLRR(L),XL,XH,YL,YH,
     %     ((XYMM(I,IXY),I=1,2),IXY=1,2),
     %     XL+0.5,YH0+0.3,LLRR(L),XL-0.4,(YL+YH)/2
 220    FORMAT(' (',A,'-Going Beam)',/,
     %   ' SET WINDOW X FROM ',0PF7.3,' TO ',F7.3,/,
     %   ' SET WINDOW Y FROM ',0PF7.3,' TO ',F7.3,/,
     %   ' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %   ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4,/,
     %   ' TITLE ',0P2F7.3,' SIZE 1.5 ',1H',
     %         A,'-going beam',1H',/,
     %   ' TITLE ',0P2F7.3,' SIZE 1.5 ANGLE 90',2H'',/,
     %   ' MORE ',1H','y (Mm)',1H',';CASE ',1H','   G  ',1H',/,
     %   ' SET AXIS ALL ON',/,
     %   ' SET LABELS LEFT ON RIGHT OFF BOTTOM OFF TOP OFF',/,
     %   ' SET TICKS ALL ON; SET TICKS SIZE 0.05')
C-- Scatter plot
        NBUF=0
        DO 300 N=1,NP
	    IF(LOST(N).NE.0) GOTO 300
          IF(ISBIN(N).NE.IS) GOTO 300
          IF(KIND(N).EQ.1) GOTO 300
          IF(PNAME(N).NE.'    ') GOTO 300
          IF(TXYS(0,N).GT.T) GOTO 300
          IF(EP(3,N).GE.0.AND.L.EQ.2.OR.
     %       EP(3,N).LT.0.AND.L.EQ.1) GOTO 300
          NBUF=NBUF+1
          XX(NBUF)=TXYS(1,N)+EP(1,N)/EP(0,N)*(T-TXYS(0,N))
          YY(NBUF)=TXYS(2,N)+EP(2,N)/EP(0,N)*(T-TXYS(0,N))
          IF(NBUF.GE.MBUF) THEN
            WRITE(IFLBPL,260) (XX(I)/UNIT,YY(I)/UNIT,I=1,NBUF)
 260        FORMAT(3(1PD11.4,D12.4,';'))
            WRITE(IFLBPL,280)
 280        FORMAT(' PLOT')
            NBUF=0
          ENDIF
 300    CONTINUE
        IF(NBUF.NE.0) THEN
          WRITE(IFLBPL,260) (XX(I)/UNIT,YY(I)/UNIT,I=1,NBUF)
          WRITE(IFLBPL,280)
        ENDIF
C-- Charge Distribution
        FACQ=ECHARG/(BBDXY(1,L)*BBDXY(2,L)*(SS(2)-SS(1)))
        I00=1+NXY(1)*NXY(2)*(L-1)
        DO 340 I=1,NXY(1)*NXY(2)
          BBWORK(I)=FACQ*BBQ(I00+I)
 340    CONTINUE
        CALL PLBBFL2(1,XL,XH,YL,YH,XL1,XH1,YL1,YH1,XYMM1,XYMM1,
     %    BBWORK,NXY,IFLBPL,MSGFL)
C-- Electric Field
        YL=YL0+0.4
        YH=(YL0+YH0)/2
        XL1=(XL+XH)/2-XB1*NXY(1)/2
        XH1=(XL+XH)/2+XB1*NXY(1)/2
        YL1=(YL+YH)/2-YB1*NXY(2)/2
        YH1=(YL+YH)/2+YB1*NXY(2)/2
        print *, " plbbfl write 420 iflbpl= ",iflbpl
        WRITE(IFLBPL,420) XL,XH,YL,YH,
     %     ((XYMM(I,IXY),I=1,2),IXY=1,2),
     %     (XL+XH)/2,YL-0.4,
     %     XL-0.4,(YL+YH)/2
 420    FORMAT(' ( Electric Field)',/,
     %   ' SET WINDOW X FROM ',0PF7.3,' TO ',F7.3,/,
     %   ' SET WINDOW Y FROM ',0PF7.3,' TO ',F7.3,/,
     %   ' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %   ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4,/,
     %   ' TITLE ',0P2F7.3,' SIZE 1.5 ',2H'',/,
     %   ' MORE ',1H','x (Mm)',1H',';CASE ',1H','   G  ',1H',/,
     %   ' TITLE ',0P2F7.3,' SIZE 1.5 ANGLE 90',2H'',/,
     %   ' MORE ',1H','y (Mm)',1H',';CASE ',1H','   G  ',1H',/,
     %   ' SET AXIS ALL ON',/,
     %   ' SET LABELS LEFT ON RIGHT OFF BOTTOM ON TOP OFF',/,
     %   ' SET TICKS ALL ON')
        FACE=2*MASS(2)*RE/(SS(2)-SS(1))
        NARW=15
        DO 430 IXY=1,2
          KARW(IXY)=MAX(1,NNXY(IXY)/NARW)
          K0ARW(IXY)=(NNXY(IXY)-(NNXY(IXY)/KARW(IXY))*KARW(IXY))/2+1
 430    CONTINUE
        DO 460 I=1,NNXY(1)
          XY(1)=XYMM(1,1)*UNIT+BBDXY(1,L)*(I-0.5D0)
          DO 450 J=1,NNXY(2)
            XY(2)=XYMM(1,2)*UNIT+BBDXY(2,L)*(J-0.5D0)
            IF(I.GT.NDXY(1).AND.I.LT.NNXY(1)-NDXY(1).AND.
     %         J.GT.NDXY(2).AND.J.LT.NNXY(2)-NDXY(2)) THEN
              DO 440 IXY=1,2
                FF(IXY)=BBPHI(I-NDXY(1),J-NDXY(2),L,IXY)/BBDXY(IXY,L)
 440          CONTINUE
            ELSE
              CALL BBKCK(3-L,1,XY,FF,NOUT,NXY(1),NXY(2),BBQ,0,IOUT)
            ENDIF
            FFA=SQRT(FF(1)**2+FF(2)**2)
            BBWORK(I+NNXY(1)*(J-1))=FACE*FFA
            IF(MOD(I-K0ARW(1),KARW(1)).EQ.0.AND.
     %         MOD(J-K0ARW(2),KARW(2)).EQ.0.AND.FFA.NE.0) THEN
              CALL TDARRW(-FF(1)/FFA,FF(2)/FFA,XY(1)/UNIT,XY(2)/UNIT,
     %           XL,XH,YL,YH,XYMM,NARW,IFLBPL)
            ENDIF
 450      CONTINUE
 460    CONTINUE
        IF(LBBEL.NE.0) THEN
          NN=100
          DTH=2*PI/NN
          print *, " plbbfl write 260 iflbpl= ",iflbpl," lbbel= ",lbbel
          WRITE(IFLBPL,260) ((XYC(1)+BBR00(L)*COS(I*DTH))/UNIT,
     %         (XYC(2)+BBR00(L)*SIN(I*DTH))/UNIT,I=0,NN)
          WRITE(IFLBPL,JFMT) PATTRN(8)
          IF(ABS(LBBEL).EQ.2) THEN
            CHU0=COSH(BBU00)
            SHU0=SINH(BBU00)
            WRITE(IFLBPL,260) ((XYC(1)+BBEL(L)*CHU0*COS(I*DTH))/UNIT,
     %         (XYC(2)+BBEL(L)*SHU0*SIN(I*DTH))/UNIT,I=0,NN) 
            WRITE(IFLBPL,JFMT) PATTRN(8)
          ENDIF
        ENDIF
        CALL PLBBFL2(2,XL,XH,YL,YH,XL,XH,YL,YH,XYMM,XYMM1,
     %      BBWORK,NNXY,IFLBPL,MSGFL)
 600  CONTINUE
      CALL CPUTIM('PLBBFL',2)
      RETURN
      END
C--------------------------------------------------------------------
      SUBROUTINE ROUND1(FMIN,FMAX,MJ,JMIN,JMAX,DF)
      IMPLICIT NONE
      INTEGER MJ,JMIN,JMAX
      REAL*8 FMIN,FMAX,DF
      INTEGER N,IDF
      REAL*8 DF1
	INCLUDE 'include/ctrlcm.h'
C
      DF1=(FMAX-FMIN)/(MJ-1)
      N=INT(LOG10(DF1)+100D0)-100
      IDF=INT(DF1/10D0**N)+1
      IF(IDF.EQ.7) THEN
        IDF=8
      ELSEIF(IDF.EQ.9) THEN
        IDF=10
      ENDIF
      DF=IDF*10D0**N
      JMIN=INT(FMIN/DF+10000D0)-10000+1
      JMAX=INT(FMAX/DF+10000D0)-10000
      IF(JMAX-JMIN+1.GT.MJ) THEN
        WRITE(MSGFL,100)
 100    FORMAT('Program error in PLBBFL/ROUND1')
        CALL STOPCAIN(100)
      ENDIF
      RETURN
      END
C--------------------------------------------------------------------
      SUBROUTINE PLBBFL2(LL,XL,XH,YL,YH,XL1,XH1,YL1,YH1,XYMM,XYMM1,
     %   BBWORK,NXY,IFLBPL,MSGFL)
      IMPLICIT NONE
      INTEGER LL,NXY(2),IFLBPL,MSGFL
      REAL*8 XL,XH,YL,YH,XL1,XH1,YL1,YH1,XYMM(2,2),XYMM1(2,2),BBWORK(*)
      INCLUDE 'include/topdraw.h'
      CHARACTER*47 JFMT/
     %  '(13H SET PATTERN ,A,19H ;JOIN 1 PATTERNED ,:,A)'/
      INTEGER IXY,I,J,JMIN,JMAX,NC
      REAL*8 FMAX,FMIN,DF1
      CHARACTER*20 TEXT
      INTEGER MFV
      PARAMETER (MFV=16)
      INTEGER NFV,LFV(MFV)
      REAL*8 FV(MFV)
C
      print *, " plbbfl2 write 300 iflbpl= ",iflbpl
      WRITE(IFLBPL,300)
 300  FORMAT('(  Frame of mesh region)')
      WRITE(IFLBPL,310) XYMM1(1,1),XYMM1(1,2),
     %     XYMM1(2,1),XYMM1(1,2),XYMM1(2,1),XYMM1(2,2),
     %     XYMM1(1,1),XYMM1(2,2),XYMM1(1,1),XYMM1(1,2)
 310  FORMAT(3(1PD11.4,D12.4,';'))
      WRITE(IFLBPL,JFMT) PATTRN(4)
      IF(LL.EQ.1) THEN
        TEXT='Charge distribution'
      ELSE
        TEXT='Electric Field'
      ENDIF
      print *, " plbbfl2 write 320 iflbpl= ",iflbpl," text= ",text
      WRITE(IFLBPL,320) TEXT,XL1,XH1,YL1,YH1,
     %     ((XYMM(I,IXY),I=1,2),IXY=1,2)
 320  FORMAT('(',A,')',/,
     %     ' SET WINDOW X FROM ',0PF7.3,' TO ',F7.3,/,
     %     ' SET WINDOW Y FROM ',0PF7.3,' TO ',F7.3,/,
     %     ' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %     ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4)
      IF(LL.EQ.1) WRITE(IFLBPL,330)
 330  FORMAT(' SET AXIS ALL OFF')
      FMAX=0
      FMIN=0
      DO 340 I=1,NXY(1)*NXY(2)
        FMIN=MIN(FMIN,BBWORK(I))
        FMAX=MAX(FMAX,BBWORK(I))
 340  CONTINUE
      CALL ROUND1(FMIN,FMAX,MFV,JMIN,JMAX,DF1)
      NFV=JMAX-JMIN+1
      DO 360 J=1,NFV
        FV(J)=DF1*(J-1+JMIN)
        LFV(J)=1
        IF(FV(J).EQ.0) LFV(J)=3
 360  CONTINUE
      IF(LL.EQ.1) THEN
        NC=6
        TEXT(1:NC)='C/m233'
        TEXT(NC+1:2*NC)='   X X'
      ELSE
        NC=3
        TEXT(1:NC)='V/m'
        TEXT(NC+1:2*NC)='   '
      ENDIF
      print *, " plbbfl2 write 400 iflbpl= ",iflbpl
      WRITE(IFLBPL,400) XH-3.5,YH+0.15,DF1,
     %     TEXT(1:NC),TEXT(NC+1:2*NC)
 400  FORMAT(' TITLE ',2F7.3,' SIZE 1.4 ',2H'',/,
     %     ' MORE ',1H','contour interval=',1PD9.2,A,1H',/,
     %     ' CASE ',1H','                 ',9X,    A,1H')
      CALL TDCNT1XX(BBWORK,NXY,XYMM,NFV,FV,LFV,IFLBPL,MSGFL)
      RETURN
      END
C---------------------------------------------------------------------
      SUBROUTINE TDCNT1XX(FF,NXY,XYMM,NFV,FV,LFV,IFL,MSGFL)
C  Shift half mesh. TDCNT1 accepts FF(0:NX,0:NY)
C  TDCNT1XX accepts FF(NX,NY)
      IMPLICIT NONE
      INTEGER NXY(2),NFV,LFV(NFV),IFL,MSGFL
      REAL*8 FF(*),XYMM(2,2),FV(NFV)
      INTEGER IXY
      REAL*8 XYC,XYW,XYMM2(2,2)
      DO 380 IXY=1,2
        XYC=(XYMM(2,IXY)+XYMM(1,IXY))/2
        XYW=(XYMM(2,IXY)-XYMM(1,IXY))/2
        XYMM2(1,IXY)=XYC-XYW*(NXY(IXY)-1)/NXY(IXY)
        XYMM2(2,IXY)=XYC+XYW*(NXY(IXY)-1)/NXY(IXY)
 380  CONTINUE
      CALL TDCNT1(FF,NXY(1)-1,NXY(1)-1,NXY(2)-1,
     %     XYMM2(1,1),XYMM2(2,1),
     %     XYMM2(1,2),XYMM2(2,2),
     %     NFV,FV,LFV,IFL,MSGFL)
      RETURN
      END
C---------------------------------------------------------------------
      SUBROUTINE TDARRW(CO,SI,X,Y,XL,XH,YL,YH,XYMM,NARW,IFLBPL)
C  Direction of the arrows is the angle in the real space
C  (not deformed by x/y scale ratio) 
      IMPLICIT NONE
      INTEGER NARW,IFLBPL
      REAL*8 CO,SI,X,Y,XL,XH,YL,YH,XYMM(2,2)
      REAL*8 AL,X1,Y1,ARSIZE
C
      AL=MIN(XH-XL,YH-YL)/(2.5*NARW)
      ARSIZE=AL/2*10
      X1=X+(XYMM(2,1)-XYMM(1,1))/(XH-XL)*AL*CO
      Y1=Y+(XYMM(2,2)-XYMM(1,2))/(YH-YL)*AL*SI
      WRITE(IFLBPL,200) ARSIZE
 200  FORMAT(' SET ARROW SIZE ',F6.3,' FLARE 0.6')
      WRITE(IFLBPL,220) X,Y,X1,Y1
 220  FORMAT(' ARROW FROM',1P2D12.4,' DATA TO',2D12.4,' DATA')
      RETURN
      END
