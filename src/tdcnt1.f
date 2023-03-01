C************************** TDCNT1 ************************************
      SUBROUTINE TDCNT1(F,MX,NX,NY,XL,XH,YL,YH,NFV,FV,LFV,IFL,MSG)
C  DRAW CONTOURS OF TWO-Dimensional array F(I,J) BY TOPDRAWER.
C  External routines:  FVLINE,FVLIN1,TRACE
C  INPUT:
C   MX      First dimension of F  (from 0 to MX)
C   NX,NY   Number of abscissa-1 in x and y direction
C   F(0:MX,0:NY)   F(I,J) is the value at
C           x=XL+I/NX*(XH-XL),  y=YL+J/NY*(YH-YL)
C   XL,XH,YL,YH   (X,Y) COORDINATE AT LEFT-BOTTOM AND RIGHT-TOP.
C          CAN BE XL>XH,YL>YH.
C   FV(I), (I=1,NFV)  VALUE OF FUNCTION ON THE CONTOURS.
C   LFV(I), (I=1,NFV)  INTEGER ARRAY. LINE INTENSITY, COLOR, MODE.
C            100*II+10*IC+IM    (0<= II,IC,IM <=9)
C        IM=0 or 1:solid, 2:long dash, 3:medium dash, 4:short dash,
C           5:long dot-dash, 6:medium dot-dash, 7:short dot-dash,
C           8:dots, 9:fine dots
C        IC=0 or 1:WHITE, 2:RED, 3:YELLOW, 4:GREEN; 5:CYAN; 6:BLUE,
C           7:MAGENTA, 8:BLACK(BACKGROUND)
C        II= 1->1, 0 AND 2->2(DEFAULT), 3->3, 4 AND OVER->4.
C   IFL    OUTPUT FILE NUMBER
C   MSG    Message file number
C  OUTPUT:
C     ON FILE FT'IFL'F001
C
C  CAUTION:  'SET LIMIT' MUST BE DEFINED BEFORE CALLING THIS ROUTINE.
C            IT IS RECOMMENDED TO INSERT 'PLOT AXIS' COMMAND
C            AFTER 'SET LIMIT', BECAUSE THE AXES MAY COME OUT WITH
C            NON-WHITE COLOR WHEN THE COLOR OF THE CONTOUR DRAWN
C            FIRST IS NOT WHITE.
C
C    ENTRY TDCNT1X(F,MX,NX,NY,XG,YG,NFV,FV,LFV,W,IFL,MSG)
C       Same as TDCNT1 except that the grid need not be equally spaced.
C       Instead, the grid lines are defined by the arrays
C         XG(i) (i=0,NX), YG(j) (j=0,NY)
C       
      IMPLICIT NONE
      INTEGER MX,NX,NY,NFV,LFV(NFV),IFL,MSG
      REAL*8 F(0:MX,0:NY),XL,XH,YL,YH,FV(NFV)
      REAL*8 XG(0:NX),YG(0:NY)
      INTEGER MP
      INTEGER, ALLOCATABLE:: ID(:)
      REAL*8, ALLOCATABLE:: X(:),Y(:)
	LOGICAL*1, ALLOCATABLE:: W(:,:,:)
      INCLUDE 'include/topdraw.h'
      CHARACTER*47 JFMT/
     %  '(13H SET PATTERN ,A,19H ;JOIN 1 PATTERNED ,:,A)'/
      CHARACTER*18 CFMT/'(11H SET COLOR ,A)'/
      INTEGER LIBRTN,LIBMSGLVL
      COMMON/LIBCOM/LIBRTN,LIBMSGLVL
C
      INTEGER IENT,IX,IY,IFV,IDMM,NP,IER,I,NCON,IMODE,INTEN,ICOLR,
     %   N,N1,ISTAT
      REAL*8 XL0,XH0,YL0,YH0,DX,DY,ANX,ANY,FMAX,FMIN
C
      IENT=1
      IF(XL.EQ.XH.OR.YL.EQ.YH) GOTO 940
      XL0=XL
      XH0=XH
      YL0=YL
      YH0=YH
      DX=(XH-XL)/NX
      DY=(YH-YL)/NY
      GOTO 200
      ENTRY TDCNT1X(F,MX,NX,NY,XG,YG,NFV,FV,LFV,IFL,MSG)
      IENT=2
      XL0=XG(0)
      XH0=XG(NX)
      YL0=YG(0)
      YH0=YG(NY)
 200  CONTINUE
C      LIBMSGLVL=1
      IF(NFV.LE.0) GOTO 900
      IF(NX.LE.4.OR.NY.LE.4) GOTO 920
	MP=2*NX*NY+NX+NY
	ALLOCATE(ID(MP),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 960
	ALLOCATE(X(MP),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 960
	ALLOCATE(Y(MP),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 960
      ALLOCATE(W(0:NX,0:NY,2),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 960
      ANX=NX
      ANY=NY
      FMAX=F(0,0)
      FMIN=F(0,0)
      DO 220 IY=0,NY
      DO 220 IX=0,NX
      FMIN=MIN(FMIN,F(IX,IY))
  220 FMAX=MAX(FMAX,F(IX,IY))
      IF(LIBMSGLVL.GE.1) WRITE(MSG,230) FMIN,FMAX,XL0,XH0,YL0,YH0
  230 FORMAT(' ***** SUBR.TDCNT1 *****',/,
     %  5X,1PE11.4,'<F<',1PE11.4,' IN THE RANGE ',/,
     %  5X,1PE11.4,'<X<',1PE11.4,4X,1PE11.4,'<Y<',1PE11.4)
      DO 600 IFV=1,NFV
      IF(FV(IFV).LE.FMIN.OR.FV(IFV).GE.FMAX) GOTO 600
      CALL FVLINE(F,1,MX,NX,NY,FV(IFV),IDMM,X,Y,ID,MP,NP,W,IER,MSG)
      IF(IER.NE.0) THEN
        IF(LIBMSGLVL.GE.0) WRITE(MSG,240) MP,FV(IFV)
 240    FORMAT(' (SUBR.TDCNT1) MP=',I5,' INSUFFICIENT FOR FV=',1PE11.4,
     %       /,'    PLOT IGNORED. CALL THE PROGRAMMER.')
      ELSE
        IF(NP.GE.1) THEN
          IF(IENT.EQ.1) THEN
            DO 250 I=1,NP
              X(I)=XL+DX*MAX(0.0D0,MIN(ANX,X(I)))
              Y(I)=YL+DY*MAX(0.0D0,MIN(ANY,Y(I)))
 250        CONTINUE
          ELSE
            DO 260 I=1,NP
              IX=MAX(0,MIN(NX-1,INT(X(I))))
              X(I)=XG(IX)+(XG(IX+1)-XG(IX))*(X(I)-IX)
              IY=MAX(0,MIN(NY-1,INT(Y(I))))
              Y(I)=YG(IY)+(YG(IY+1)-YG(IY))*(Y(I)-IY)
 260        CONTINUE
          ENDIF
        ENDIF
        NCON=0
        IF(NP.GE.2) THEN
          IMODE=MAX(1,MIN(MLMODE,MOD(LFV(IFV),10)))
          INTEN=LFV(IFV)/10
          ICOLR=MAX(1,MIN(MCOLR,MOD(INTEN,10)))
          INTEN=MAX(0,MIN(4,INTEN/10))
          IF(INTEN.EQ.0) INTEN=2
          IF(INTEN.NE.2) WRITE(IFL,270) INTEN
 270      FORMAT(' SET INTENSITY ',I1)
          IF(ICOLR.NE.0) WRITE(IFL,CFMT) COLOR(ICOLR)
          DO 400 N=1,NP
            IF(ID(N).LT.0) THEN
 320          WRITE(IFL,340) (X(I),Y(I),I=N1,N)
 340          FORMAT(3(1PD11.4,D12.4,';'))
              WRITE(IFL,JFMT) PATTRN(IMODE)
              WRITE(IFL,360) FV(IFV)
 360          FORMAT(' ( End of F=',1PE11.4,')')
              NCON=NCON+1
            ELSEIF(ID(N).GT.0) THEN
              N1=N
            ENDIF
 400      CONTINUE
          IF(INTEN.NE.2) WRITE(IFL,270) 2
          IF(ICOLR.NE.0) WRITE(IFL,CFMT) 'WHITE'
        ENDIF
        IF(LIBMSGLVL.GE.1) WRITE(MSG,520) NCON,FV(IFV)
 520    FORMAT(10X,I3,' CONTOURS PLOTTED FOR FV=',1PE11.4)
      ENDIF
 600  CONTINUE
      GOTO 990
  900 IF(LIBMSGLVL.GE.0) WRITE(MSG,910) NFV
  910 FORMAT(' (SUBR.TDCNT1) INVALID ARGUMENT NFV=',I5)
      GOTO 990
  920 IF(LIBMSGLVL.GE.0) WRITE(MSG,930) NX,NY
  930 FORMAT(' (SUBR.TDCNT1) INVALID ARGUMENT NX=',I5,'  NY=',I5)
      GOTO 990
  940 IF(LIBMSGLVL.GE.0) WRITE(MSG,950) XL,XH,YL,YH
  950 FORMAT(' (SUBR.TDCNT1) INVALID ARGUMENT XL,XH,YL,YH=',/,
     %  10X,1P4E12.5)
      GOTO 990
960	IF(LIBMSGLVL.GE.0) WRITE(MSG,965)
965   FORMAT(' (SUBR.TDCNT1) Memory allocation for contour plot ',
     %   'failed.')
	GOTO 990
990	DEALLOCATE(ID,X,Y,W,STAT=ISTAT)
	RETURN
      END
