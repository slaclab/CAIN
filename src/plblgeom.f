	SUBROUTINE PLBLGEOM(BLNAME,XYS0,E10,E30,HVMAT,PAPER,VHRATIO,
     %  MAGWIDTH,NAMFIL,FILE,IRTN)
C  Plot geometry of beamline
C   BLNAME     beamline name
C   XYS0(3)    CAIN coordinate (x,y,s) at the beamline entrance
C   E30(3)     Unit vector along beamline coordinate axis S
C              in CAIN coordinate
C   E10(3)     Unit vector along beamline coordinate axis X
C              in CAIN coordinate
C   HVMAT(2,3) Horizontal and vertical variables in the plot (i.e.,
C              the data coordinate of TopDrawer) in CAIN coordinate are
C               hor. variable = HVMAT(1,1)*x + HVMAT(1,2)*y + HVMAT(1,3)*s
C               ver. variable = HVMAT(2,1)*x + HVMAT(2,2)*y + HVMAT(2,3)*s
C   PAPER(2)   Paper size in inches. If =(0,0), adopt (13,10).
C   VHRATIO    If !=1, adopt different scale in hor. and ver. direction.
C              When >1, expanded in vertical direction.
C   NAMFIL     Filter of the magnet neames to be printed.
C              It may contain the wildcard *. If NAMFIL is null or
C              blanck, all bends and quads are printed.
C    IRTN     0   normal
C          1000   File open error
C          1001   Beamline nonexisting
C          1002   Invalid E3/E1 vector
C          1011   STARTBL not called yet
C          1012   nesting level too deep
C          1013   recursive citation of beamline name
	USE BEAMLN
	IMPLICIT NONE
	CHARACTER(*) BLNAME
	INTEGER FILE,IRTN
	REAL(8) XYS0(3),E10(3),E30(3),HVMAT(2,3),PAPER(2),VHRATIO,MAGWIDTH
	CHARACTER(*) NAMFIL
	INTEGER IDBL,JJ,I,NN,N,K,N0
	REAL(8) XYS(3),XYS1(3),S,SUM1,SUM3,SUM31,APERT1(2),
     %  WND(3,2),DTOTXT(2,2),SCL,HVMM(3,2),HV1(2),WID(0:3),
     %  CSIZE
C  DTOTXT: transformation from data coordinate (h,v) in meters 
C  to text coodinate (ht,vt) in inches
C      ht = DTOTXT(1,1)*h + DTOTXT(1,2)
C      vt = DTOTXT(2,1)*v + DTOTXT(2,2)
C  In the text coodinate, the left-bottom edge of the paper is (0,0)
C  and the top-right edge is (PSIZE(1),PSIZE(2)).
C  WID(3):  width of dipole, quadrupole, other higher multipoles
C       in the data coordinate (meter)
C
	REAL(8) E1(3),E2(3),E3(3),EE(3,3),EE0(3,3)
	EQUIVALENCE (EE(1,1),E1(1)),(EE(1,2),E2(1)),(EE(1,3),E3(1))
	REAL(8) PSIZE(2)
	REAL(8) MARGIN1(2),MARGIN2(2)
	CHARACTER(MAXMAGNAME) NAM
	CHARACTER(21) LLEN
	CHARACTER(5) FTOC5
      INCLUDE 'include/ctrlcm.h'
	REAL(8) PI/3.141592653589793238D0/

	MARGIN1(1)=1.0
	MARGIN1(2)=1.0
C       paper edge to axis in inches
	MARGIN2(1)=0.3
	MARGIN2(2)=0.3
C       axis to object edge
	PSIZE=PAPER
	IF(PSIZE(1).LE.3*(MARGIN1(1)+MARGIN2(1))) PSIZE(1)=13
	IF(PSIZE(2).LE.3*(MARGIN1(2)+MARGIN2(2))) PSIZE(2)=10
	CSIZE=1.0
	WID(0)=0.5*MAGWIDTH   !!  drift. used only for writing the name
	WID(1)=MAGWIDTH
	WID(2)=0.75*MAGWIDTH
	WID(3)=0.55*MAGWIDTH
	E1=E10
	E3=E30
	SUM3=E3(1)**2+E3(2)**2+E3(3)**2
	SUM1=E1(1)**2+E1(2)**2+E1(3)**2
	IF(SUM3.NE.0) THEN
	  SUM3=SQRT(SUM3)
	  E3=E3/SUM3
	  SUM31=E3(1)*E1(1)+E3(2)*E1(2)+E3(3)*E1(3)
	  E1=E1-SUM31*E3
	  SUM1=E1(1)**2+E1(2)**2+E1(3)**2
	  IF(SUM1.NE.0) THEN
	    SUM1=SQRT(SUM1)
	    E1=E1/SUM1
	  ELSE
	    GOTO 920
	  ENDIF
	ELSE
	  GOTO 920
	ENDIF
	E2(1)=E3(2)*E1(3)-E3(3)*E1(2)
	E2(2)=E3(3)*E1(1)-E3(1)*E1(3)
	E2(3)=E3(1)*E1(2)-E3(2)*E1(1)
	EE0=EE
C  Find the coordinate range
      HVMM(1,1:2)=1D60
	HVMM(2,1:2)=-1D60
	CALL STARTBL(BLNAME,IDBL,APERT1)
	IF(IDBL.LE.0) GOTO 910
	NN=BL(IDBL)%NEXP
	XYS=XYS0
	DO N=0,NN
	  IF(N.NE.0) THEN
          JJ=BL(IDBL)%MAGID(N)
	    CALL TRKBLGEO(MAG(JJ)%LENGTH%X,MAG(JJ)%ANGLE,
     %      MAG(JJ)%ROTATE,S,XYS,EE)
	  ENDIF
        DO I=1,2
          HV1(I)=HVMAT(I,1)*XYS(1)+HVMAT(I,2)*XYS(2)+HVMAT(I,3)*XYS(3)
	    HVMM(1,I)=MIN(HVMM(1,I),HV1(I))
	    HVMM(2,I)=MAX(HVMM(2,I),HV1(I))
	  ENDDO
	ENDDO
      DO I=1,2
	  HVMM(1,I)=HVMM(1,I)-WID(1)/2*1.1
	  HVMM(2,I)=HVMM(2,I)+WID(1)/2*1.1
        HVMM(3,I)=(HVMM(2,I)-HVMM(1,I))
     %    *(PSIZE(I)-2*MARGIN1(I))/(PSIZE(I)-2*(MARGIN1(I)+MARGIN2(I)))
	  HV1(I)=(HVMM(2,I)+HVMM(1,I))/2
	  HVMM(1,I)=HV1(I)-HVMM(3,I)/2
	  HVMM(2,I)=HV1(I)+HVMM(3,I)/2
	  WND(3,I)=PSIZE(I)-2*MARGIN1(I)
	  WND(1,I)=MARGIN1(I)
	  WND(2,I)=PSIZE(I)-MARGIN1(I)
      ENDDO
      IF(HVMM(3,1)/WND(3,1).GE.VHRATIO*HVMM(3,2)/WND(3,2)) THEN
	  SCL=WND(3,1)/HVMM(3,1)
	  DTOTXT(1,1)=SCL
	  DTOTXT(1,2)=-SCL*HVMM(1,1)+WND(1,1)
	  DTOTXT(2,1)=SCL*VHRATIO
	  DTOTXT(2,2)=-SCL*VHRATIO*(HVMM(1,2)+HVMM(2,2))/2
     %    +(WND(1,2)+WND(2,2))/2
	  WND(3,2)=VHRATIO*HVMM(3,2)*SCL
	  WND(1,2)=PSIZE(2)/2-WND(3,2)/2
	  WND(2,2)=PSIZE(2)/2+WND(3,2)/2
	ELSE
	  SCL=WND(3,2)/HVMM(3,2)
	  DTOTXT(1,1)=SCL
	  DTOTXT(1,2)=-SCL*(HVMM(1,1)+HVMM(2,1))/2
     %    +(WND(1,1)+WND(2,1))/2
	  DTOTXT(2,1)=SCL*VHRATIO
	  DTOTXT(2,2)=-SCL*VHRATIO*HVMM(1,2)+WND(1,2)
	  WND(3,1)=HVMM(3,1)*SCL/VHRATIO
	  WND(1,1)=PSIZE(1)/2-WND(3,1)/2
	  WND(2,1)=PSIZE(1)/2+WND(3,1)/2
	ENDIF

      WRITE(FILE,200) PSIZE(1),PSIZE(2),
     %  WND(1,1),WND(2,1),WND(1,2),WND(2,2),
     %  HVMM(1,1),HVMM(2,1),HVMM(1,2),HVMM(2,2)
200   FORMAT(' NEWFRAME; SET FONT DUPLEX',/,
     % ' SET SIZE ',0P2F8.2,/,
     % ' SET WINDOW X',0P2F10.3,' Y',2F10.3,/,
     % ' SET LIMIT X',1P2D13.5,' Y',2D13.5,/,
     % ' SET AXIS ALL OFF BOTTOM ON LEFT ON',/,
     % ' SET TICKS ALL OFF BOTTOM ON LEFT ON',/,
     % ' SET LABELS ALL OFF BOTTOM ON LEFT ON',/,
     % ' PLOT AXIS')
C      WRITE(FILE,220) WND(1,1),WND(2,1),WND(1,2),WND(2,2),
C     %  WND(1,1),WND(2,1),WND(1,2),WND(2,2)
C220   FORMAT(' SET WINDOW X',0P2F10.3,' Y',2F10.3,/,
C     % ' SET LIMIT X',1P2D13.5,' Y',2D13.5,/,
C     % ' SET AXIS ALL OFF')

	DO K=1,2
	  XYS=XYS0
	  XYS1=XYS
	  EE=EE0
	  S=0
	  N0=1
	  IF(K.EQ.1) THEN
	    WRITE(FILE,300)
300       FORMAT('( Magnet pictures )')
        ELSE
	    WRITE(FILE,310)
310       FORMAT('( Magnet names )')
        ENDIF
	  DO N=1,NN+1
          IF(N.LE.NN) THEN
			  JJ=BL(IDBL)%MAGID(N)
	      IF(MAG(JJ)%K1%X.EQ.0.AND.MAG(JJ)%ANGLE.EQ.0
     %        .AND.MAG(JJ)%LENGTH%X.EQ.0) CYCLE
	    ELSE
	      JJ=0
	    ENDIF
	    IF(JJ.EQ.0.OR.JJ.NE.BL(IDBL)%MAGID(N0)) THEN
	      CALL BLMAGPIC2(K,FILE,IDBL,N0,CSIZE,
     %        HVMAT,DTOTXT,XYS1,XYS,WID,NAMFIL)
	      N0=N
	      XYS1=XYS
	    ENDIF
	    IF(JJ.NE.0) THEN
			  CALL TRKBLGEO(MAG(JJ)%LENGTH%X,MAG(JJ)%ANGLE,
     %        MAG(JJ)%ROTATE,S,XYS,EE)
	    ENDIF
	  ENDDO
	ENDDO
600   IRTN=0
	GOTO 1000

910	IRTN=1001
	IF(MSGLVL.GE.0) WRITE(MSGFL,915) BLNAME
915   FORMAT(' (SUBR.PLBLGEOM) Beamline "',A,'" does not exist.')
      GOTO 1000
920	IRTN=1002
	IF(MSGLVL.GE.0) WRITE(MSGFL,925) BLNAME
925   FORMAT(' (SUBR.PLBLGEOM) Invalid E3 and/or E1 vector.')
      GOTO 1000

1000  RETURN
	END

	SUBROUTINE BLMAGPIC2(K,FILE,IDBL,N0,CSIZE,
     %            HVMAT,DTOTXT,XYS0,XYS1,WID,NAMFIL)
C        Changed to data coordinate on Jan.31.2003
	USE BEAMLN
	IMPLICIT NONE
	INTEGER K,FILE,IDBL,N0
	REAL(8) CSIZE,HVMAT(2,3),DTOTXT(2,2),XYS0(3),XYS1(3),WID(0:3)
	CHARACTER(*) NAMFIL
	INTEGER J,ITY,I,NC
	REAL(8) HV0(2),HV1(2),HVT0(2),HVT1(2),U(2),V(2),DU,ANG,VL
	REAL(8) HVA0(2),HVA1(2),HVA2(2),HVA3(2)
	LOGICAL NAMEFILTER,LL
	REAL(8) PI/3.141592653589793238D0/
	INTEGER LSIDE/1/
C        name on the counterclockwise side. (-1 for clockwise)

	DO I=1,2
	  HV0(I)=HVMAT(I,1)*XYS0(1)+HVMAT(I,2)*XYS0(2)+HVMAT(I,3)*XYS0(3)
	  HVT0(I)=DTOTXT(I,1)*HV0(I)+DTOTXT(I,2)
	  HV1(I)=HVMAT(I,1)*XYS1(1)+HVMAT(I,2)*XYS1(2)+HVMAT(I,3)*XYS1(3)
	  HVT1(I)=DTOTXT(I,1)*HV1(I)+DTOTXT(I,2)
	ENDDO
	IF(HVT0(1).EQ.HVT1(1).AND.HVT0(2).EQ.HVT1(2)) RETURN
	J=BL(IDBL)%MAGID(N0)
	IF(MAG(J)%ANGLE.NE.0) THEN
	  ITY=1
	ELSEIF(MAG(J)%K1%X.NE.0) THEN
	  ITY=2
	ELSE
	  ITY=0
	ENDIF
	U=HV1-HV0
	DU=SQRT(U(1)**2+U(2)**2)
      V(1)=-U(2)/DU*WID(ITY)/2*DTOTXT(1,1)
	V(2)=U(1)/DU*WID(ITY)/2*DTOTXT(2,1)
	IF(K.EQ.1) THEN
	  IF(ITY.EQ.0) THEN
C	    WRITE(FILE,200) HVT0,HVT1
C200       FORMAT(2(1P2D14.6,';'),'JOIN 1 TEXT')
          HVA0=(HVT0-DTOTXT(1:2,2))/DTOTXT(1:2,1)
	    HVA1=(HVT1-DTOTXT(1:2,2))/DTOTXT(1:2,1)
	    WRITE(FILE,200) HVA0,HVA1
200       FORMAT(2(1P2D14.6,';'),'JOIN 1 ')
        ELSE
	    HVA0=(HVT0+V-DTOTXT(1:2,2))/DTOTXT(1:2,1)
	    HVA1=(HVT1+V-DTOTXT(1:2,2))/DTOTXT(1:2,1)
	    HVA2=(HVT1-V-DTOTXT(1:2,2))/DTOTXT(1:2,1)
	    HVA3=(HVT0-V-DTOTXT(1:2,2))/DTOTXT(1:2,1)
C	    WRITE(FILE,220) (HVT0(I)+V(I),I=1,2),(HVT1(I)+V(I),I=1,2),
C     %      (HVT1(I)-V(I),I=1,2),(HVT0(I)-V(I),I=1,2),
C     %      (HVT0(I)+V(I),I=1,2)
C220       FORMAT(3(1P2D14.6,';'),/,2(1P2D14.6,';'),'JOIN 1 TEXT')
	    WRITE(FILE,220) HVA0,HVA1,HVA2,HVA3,HVA0
220       FORMAT(3(1P2D14.6,';'),/,2(1P2D14.6,';'),'JOIN 1 ')
        ENDIF
      ELSE
	  NC=MAG(J)%NC
	  IF(LEN(NAMFIL).EQ.0.OR.NAMFIL.EQ.' ') THEN
	    LL=ITY.NE.0
	  ELSE
	    LL=NAMEFILTER(MAG(J)%NAME(1:NC),NAMFIL)
	  ENDIF
	  IF(LL) THEN
	    VL=SQRT(V(1)**2+V(2)**2)
	    HVT0=(HVT0+HVT1)/2+(V+V/VL*CSIZE/10)*LSIDE
	    HVA0=(HVT0-DTOTXT(1:2,2))/DTOTXT(1:2,1)
	    ANG=ATAN2(U(2),U(1))*180/PI+90*LSIDE
C	    WRITE(FILE,260) HVT0,ANG,CSIZE,MAG(J)%NAME(1:NC)
C260       FORMAT(" TITLE",0P2F9.3," ANGLE",0PF7.1," SIZE",0PF7.3,
C     %      " '",A,"'")
	    WRITE(FILE,260) HVA0,ANG,CSIZE,MAG(J)%NAME(1:NC)
260       FORMAT(" TITLE",0P2F9.3," DATA ANGLE",0PF7.1," SIZE",0PF7.3,
     %      " '",A,"'")
	  ENDIF
      ENDIF
	RETURN
	END
	
	FUNCTION NAMEFILTER(NAME,FILTER)
C  Search for the string FILTER in the string NAME
C  Return TRUE if found, FALSE if not.
C  FILTER may contain the wildcard *, which can stand for
C  any sequence of characters including null.
	IMPLICIT NONE
	LOGICAL NAMEFILTER
	CHARACTER(*) NAME,FILTER
	INTEGER NC1,NC2,NC,I,I1,J,J1,J2
	LOGICAL WILD,FOUND

	NAMEFILTER=.FALSE.
	NC1=LEN(NAME)
	NC2=LEN(FILTER)
	IF(NC1.EQ.0.OR.NC2.EQ.0) RETURN
	WILD=.FALSE.
	I1=0
	J1=0
	DO J=1,NC2
	  IF(J.EQ.NC2.OR.FILTER(J:J).EQ.'*') THEN
	    IF(J1.EQ.0) THEN
	    ELSE
	      NC=J2-J1+1
	      IF(I1+NC.GT.NC1) RETURN
	      IF(WILD) THEN
	        FOUND=.FALSE.
	        DO I=I1+NC,NC1
	          IF(NAME(I-NC+1:I).EQ.FILTER(J1:J2)) THEN
	            FOUND=.TRUE.
	            I1=I
	            EXIT
	          ENDIF
	        ENDDO
	        IF(.NOT.FOUND) RETURN
	      ELSE
	        IF(NAME(I1+1:I1+NC).NE.FILTER(J1:J2)) RETURN
	        I1=I1+NC
	      ENDIF
	      J1=0
	      WILD=.FALSE.
	    ENDIF
          IF(FILTER(J:J).EQ.'*') WILD=.TRUE.
	  ELSE
	    IF(J1.EQ.0) J1=J
	    J2=J
	  ENDIF
	ENDDO
	IF(.NOT.WILD.AND.I1.NE.NC1-1) RETURN
	NAMEFILTER=.TRUE.
	RETURN
	END