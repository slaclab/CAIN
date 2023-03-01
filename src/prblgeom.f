	SUBROUTINE PRBLGEOM(BLNAME,XYS0,E10,E30,FILE,IRTN)
C  Print geometry of beamline
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
	REAL(8) XYS0(3),E10(3),E30(3)
	INTEGER IDBL,JJ,I
	REAL(8) XYS(3),S,SUM1,SUM3,SUM31,APERT1(2)
	REAL(8) E1(3),E2(3),E3(3),EE(3,3)
	EQUIVALENCE (EE(1,1),E1(1)),(EE(1,2),E2(1)),(EE(1,3),E3(1))
	CHARACTER(MAXMAGNAME) NAM
	CHARACTER(21) LLEN
	CHARACTER(5) FTOC5
      INCLUDE 'include/ctrlcm.h'
	REAL(8) PI/3.141592653589793238D0/

	CALL STARTBL(BLNAME,IDBL,APERT1)
	IF(IDBL.LE.0) GOTO 910
	XYS=XYS0
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
	WRITE(FILE,160) BLNAME,(I,I,I,I=1,3)
160   FORMAT(' ***** Geometry of Beamline "',A,'" *****',/,
     %   '  S:        length along the beamline,   L: magnet length',/,
     %   '  ang:      bend angle (rad),   rot:  rotation angle (deg)',/,
     %   '  x,y,s:    CAIN coord at magnet entrance (m)',/,
     %   '  apx,apy:  aperture (mm)',/,
     %   '  e1,e2,e3: beamline axis in CAIN coordinate (times 10000',
     %       ' if not +-1)',/,
     %   '    name    S(m)     L(m)  ang(rad) rot   x(m)    y(m) ',
     %   '    s(m)  apx apy',1X,
     %     3(1X,' e',I1,'x ',' e',I1,'y ',' e',I1,'s ',1X))
	S=0
	E2(1)=E3(2)*E1(3)-E3(3)*E1(2)
	E2(2)=E3(3)*E1(1)-E3(1)*E1(3)
	E2(3)=E3(1)*E1(2)-E3(2)*E1(1)
300	CALL NEXTMAG(JJ,APERT1)
	IF(JJ.LT.0) THEN
	  IRTN=1010+ABS(JJ)
	  GOTO 1000
	ELSEIF(JJ.EQ.0) THEN
	  NAM=' (exit)'
	  LLEN=' '
	ELSE
	  NAM=MAG(JJ)%NAME
	  WRITE(LLEN,'(F8.4,F8.5,F5.0)') MAG(JJ)%LENGTH%X,MAG(JJ)%ANGLE,
     %     MAG(JJ)%ROTATE*180/PI
	ENDIF
	WRITE(FILE,400) NAM,S,LLEN,XYS,(1000*APERT1(I),I=1,2),
     %  (FTOC5(E1(I)),I=1,3),(FTOC5(E2(I)),I=1,3),(FTOC5(E3(I)),I=1,3)
400   FORMAT(1X,A,F9.4,A,2F8.4,F9.4,2F4.0,1X,3('(',3A5,')'))
	IF(JJ.EQ.0) GOTO 600
	CALL TRKBLGEO(MAG(JJ)%LENGTH%X,MAG(JJ)%ANGLE,
     %   MAG(JJ)%ROTATE,S,XYS,EE)
	GOTO 300
600   IRTN=0
	GOTO 1000

910	IRTN=1001
	IF(MSGLVL.GE.0) WRITE(MSGFL,915) BLNAME
915   FORMAT(' (SUBR.PRBLGEOM) Beamline "',A,'" does not exist.')
      GOTO 1000
920	IRTN=1002
	IF(MSGLVL.GE.0) WRITE(MSGFL,925) BLNAME
925   FORMAT(' (SUBR.PRBLGEOM) Invalid E3 and/or E1 vector.')
      GOTO 1000

1000  RETURN
	END

	FUNCTION FTOC5(X)
C  Floating number -1<=x<=1 to a string of 5 characters
C      e.g.,   0.123456  ->  +1235
C             -0.54321   ->  -5432
C              -1        ->  -1.0
	IMPLICIT NONE
	REAL(8) X
	CHARACTER(5) FTOC5
	INTEGER L,I
	CHARACTER(15) TEXT
	CHARACTER(1) NN(0:9)/'0','1','2','3','4','5','6','7','8','9'/

	L=NINT(X*10000D0)
	IF(L.GT.0) THEN
	  FTOC5(1:1)='+'
	ELSE
	  FTOC5(1:1)='-'
	ENDIF
	L=ABS(L)
	IF(L.EQ.0) THEN
	  FTOC5=' 0   '
	ELSEIF(L.LT.10000) THEN
	  DO I=5,2,-1
	    FTOC5(I:I)=NN(MOD(L,10))
	    L=L/10
	  ENDDO
	ELSEIF(L.LT.10002) THEN
	  FTOC5(2:5)='1.0 '
	ELSE
	  FTOC5(2:5)='****'
	ENDIF
	RETURN
	END
