      SUBROUTINE BBELLP(NMOM,LEL,EU0,EL,QMOM,NP,IFLG,FXY)
C Elliptic expansion
      IMPLICIT NONE
      INTEGER NMOM,LEL,NP,IFLG(NP)
      REAL*8 EU0,EL,FXY(2,NP)
      COMPLEX*16 QMOM(0:NMOM)
      INTEGER N,NM
      REAL*8 XYX,XYY
      COMPLEX*16 CZ,CF
C
      CALL CPUTIM('BBELLP',1)
      DO 300 N=1,NP
        IF(IFLG(N).NE.3) GOTO 300
        IF(LEL.GE.0) THEN
          XYX=FXY(1,N)/EL
          XYY=FXY(2,N)/EL
        ELSE
          XYX=FXY(2,N)/EL
          XYY=FXY(1,N)/EL
        ENDIF
        CZ=DCMPLX(XYX,XYY)
        CF=SQRT(CZ**2-1D0)
        IF(DREAL(CZ)*DREAL(CF)+DIMAG(CZ)*DIMAG(CF).LT.0) CF=-CF
        CZ=EU0/(CZ+CF)
        CF=QMOM(NMOM)
        IF(NMOM.GE.1) THEN
          DO 220 NM=NMOM-1,0,-1
            CF=CF*CZ+QMOM(NM)
 220      CONTINUE
        ENDIF
        CF=CF*CZ
        IF(LEL.GE.0) THEN
          FXY(1,N)=-DREAL(CF)
          FXY(2,N)=+DIMAG(CF)
        ELSE
          FXY(2,N)=-DREAL(CF)
          FXY(1,N)=+DIMAG(CF)
        ENDIF
 300  CONTINUE
      CALL CPUTIM('BBELLP',2)
      RETURN
      END
