      SUBROUTINE BBKKMM(NP,IFLG,XY,XYCM,NX,NY,DX,DY,
     %       MMOM,NMOM,LEL,R00,EU0,CHU0,SHU0,EL,QMOM,MX,MY,Q,FXY)
      IMPLICIT NONE
      INTEGER NP,IFLG(NP),NX,NY,MMOM,NMOM,LEL,MX,MY
      REAL*8 XY(2,NP),XYCM(2),DX,DY,R00,EU0,CHU0,SHU0,EL,
     %     Q(MX,MY),FXY(2,NP)
      COMPLEX*16 QMOM(0:MMOM,2)
      INTEGER NM,N,K,I,J
      REAL*8 XY1(2),R002,XYX,XYY,X1,Y1,X10,Y10,RR
      COMPLEX*16 CZ,CF
C
      IF(LEL.EQ.0) THEN
        DO 200 N=1,NP
          IF(IFLG(N).GT.0) THEN
            FXY(1,N)=0
            FXY(2,N)=0
          ENDIF
 200    CONTINUE
        RETURN
      ENDIF
      R002=R00**2
      X10=0.5D0*(NX+1)*DX
      Y10=0.5D0*(NY+1)*DY
      DO 300 N=1,NP
        IF(IFLG(N).LE.0) GOTO 300
        XY1(1)=XY(1,N)-XYCM(1)
        XY1(2)=XY(2,N)-XYCM(2)
        IF(XY1(1)**2+XY1(2)**2.GT.R002) THEN
C Harmonic expansion
          CZ=1/DCMPLX(XY1(1)/R00,XY1(2)/R00)
          K=1
        ELSE
          IF(LEL.EQ.1) GOTO 240
C Elliptic expansion
          IF(LEL.GE.0) THEN
            XYX=XY1(1)/EL
            XYY=XY1(2)/EL
          ELSE
            XYX=XY1(2)/EL
            XYY=XY1(1)/EL
          ENDIF
          IF((XYX/CHU0)**2+(XYY/SHU0)**2.LE.1) GOTO 240
          CZ=DCMPLX(XYX,XYY)
          CF=SQRT(CZ**2-1D0)
          IF(DREAL(CZ)*DREAL(CF)+DIMAG(CZ)*DIMAG(CF).GE.0) THEN
            CZ=1/(CZ+CF)
          ELSE
            CZ=1/(CZ-CF)
          ENDIF
          CZ=CZ*EU0
          K=2
        ENDIF
        CF=QMOM(NMOM,K)
        IF(NMOM.GE.K) THEN
          DO 220 NM=NMOM-1,0,-1
            CF=CF*CZ+QMOM(NM,K)
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
        GOTO 300
C Direct Coulomb force
 240    FXY(1,N)=0
        FXY(2,N)=0
        DO 280 I=1,NX
          X1=I*DX-X10-XY1(1)
          DO 260 J=1,NY
            Y1=J*DY-Y10-XY1(2)
            IF(Q(I,J).NE.0) THEN
              RR=X1**2+Y1**2
              FXY(1,N)=FXY(1,N)+Q(I,J)*X1/RR
              FXY(2,N)=FXY(2,N)+Q(I,J)*Y1/RR
            ENDIF
 260      CONTINUE
 280    CONTINUE
 300  CONTINUE
      RETURN
      END
