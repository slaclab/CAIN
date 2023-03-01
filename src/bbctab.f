      SUBROUTINE BBCTAB(DX,DY,NCX,NCY,ACX,ACY,CF)
C  Table for direct Coulomb force
      IMPLICIT NONE
      INTEGER NCX,NCY
      REAL*8 DX,DY,ACX,ACY,CF(2,0:NCX,0:NCY)
      INTEGER I,J
      REAL*8 A,B,FAC,X,Y
      COMPLEX*16 CXY1,CXY2,C,CIPI
      REAL*8 PI/3.14159 26535 89793 238D0/
C
      CIPI=DCMPLX(0D0,PI)
      FAC=(DX+DY)**2/(DX*DY)
      A=DX/(DX+DY)/2
      B=DY/(DX+DY)/2
      DO 300 I=0,NCX
        X=I*ACX/NCX
        DO 280 J=0,NCY
          Y=J*ACY/NCY
          CXY1=DCMPLX(X-A,Y-B)
          CXY2=DCMPLX(X-A,Y+B)
          IF(X-A.GE.0) THEN
            C=CXY1*LOG(CXY1)-CXY2*LOG(CXY2)
          ELSE
            C=CXY1*(LOG(-CXY1)+CIPI)-CXY2*(LOG(-CXY2)+CIPI)
          ENDIF
          CXY1=DCMPLX(X+A,Y-B)
          CXY2=DCMPLX(X+A,Y+B)
          C=C-CXY1*LOG(CXY1)+CXY2*LOG(CXY2)
          CF(1,I,J)=-DIMAG(C)*FAC
          CF(2,I,J)=-DREAL(C)*FAC
 280    CONTINUE
 300  CONTINUE
      RETURN
      END

          
