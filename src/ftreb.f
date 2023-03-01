      SUBROUTINE FTREB(EP,EB,FT)
C   Compute transverse component of the Lorentz force (eV/m)
C   (sign of charge must be multiplied)
C   The upsilon parameter is given by
C       Abs(FT)*(Compton wavelength[m])*Energy[eV]/mass[eV]^2
      IMPLICIT NONE
      REAL*8 EP(0:3),EB(3,2),FT(3)
      INTEGER I
      REAL*8 PP,FP
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
C
      PP=EP(1)**2+EP(2)**2+EP(3)**2
      FP=(EP(1)*EB(1,1)+EP(2)*EB(2,1)
     %     +EP(3)*EB(3,1))/PP
      DO 200 I=1,3
        FT(I)=EB(I,1)-EP(I)*FP
     %       +(EP(I2(I))*EB(I3(I),2)
     %       -EP(I3(I))*EB(I2(I),2))/EP(0)
 200  CONTINUE
      RETURN
      END
