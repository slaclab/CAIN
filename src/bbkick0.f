      SUBROUTINE BBKICK0(L,DS,KIN,XYS,EP,FLD,FF,NOUT,NXY,BBQ)
C-- Assume the following beam-beam field, which is valid
C   when the main part of the beam is almost parallel to the s-sxis
C       Magnetic field =  sgn*es \times (Electric field)
C         es= unit vector along s-axis
C         sgn=1 when the beam creating the field is right-going,
C            -1 when left-going.
C   L  :  1(2) when the particle receiving the kick is right(left)
C         going.
C   FLD:  electric/magnetic field (V/m)
C   FF :  Lorentz force (V/m)
      IMPLICIT NONE
      INTEGER L,KIN,NOUT,NXY(2)
      REAL*8 DS,XYS(3),EP(0:3),FLD(3,2),FF(3),BBQ(*)
      INTEGER L2,IOUT(1)
      REAL*8 FAC0,FXY(2),SGN
      INCLUDE 'include/cnstcm.h'
C
      L2=3-L
      CALL BBKCK(L,1,XYS,FXY,NOUT,NXY(1),NXY(2),BBQ,0,IOUT)
      IF(L2.EQ.1) THEN
        SGN=1
      ELSE
        SGN=-1
      ENDIF
      FAC0=-2*MASS(2)*RE/DS
      FLD(1,1)=FAC0*FXY(1)
      FLD(2,1)=FAC0*FXY(2)
      FLD(3,1)=0
      FLD(1,2)=-SGN*FAC0*FXY(2)
      FLD(2,2)=SGN*FAC0*FXY(1)
      FLD(3,2)=0
      IF(KIN.EQ.1) RETURN
      FF(1)=CHARGE(KIN)*(FLD(1,1)-EP(3)/EP(0)*FLD(2,2))
      FF(2)=CHARGE(KIN)*(FLD(2,1)+EP(3)/EP(0)*FLD(1,2))
      FF(3)=CHARGE(KIN)*(EP(1)*FLD(2,2)-EP(2)*FLD(1,2))/EP(0)
      RETURN
      END
