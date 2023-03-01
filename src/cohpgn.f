      SUBROUTINE COHPGN(EPPH,TXYSPH,FT,DT,PMAX,WENH,
     %    LPH,LPAIR,EPEP,TXYSEP,ISPIN,EV,STOKES,SPINEP,CHI,PROB,IRTN)
C  Generate coherent pair.
C    Angle distribution is not calculated.
C  Input:
C    EPPH  Initial photon energy-momentum (eV)
C    TXYSPH  Initial photon coordinate (m)
C    FT    Transverse electric field (V/m) including the
C          contribution of the magnetic Lorentz force.
C    DT    Time interval (meter)
C    PMAX  Maximum probability per one step.
C    WENH   Event rate enhancement factor.
C    ISPIN  Flag for polarization
C    EV     EV(*,1)  unit vector along transverse acceleration
C                    (transverse component of Lorentz force)
C                    for positron running along EV(*,3)
C                    with infinite energy.
C           EV(*,3)  unit vector along the 3-momentum of
C                    initial photon.
C           EV(*,2)  = EV(*,3) x EV(*,1)
C           EV is needed only when ISPIN is on.
C    STOKES   Stokes parameter of the initial photon based
C           on the basis EV.
C    WENH  Event rate enhancement factor.
C  Output:
C    LPH    0: No pair creation. No change of Stokes parameters.
C           1: Stokes parameters revised.
C           2: Photon eliminated.
C    LPAIR  0: No pair data stored.
C           1: One pair data stored.
C    EPEP(*,k)    Energy-momentum of the created 
C           electron (k=1) and positron (k=2)
C    TXYSEP(*,k)  Coordinate of the created electron and positron
C    SPINEP(*,k)   Spin vector of electron and positron.
C    CHI   Chi parameter.
C    PROB  Probability in the given time interval.
C          If this is too large, there is a statistical problem.
C    IRTN  Return code.
C          0: normal
C          1000: Prob > PMAX
      IMPLICIT NONE
      INTEGER LPH,LPAIR,ISPIN,IRTN
      REAL*8 EPPH(0:3),TXYSPH(0:3),FT,DT,PMAX,WENH,
     %   EPEP(0:3,2),TXYSEP(0:3,2),EV(3,3),STOKES(3),SPINEP(3,2),
     %   CHI,PROB
      REAL*8 CMPTNWL/3.86159323D-13/,EMASS/0.51099906D6/
      INTEGER K,I
      REAL*8 FT1,XPAIR(2),DT1
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
C
      IRTN=0
      LPH=0
      LPAIR=0
	PROB=0    !   after David Asner  Sep.9.2001
      IF(DT.LE.0) RETURN
      FT1=FT/EMASS
      CHI=CMPTNWL*(EPPH(0)/EMASS)*FT1
      CALL CPRND2(DT,EPPH(0),CHI,XPAIR,ISPIN,EV,STOKES,SPINEP,
     %       WENH,PROB,LPH,LPAIR)
      IF(PROB.GE.PMAX) THEN
        IRTN=1000
        RETURN
      ENDIF
      IF(LPAIR.GE.1) THEN
        DT1=DT*RANDCAIN()
        DO 240 K=1,2
          DO 220 I=1,3
            EPEP(I,K)=XPAIR(K)*EPPH(I)
 220      CONTINUE
          EPEP(0,K)=SQRT((XPAIR(K)*EPPH(0))**2+EMASS**2)
C            XPAIR should be the fraction of energy rather than
C            that of momentum. Here we take the latter
C            because the subroutine CPRND2 may return too small
C            XPAIR in a rare case.
 240    CONTINUE
        DO 260 I=0,3
          TXYSEP(I,1)=TXYSPH(I)+EPPH(I)/EPPH(0)*DT1
          TXYSEP(I,2)=TXYSEP(I,1)
 260    CONTINUE
      ENDIF
      IRTN=0
      RETURN
      END
