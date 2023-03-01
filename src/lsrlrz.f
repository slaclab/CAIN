      SUBROUTINE LSRLRZ(NTRANS,ITRANS,TXYS0,BG,EV,ANGLE,AXIS)
C Lorentz transformation of the laser      
C Actually, transformation of the field is not done here.
C Only accumulates the transformations for each laser
C  (Each laser may have been defined at different stage of
C   Lorentz transformation.)
	USE LASRDATA
      IMPLICIT NONE
      INTEGER NTRANS,ITRANS(NTRANS)
      REAL*8 TXYS0(0:3),BG,EV(3),ANGLE,AXIS(3)
      INCLUDE 'include/lasrcm.h'
      INTEGER II,I,J,L
      REAL*8 T(0:3,0:3),TI(0:3,0:3)
	EXTERNAL MATMUL
C
      IF(NLSR.LE.0) RETURN
      DO 100 L=1,NLSR
        LTRLSR(L)=1
 100  CONTINUE
      DO 500 II=1,NTRANS
        GOTO (200,300,400), ITRANS(II)
C  Origin shift
 200    DO 240 L=1,NLSR
          DO 220 I=0,3
            TR0LSR(I,L)=TR0LSR(I,L)-TXYS0(I)
 220      CONTINUE
 240    CONTINUE
        GOTO 500
C  Rotation
 300    CALL ROTMAT(ANGLE,AXIS,0,3,0,3,T)
        T(0,0)=1
        DO 320 I=1,3
          T(I,0)=0
          T(0,I)=0
 320    CONTINUE
        GOTO 420
C  Lorentz boost
 400    CALL LBOOST(BG,EV,T)
 420    TI(0,0)=T(0,0)
        DO 460 I=1,3
          TI(0,I)=-T(I,0)
          TI(I,0)=-T(0,I)
          DO 440 J=1,3
            TI(I,J)=T(J,I)
 440      CONTINUE
 460    CONTINUE
        DO 480 L=1,NLSR
          CALL MATVEC(T,4,4,TR0LSR(0,L))
          CALL MATMUL(4,T,4,TRLSR(0,0,L),4,TRLSR(0,0,L),4)
          CALL MATMUL(4,TRILSR(0,0,L),4,TI,4,TRILSR(0,0,L),4)
 480    CONTINUE
 500  CONTINUE
      RETURN
      END

