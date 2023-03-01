	SUBROUTINE OPTICSINT(JJ,TWISS,NDIV,IRTN)
C  Interpolate twiss parameter in Magnet ID=JJ
C  TWISS(*,0)  entrance (input)
C  TWISS(*,NDIV) exit
	USE BEAMLN
	IMPLICIT NONE
	INTEGER JJ,NDIV,IRTN
	REAL(8) TWISS(MTWISS,0:NDIV)
	INTEGER N,I,J
	REAL(8) L,K1,ANG,ROT,TED(2),T(6,6),B1,A1,G1,EP(4)
	INCLUDE 'include/ctrlcm.h'
	REAL(8) PI/3.141592653589793238D0/
	
	DO N=1,NDIV
	  L=MAG(JJ)%LENGTH%X/NDIV
	  K1=MAG(JJ)%K1%X/NDIV
	  ANG=MAG(JJ)%ANGLE/NDIV
	  ROT=MAG(JJ)%ROTATE
	  TED=MAG(JJ)%TEDGE
	  IF(N.NE.1) TED(1)=0
	  IF(N.NE.NDIV) TED(2)=0
	  CALL TMATRIX(L,ANG,K1,ROT,TED,T,IRTN)
	  IF(IRTN.NE.0) THEN
	    IRTN=1009
          IF(IRTN.EQ.1) THEN
	     IF(MSGLVL.GE.0) WRITE(MSGFL,260) MAG(JJ)%NAME(1:MAG(JJ)%NC)
260        FORMAT(' (SUBR.OPTICSINT) Coupled element "',A,'" found. ',
     %        ' CAIN not ready.')
	    ELSEIF(IRTN.GE.100) THEN
	     IF(MSGLVL.GE.0) WRITE(MSGFL,280) MAG(JJ)%NAME(1:MAG(JJ)%NC)
280        FORMAT(' (SUBR.OPTICSINT) BLOPTICS not ready for element "',
     %        A,'"')
	    ENDIF
          GOTO 1000
	  ENDIF
	  DO J=1,2
	    I=0
	    IF(J.EQ.2) I=2
	    B1=TWISS(1+5*(J-1),N-1)
	    A1=TWISS(2+5*(J-1),N-1)
	    EP(2*J-1)=TWISS(3+5*(J-1),N-1)
	    EP(2*J)=TWISS(4+5*(J-1),N-1)
	    G1=(1+A1**2)/B1
	    TWISS(1+5*(J-1),N)=T(I+1,I+1)**2*B1
     %        -2*T(I+1,I+1)*T(I+1,I+2)*A1+T(I+1,I+2)**2*G1
	    TWISS(2+5*(J-1),N)=-T(I+1,I+1)*T(I+2,I+1)*B1
     %      +(1+2*T(I+1,I+2)*T(I+2,I+1))*A1
     %      -T(I+1,I+2)*T(I+2,I+2)*G1
	    TWISS(5+5*(J-1),N)=TWISS(5+5*(J-1),N-1)
     %       +ATAN2(T(I+1,I+2),T(I+1,I+1)*B1-T(I+1,I+2)*A1)/(2*PI)
	  ENDDO
	  CALL MATVEC(T,6,4,EP)
	  DO I=1,4
	    EP(I)=EP(I)+T(I,6)
	  ENDDO
	  DO J=1,2
	    TWISS(3+5*(J-1),N)=EP(2*J-1)
	    TWISS(4+5*(J-1),N)=EP(2*J)
	  ENDDO
	ENDDO
	IRTN=0
1000	RETURN
	END