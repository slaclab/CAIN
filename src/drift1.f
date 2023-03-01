      SUBROUTINE DRIFT1(T1,N1,N20,ISPIN)
C  Drift to T=T1. 
C  To be called in PUSH loop, not from DRIFT EXTERNAL command.
C  Take into account external field exept for those already kicked 
C  by beam-beam. (External field effects are already calculated
C  together with beam-beam.)
	USE BEAMCM
      IMPLICIT NONE
      INTEGER N1,N20,ISPIN
      REAL*8 T1
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/extfcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER N,I,N2,KIN
      REAL*8 DT,SGN,V(3),ANOM1,ANOME
      EXTERNAL ANOME
	INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
	INTEGER K,NDIV,IDIV
	REAL(8) P0(3),DP(3),DX(3),E1,FMAX,DTN
	REAL(8) H(4)/1D0,0.5D0,0.5D0,1D0/,W(4)/1D0,2D0,2D0,1D0/
C
      IF(N20.EQ.0) THEN
        N2=NP
      ELSE
        N2=N20
      ENDIF
      IF(N2-N1.LT.0) RETURN
      CALL CPUTIM('DRIFT1',1)
      DO N=N1,N2
        IF(LOST(N).NE.0) CYCLE
        DT=T1-TXYS(0,N)
        IF(LEXTF.LE.0.OR.(LBBFIN(N).GE.1.OR.KIND(N).EQ.1)) THEN
	    DO I=1,3
            TXYS(I,N)=TXYS(I,N)+EP(I,N)/EP(0,N)*DT
	    ENDDO
	    TXYS(0,N)=T1
	  ELSE
          KIN=KIND(N)
          SGN=CHARGE(KIN)
C   Runge-Kutta introduced (Jan.31.2002)
          FMAX=0
	    DO I=1,3
	      FMAX=MAX(FMAX,ABS(FLD(I,1,N)),ABS(FLD(I,2,N)))
	    ENDDO
          NDIV=MAX(1,INT(DT*FMAX/EP(0,N)/0.1D0))
	    DTN=DT/NDIV
	    DO IDIV=1,NDIV
	      P0=EP(1:3,N)
	      DP=0
	      DO K=1,4
	        V=P0+H(K)*DP
	        E1=SQRT(MASS(KIN)**2+V(1)**2+V(2)**2+V(3)**2)
	        V=V/E1
	        DX=V*DTN
	        DO I=1,3
	          DP(I)=DTN*SGN*(FLD(I,1,N)
     %             +V(I2(I))*FLD(I3(I),2,N)-V(I3(I))*FLD(I2(I),2,N))
	        ENDDO
	        TXYS(1:3,N)=TXYS(1:3,N)+W(K)*DX/6D0
	        EP(1:3,N)=EP(1:3,N)+W(K)*DP/6D0
	      ENDDO
            EP(0,N)=SQRT(MASS(KIN)**2
     %          +EP(1,N)**2+EP(2,N)**2+EP(3,N)**2)
	    ENDDO
	    TXYS(0,N)=T1
          IF(ISPIN.GE.1.AND.KIN.NE.1) THEN
            ANOM1=ANOME(FLD(1,1,N),EP(0,N),2)
            CALL BMTEQ(EP(0,N),SGN,MASS(KIN),ANOM1,FLD(1,1,N),
     %          DT,SPIN(1,N))
          ENDIF
        ENDIF
        LBBFIN(N)=1
      ENDDO
      CALL CPUTIM('DRIFT1',2)
      RETURN
      END

