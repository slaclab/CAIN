	SUBROUTINE TMATRIX(L,ANG,K1,ROT,TEDGE,T,IRTN)
C   6*6 transfer matrix
C   IRTN  0:  normal
C         1:  coupled element found (but the matrix computed)
C     >=100:  matrix not computed
	IMPLICIT NONE
	INTEGER IRTN
	REAL(8) L,ANG,K1,ROT,TEDGE(2),T(6,6)
      INCLUDE 'include/ctrlcm.h'
	INTEGER I,J,I1,I2
	REAL(8) T0(6,6),C,S,SH,CC,SS,CS,X,RHO,EDG

	CALL UNIT8(T,6,6)
	IF(K1.NE.0) THEN
	  IF(ANG.NE.0) THEN
	    IRTN=100
	    IF(MSGLVL.GE.0) WRITE(MSGFL,220)
220       FORMAT(' (SUBR.BLOPTICS) Combined function bend not ready.')
          RETURN
	  ENDIF
	  IF(L.EQ.0) THEN
C   Thin lens quad
	    T(2,1)=-K1
	    T(4,3)=K1
	  ELSE
C   Thick lens quad
	    X=K1*L
	    IF(X.GE.0) THEN
	      I1=1
	      I2=3
	    ELSE
	      I1=3
	      I2=1
	    ENDIF
	    X=SQRT(ABS(X))
	    S=SIN(X)
	    T(I1,I1)=COS(X)
	    T(I1+1,I1+1)=T(I1,I1)
	    T(I1,I1+1)=S/X*L
	    T(I1+1,I1)=-S*X/L
	    SH=SINH(X)
	    T(I2,I2)=COSH(X)
	    T(I2+1,I2+1)=T(I2,I2)
	    T(I2,I2+1)=SH/X*L
	    T(I2+1,I2)=SH*X/L
	  ENDIF
	ELSEIF(ANG.NE.0) THEN
	  IF(L.EQ.0) THEN
C  Thin lens bend
	    T(2,6)=ANG
	    T(5,1)=ANG
	  ELSE
C  Thick lens bend
          RHO=L/ANG
	    T(1,1)=COS(ANG)
	    T(2,2)=T(1,1)
	    S=SIN(ANG)
	    T(1,2)=S*RHO
	    T(2,1)=-S/RHO
	    T(3,4)=RHO*S
	    T(1,6)=2*RHO*SIN(ANG/2)**2
	    T(2,6)=S
	    T(5,1)=S
	    T(5,2)=T(1,6)
	    T(5,6)=RHO*(ANG-S)
	    IF(TEDGE(1).NE.0) THEN
	      EDG=TEDGE(1)/RHO
	      DO I=1,6
	        T(I,1)=T(I,1)+EDG*T(I,2)
	        T(I,3)=T(I,3)-EDG*T(I,4)
	      ENDDO
	    ENDIF
	    IF(TEDGE(2).NE.0) THEN
	      EDG=TEDGE(2)/RHO
	      DO J=1,6
	        T(2,J)=T(2,J)+EDG*T(1,J)
	        T(4,J)=T(4,J)-EDG*T(3,J)
	      ENDDO
	    ENDIF
	  ENDIF
	ELSEIF(L.NE.0) THEN
C  Drift
	  T(1,2)=L
	  T(3,4)=L
	ENDIF
	IRTN=0
	IF(ROT.NE.0) THEN
C        Tnew = ( c -s ) * Told * ( c  s)
C               ( s  c )          (-s  c)
C          i.e., ROT is the rotation angle of the magnet, not the
C          rotation of the x-y axis.
	  C=COS(ROT)
	  S=SIN(ROT)
	  IF(ABS(C).GE.1D-8.AND.ABS(S).GE.1D-8) THEN
		  IF(ANG.NE.0.OR.K1.NE.0) IRTN=1
	  ENDIF
	  DO I=1,6
	    DO J=1,6
	      T0(I,J)=0
	    ENDDO
	    T0(I,I)=1
	    IF(I.LE.4) T0(I,I)=C
	  ENDDO
	  T0(1,3)=S
	  T0(2,4)=S
	  T0(3,1)=-S
	  T0(4,2)=-S
	  CALL MATMUL(6,T0,6,T,6,T,6)
	  T0(1,3)=-S
	  T0(2,4)=-S
	  T0(3,1)=S
	  T0(4,2)=S
	  CALL MATMUL(6,T,6,T0,6,T,6)
	ENDIF
	RETURN
	END
