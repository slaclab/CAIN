	SUBROUTINE ARRIND2N(IND,RANK,DIM,N,IRTN)
C  Array index (i1,i2...)  --> n
C    IRTN= 0:  normal
C        100:  invalid subscript range
C   Assume RANK and DIM are correct
	IMPLICIT NONE
	INTEGER RANK,IND(RANK),DIM(3,RANK),N,IRTN
	INTEGER I

	N=0
	DO I=RANK,1,-1
	  IF(IND(I).LT.DIM(1,I).OR.IND(I).GT.DIM(2,I)) THEN
	    IRTN=100
	    RETURN
	  ENDIF
	  N=IND(I)-DIM(1,I)+N*DIM(3,I)
	ENDDO
	N=N+1
	IRTN=0
	RETURN
	END

	SUBROUTINE ARRN2IND(IND,RANK,DIM,N,IRTN)
C  Array   n  -> index (i1,i2...) 
C    IRTN= 0:  normal
C        100:  invalid subscript range
C   Assume RANK and DIM are correct
	IMPLICIT NONE
	INTEGER RANK,IND(RANK),DIM(3,RANK),N,IRTN
	INTEGER I,N1

      IRTN=100
      IF(N.LE.0) RETURN
      N1=N-1
	DO I=1,RANK
	  IND(I)=MOD(N1,DIM(3,I))+DIM(1,I)
	  IF(IND(I).GT.DIM(2,I)) RETURN
	  N1=N1/DIM(3,I)
	ENDDO
	IRTN=0
	RETURN
	END
