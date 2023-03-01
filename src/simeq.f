C************************** DLUDCP **********************************
      SUBROUTINE DLUDCP(A,ND,N,ID,W,IRTN)
C  LU DECOMPOSITION FOR REAL*8
C  INPUT
C   A     MATRIX
C   ND    FIRST DIMENSION OF A
C   N     MATRIX DIMENSION
C  OUTPUT
C   IRTN  RETURN CODE
C  WORK AREA
C   ID    LENGTH N (INTEGER*4)
C   W     LENGTH N (REAL*8)
C
C  LINEAR EQUATION (BACKWARD SUBSTITUTION) IS SOLVED BY SUBR.DLUBKS.
C  ARRAY A AND ID MUST BE RETAINED UNTILL CALLING DLUBKS.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(ND,N),ID(N),W(N)
      DATA TINY/1D-14/
	INCLUDE 'include/ctrlcm.h'
C
      ISG=1
      DO 120 I=1,N
      AAMAX=0
      DO 110 J=1,N
      AA=ABS(A(I,J))
  110 IF(AA.GT.AAMAX) AAMAX=AA
      IF(AAMAX.EQ.0) GOTO 900
  120 W(I)=1/AAMAX
      DO 190 J=1,N
      DO 140 I=1,J-1
      SUM=A(I,J)
      DO 130 K=1,I-1
  130 SUM=SUM-A(I,K)*A(K,J)
  140 A(I,J)=SUM
      AAMAX=0
      DO 160 I=J,N
      SUM=A(I,J)
      DO 150 K=1,J-1
  150 SUM=SUM-A(I,K)*A(K,J)
      A(I,J)=SUM
      DUM=W(I)*ABS(SUM)
      IF(DUM.GE.AAMAX) THEN
          IMAX=I
          AAMAX=DUM
        ENDIF
  160 CONTINUE
      IF(J.NE.IMAX) THEN
          DO 170 K=1,N
          A1=A(IMAX,K)
          A(IMAX,K)=A(J,K)
  170     A(J,K)=A1
          ISG=-ISG
          W(IMAX)=W(J)
        ENDIF
      ID(J)=IMAX
c      IF(A(J,J).EQ.0) A(J,J)=TINY
      IF(A(J,J).EQ.0) GOTO 920
      IF(J.NE.N) THEN
          A1=1/A(J,J)
          DO 180 I=J+1,N
  180     A(I,J)=A(I,J)*A1
        ENDIF
  190 CONTINUE
      ID(1)=ID(1)*ISG
C        store the signature of permutation as the sign of ID(1).
      IRTN=0
      RETURN
  900 WRITE(MSGFL,910) I
  910 FORMAT(' (SUBR.DLUDCP)',I5,'-th row is zero.')
      ID(1)=0
      IRTN=100
      RETURN
  920 WRITE(MSGFL,930) 
  930 FORMAT(' (SUBR.DLUDCP) Matrix singular.')
      ID(1)=0
      IRTN=101
      RETURN
      END
C*********************** DLUBKS ***********************************
      SUBROUTINE DLUBKS(A,ND,N,ID,B)
C BACKWARD SUBSTITUTION TO FOLLOW SUBR.DLUDCP.   REAL*8
C INPUT
C   A      LU-DECOMPOSED MATRIX
C   ND     FIRST DIMENSION OF A
C   N      MATRIX DIMENSION
C   ID     ARRAY USED IN DLUDCP
C INPUT/OUTPUT
C   B      R.H.S. VECTOR./ SOLUTION     LENGTH N (REAL*8)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(ND,N),B(N),ID(N)
	INCLUDE 'include/ctrlcm.h'
C
      IF(ID(1).EQ.0) GOTO 900
      ID(1)=ABS(ID(1))
      II=0
      DO 120 I=1,N
      LL=ID(I)
      SUM=B(LL)
      B(LL)=B(I)
      IF(II.NE.0) THEN
          DO 110 J=II,I-1
  110     SUM=SUM-A(I,J)*B(J)
        ELSE IF(SUM.NE.0) THEN
          II=I
        ENDIF
  120 B(I)=SUM
      DO 140 I=N,1,-1
      SUM=B(I)
      IF(I.LT.N) THEN
          DO 130 J=I+1,N
  130     SUM=SUM-A(I,J)*B(J)
        ENDIF
  140 B(I)=SUM/A(I,I)
      RETURN
 900  WRITE(MSGFL,910)
 910  FORMAT(' (SUBR.DLUBKS) Error in the preceeding call of ',
     %  'DLUDCP.')
      RETURN
      END
