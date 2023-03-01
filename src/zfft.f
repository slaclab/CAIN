C**************************ZFFT ***************************************
      SUBROUTINE ZFFT(A,NN,ISGN)
C  FFT routine for Complex*16 array with arbitrary number of elements.
C      B(k)= Sum A(j)* exp[+-2*pi*i*k*j/NN]   over j=0 to NN-1
C                     for k=0 to NN-1
C Input:
C  A     Complex*16 array of dimension A(0:NN-1)
C  NN    Length of A. (>=1)
C  ISGN  Sign of the phase, +1 or -1.
C Output:
C  A     B is stored in the same area as A.
C Restriction:
C  * NN must not contain a prime factor larger than the MPRIME-th
C    prime number. This is only for the work area WORK(MPRIME).
C  * When NN has two (or more) different prime factors, NN must be
C    less than MW defined in subroutine ZFFTTR.  This is the work
C    area to transpose a rectangular matrix. This problem can be
C    eliminated, if an efficient algorithm of transposing "in place",
C    or if dynamic memory allocation is allowed.
C
      IMPLICIT NONE
      INTEGER MPRIME
      PARAMETER (MPRIME=26)
      INTEGER NN,ISGN
      INTEGER I,J,KK,L,M,N,N1,K,KPRIME
      COMPLEX*16 A(0:NN-1)
      INTEGER PRIME(MPRIME)/2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,
     %    53,59,61,67,71,73,79,83,89,97,101/
      COMPLEX*16 WORK(MPRIME)
      COMPLEX*16 W,W0,WP
      REAL*8 THETA
      REAL*8 PI2/6.28318 53071 79586 477D0/
	INCLUDE 'include/ctrlcm.h'
C
      IF(NN.LE.1) GOTO 900
      IF(ABS(ISGN).NE.1) GOTO 920
      M=NN
      KPRIME=1
 200  N1=M
      KK=PRIME(KPRIME)
 220  IF(MOD(N1,KK).EQ.0) THEN
        N1=N1/KK
        GOTO 220
      ELSE
        L=NN/M
        N=M/N1
        M=N1
C         NN=L*N*M
C         L: factor already FFTed
C         N: factor to be FFTed in this step. A power of prime KK
C         M: factor not FFTed yet.
        IF(N.NE.1) THEN
          CALL ZFFT0(A,L*M,N,KK,ISGN,WORK)
          IF(M.NE.1) THEN
            W0=1
            THETA=PI2/(ISGN*M*N)
            WP=DCMPLX(COS(THETA),SIN(THETA))
c            WP=DCMPLX(-2D0*SIN(THETA/2D0)**2,SIN(THETA))
            DO 260 J=0,M-1
              W=1
              DO 240 K=0,N-1
                DO 230 I=0,L-1
 230            A(I+L*(J+M*K))=W*A(I+L*(J+M*K))
                W=W0*W
 240          CONTINUE
c              W0=W0*WP+W0
              W0=W0*WP
 260        CONTINUE
            CALL ZFFTTR(A,L,M,N)
          ENDIF
        ENDIF
        IF(M.NE.1) THEN
          KPRIME=KPRIME+1
          IF(KPRIME.GT.MPRIME) GOTO 940
          GOTO 200
        ENDIF
      ENDIF
      RETURN
 900  IF(NN.LE.0) WRITE(MSGFL,910) NN
 910  FORMAT(' (SUBR.ZFFT) Invalid 2nd argument n=',I4,
     %  '. Must be >=1.')
      RETURN
 920  WRITE(MSGFL,930) ISGN
 930  FORMAT(' (SUBR.ZFFT) Invalid 3rd argument ISGN=',I4,
     %  '. Must be 1 or -1.')
      RETURN
 940  WRITE(MSGFL,950) NN
 950  FORMAT(' (SUBR.ZFFT) 2nd argument',I8,' contains too large ',
     %  'a prime factor.')
      RETURN
      END
C-------------------------- ZFFT0 --------------------------------------
      SUBROUTINE ZFFT0(A,M,N,K,ISGN,WORK)
C Fast Fourier Transformation of arbitrary base to be called by ZFFT.
C    B(l,k)= Sum A(l,j)*exp[+-2*pi*i*k*j/N]   over j=0 to N-1
C             for k=0 to N-1, l=1 to M.
C Input:
C   A    Complex*16 array of dimension A(M,0:N-1)
C   M    First dimension of A.
C   N    Length of Fourier transformation. 
C         N= K**(positive integer). (Not checked for.)
C   K    base.  >=2. (need not be a prime)
C   ISGN +1 or -1, sign of the phase.
C Work area
C   WORK Complex*16 array of length K.
C Output:
C   A    B(l,k) is stored in the same place as A.
C   
      IMPLICIT NONE
      INTEGER M,N,K,ISGN
      COMPLEX*16 A(M,0:N-1)
      INTEGER M1,N1,I,J,NN,IS,K1,L
      COMPLEX*16 T,W,WP,W0,WP0,W2,WORK(0:K-1)
      REAL*8 THETA,SGN
      REAL*8 PI2/6.28318 53071 79586 477D0/
      REAL*8 C31/0.86602 54037 84438 64676D0/,
     %       C51/0.30901 69943 74947 42410D0/,
     %       C52/0.95105 65162 95153 57212D0/,
     %       C53/-.80901 69943 74947 42410D0/,
     %       C54/0.58778 52522 92473 12917D0/
      COMPLEX*16 X0,X1,X2,X3,X4,X5,X6,X7,X8
C
      J=0
      K1=K-1
      NN=N/K*K1
      DO 120 I=0,N-1
        IF(J.GT.I) THEN
          DO 110 M1=1,M
          T=A(M1,J)
          A(M1,J)=A(M1,I)
 110      A(M1,I)=T
        ENDIF
C  Bit inversion in K-scale
        N1=NN
        DO WHILE (N1.GE.K1.AND.J.GE.N1)
          J=J-N1
          N1=N1/K
        ENDDO
        J=J+N1/K1
 120  CONTINUE
C  Following statements apply for any K>=2, but in
C  order for the computing time, the cases K<=5 are
C  written separately.
      NN=1
      SGN=DFLOAT(ISGN)
      IF(K.GE.6) THEN
        THETA=PI2/(ISGN*K)
c        WP0=DCMPLX(-2D0*SIN(THETA/2D0)**2,SIN(THETA))
        WP0=DCMPLX(COS(THETA),SIN(THETA))
      ENDIF
      DO WHILE (NN.LT.N)
        IS=K*NN
        THETA=PI2/(ISGN*IS)
c        WP=DCMPLX(-2D0*SIN(THETA/2D0)**2,SIN(THETA))
        WP=DCMPLX(COS(THETA),SIN(THETA))
        W=1
        DO 300 N1=0,NN-1
          DO 280 M1=1,M
            DO 260 I=N1,N1+N-1,IS
            IF(K.EQ.2) THEN
              T=W*A(M1,I+NN)
              A(M1,I+NN)=A(M1,I)-T
              A(M1,I)=A(M1,I)+T
            ELSEIF(K.EQ.3) THEN
              X0=A(M1,I)
              X1=W*A(M1,I+NN)
              X2=W**2*A(M1,I+2*NN)
              X3=X1+X2
              X4=DCMPLX(0D0,SGN*C31)*(X1-X2)
              X5=X0-0.5D0*X3
              A(M1,I)=X0+X3
              A(M1,I+NN)=X5+X4
              A(M1,I+2*NN)=X5-X4
            ELSEIF(K.EQ.4) THEN
              W2=W**2
              X1=W*A(M1,I+NN)
              X2=W2*A(M1,I+2*NN)
              X3=W2*W*A(M1,I+3*NN)
              X4=A(M1,I)+X2
              X5=A(M1,I)-X2
              X6=X1+X3
              X7=DCMPLX(0D0,SGN)*(X1-X3)
              A(M1,I)=X4+X6
              A(M1,I+NN)=X5+X7
              A(M1,I+2*NN)=X4-X6
              A(M1,I+3*NN)=X5-X7
            ELSEIF(K.EQ.5) THEN
              W2=W**2
              X1=W*A(M1,I+NN)
              X2=W2*A(M1,I+2*NN)
              X3=W2*W*A(M1,I+3*NN)
              X4=W2*W2*A(M1,I+4*NN)
              X0=A(M1,I)
              X5=X1+X4
              X6=DCMPLX(0D0,SGN)*(X1-X4)
              X7=X2+X3
              X8=DCMPLX(0D0,SGN)*(X2-X3)
              A(M1,I)=X0+X5+X7
              X1=X0+C51*X5+C53*X7
              X2=X0+C53*X5+C51*X7
              X3=C52*X6+C54*X8
              X4=C54*X6-C52*X8
              A(M1,I+NN)=X1+X3
              A(M1,I+2*NN)=X2+X4
              A(M1,I+3*NN)=X2-X4
              A(M1,I+4*NN)=X1-X3
            ELSE
              W0=W
              DO 200 J=0,K1
 200          WORK(J)=A(M1,I+J*NN)
              DO 240 L=0,K1
                T=WORK(K1)
                DO 220 J=K1-1,0,-1
 220              T=T*W0+WORK(J)
                A(M1,I+L*NN)=T
c                W0=W0*WP0+W0
                W0=W0*WP0
 240          CONTINUE
            ENDIF
 260        CONTINUE
 280      CONTINUE
c          W=W*WP+W
          W=W*WP
 300    CONTINUE
        NN=IS
      ENDDO
      RETURN
      END
C--------------- ZFFTTR -----------------
      SUBROUTINE ZFFTTR(A,L,M,N)
      IMPLICIT NONE
      INTEGER L,M,N,I,J,K
      COMPLEX*16 A(L,0:M*N-1)
      INTEGER MW
      PARAMETER (MW=100000)
      COMPLEX*16 W(0:MW-1)
	INCLUDE 'include/ctrlcm.h'
      IF(M*N.GT.MW)  THEN
        WRITE(MSGFL,100)
 100    FORMAT(' Insufficient storage in ZFFTTR.')
        CALL STOPCAIN(100)
      ENDIF
      DO 300 I=1,L
        DO 200 J=0,M-1
        DO 200 K=0,N-1
 200    W(J+M*K)=A(I,J+M*K)
        DO 240 J=0,M-1
        DO 240 K=0,N-1
 240    A(I,K+N*J)=W(J+M*K)
 300  CONTINUE
      RETURN
      END
