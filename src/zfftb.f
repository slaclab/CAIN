C**************************ZFFTB ***************************************
      SUBROUTINE ZFFTB(A,MA,MM,NN,ISGN)
C  FFT routine for Complex*16 array with arbitrary number of elements.
C  Simultaneous FFT for many arrays. (FFT on the right index)
C      B(k)= Sum A(j)* exp[+-2*pi*i*k*j/NN]   over j=0 to NN-1
C                     for k=0 to NN-1
C Input:
C  A     Complex*16 array of dimension A(MA,0:NN-1)
C  MA    The first dimension of A.
C  MM    Number of arrays to be FFTed. (<=MA)
C  NN    Length of A. (>=1)
C  ISGN  Sign of the phase, +1 or -1.
C Output:
C  A     B is stored in the same area as A.
C Restriction:
C  * NN must not contain a prime factor larger than the MPRIME-th
C    prime number. This is only for the work area WORK(MPRIME).
C  * When NN has two (or more) different prime factors, NN must be
C    less than MW defined in subroutine ZFFTBTR.  This is the work
C    area to transpose a rectangular matrix. This problem can be
C    eliminated, if an efficient algorithm of transposing "in place",
C    or if dynamic memory allocation is allowed.
C
      IMPLICIT NONE
      INTEGER MPRIME
      PARAMETER (MPRIME=26)
      INTEGER MA,MM,NN,ISGN
      INTEGER I,J,KK,L,M,N,N1,K,IM,KPRIME
      COMPLEX*16 A(MA,0:NN-1)
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
          CALL ZFFTB0(A,MA,MM,L*M,N,KK,ISGN,WORK)
          IF(M.NE.1) THEN
            W0=1
            THETA=PI2/(ISGN*M*N)
            WP=DCMPLX(COS(THETA),SIN(THETA))
            DO 260 J=0,M-1
              W=1
              DO 250 K=0,N-1
                DO 240 I=0,L-1
                  DO 230 IM=1,MM
                    A(IM,I+L*(J+M*K))=W*A(IM,I+L*(J+M*K))
 230              CONTINUE
 240            CONTINUE
                W=W0*W
 250          CONTINUE
              W0=W0*WP
 260        CONTINUE
            CALL ZFFTBTR(A,MA,MM,L,M,N)
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
 910  FORMAT(' (SUBR.ZFFTB) Invalid 2nd argument n=',I4,
     %  '. Must be >=1.')
      RETURN
 920  WRITE(MSGFL,930) ISGN
 930  FORMAT(' (SUBR.ZFFTB) Invalid 3rd argument ISGN=',I4,
     %  '. Must be 1 or -1.')
      RETURN
 940  WRITE(MSGFL,950) NN
 950  FORMAT(' (SUBR.ZFFTB) 2nd argument',I8,' contains too large ',
     %  'a prime factor.')
      RETURN
      END
C-------------------------- ZFFTB0 --------------------------------------
      SUBROUTINE ZFFTB0(A,MA,MM,M,N,K,ISGN,WORK)
C Fast Fourier Transformation of arbitrary base to be called by ZFFTB.
C    B(l,k)= Sum A(l,j)*exp[+-2*pi*i*k*j/N]   over j=0 to N-1
C             for k=0 to N-1, l=1 to M.
C Input:
C   A    Complex*16 array of dimension A(MA,M,0:N-1)
C   MA   First dimension of A.
C   MM   Actual length of A (<=MA)
C   M    Second dimension of A.
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
      INTEGER MA,MM,M,N,K,ISGN
      COMPLEX*16 A(MA,M,0:N-1)
      INTEGER M1,N1,I,J,NN,IS,K1,L,IM
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
      DO 130 I=0,N-1
        IF(J.GT.I) THEN
          DO 120 M1=1,M
            DO 110 IM=1,MM
              T=A(IM,M1,J)
              A(IM,M1,J)=A(IM,M1,I)
              A(IM,M1,I)=T
 110        CONTINUE
 120      CONTINUE
        ENDIF
C  Bit inversion in K-scale
        N1=NN
        DO WHILE (N1.GE.K1.AND.J.GE.N1)
          J=J-N1
          N1=N1/K
        ENDDO
        J=J+N1/K1
 130  CONTINUE
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
              DO 150 IM=1,MM
                T=W*A(IM,M1,I+NN)
                A(IM,M1,I+NN)=A(IM,M1,I)-T
                A(IM,M1,I)=A(IM,M1,I)+T
 150          CONTINUE
            ELSEIF(K.EQ.3) THEN
              DO 160 IM=1,MM
                X0=A(IM,M1,I)
                X1=W*A(IM,M1,I+NN)
                X2=W**2*A(IM,M1,I+2*NN)
                X3=X1+X2
                X4=DCMPLX(0D0,SGN*C31)*(X1-X2)
                X5=X0-0.5D0*X3
                A(IM,M1,I)=X0+X3
                A(IM,M1,I+NN)=X5+X4
                A(IM,M1,I+2*NN)=X5-X4
 160          CONTINUE
            ELSEIF(K.EQ.4) THEN
              W2=W**2
              DO 170 IM=1,MM
                X1=W*A(IM,M1,I+NN)
                X2=W2*A(IM,M1,I+2*NN)
                X3=W2*W*A(IM,M1,I+3*NN)
                X4=A(IM,M1,I)+X2
                X5=A(IM,M1,I)-X2
                X6=X1+X3
                X7=DCMPLX(0D0,SGN)*(X1-X3)
                A(IM,M1,I)=X4+X6
                A(IM,M1,I+NN)=X5+X7
                A(IM,M1,I+2*NN)=X4-X6
                A(IM,M1,I+3*NN)=X5-X7
 170          CONTINUE
            ELSEIF(K.EQ.5) THEN
              W2=W**2
              DO 180 IM=1,MM
                X1=W*A(IM,M1,I+NN)
                X2=W2*A(IM,M1,I+2*NN)
                X3=W2*W*A(IM,M1,I+3*NN)
                X4=W2*W2*A(IM,M1,I+4*NN)
                X0=A(IM,M1,I)
                X5=X1+X4
                X6=DCMPLX(0D0,SGN)*(X1-X4)
                X7=X2+X3
                X8=DCMPLX(0D0,SGN)*(X2-X3)
                A(IM,M1,I)=X0+X5+X7
                X1=X0+C51*X5+C53*X7
                X2=X0+C53*X5+C51*X7
                X3=C52*X6+C54*X8
                X4=C54*X6-C52*X8
                A(IM,M1,I+NN)=X1+X3
                A(IM,M1,I+2*NN)=X2+X4
                A(IM,M1,I+3*NN)=X2-X4
                A(IM,M1,I+4*NN)=X1-X3
 180          CONTINUE
            ELSE
              DO 250 IM=1,MM
                W0=W
                DO 200 J=0,K1
                  WORK(J)=A(IM,M1,I+J*NN)
 200            CONTINUE
                DO 240 L=0,K1
                  T=WORK(K1)
                  DO 220 J=K1-1,0,-1
                    T=T*W0+WORK(J)
 220              CONTINUE
                  A(IM,M1,I+L*NN)=T
                  W0=W0*WP0
 240            CONTINUE
 250          CONTINUE
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
C--------------- ZFFTBTR -----------------
      SUBROUTINE ZFFTBTR(A,MA,MM,L,M,N)
      IMPLICIT NONE
      INTEGER MA,MM,L,M,N,I,J,K,IM
      COMPLEX*16 A(MA,L,0:M*N-1)
	INCLUDE 'include/ctrlcm.h'
CC      COMPLEX*16 W(0:M*N-1)
C Since some systems do not allow dynamical memory allocation,
C the above one line has been replaced by the following 4 lines.
      INTEGER MW
      PARAMETER (MW=40000)
      COMPLEX*16 W(0:MW-1)
      IF(M*N.GT.MW) THEN
        WRITE(MSGFL,100)
 100    FORMAT(' Insufficient storage in ZFFTBTR.')
        CALL STOPCAIN(100)
      ENDIF
      DO 320 IM=1,MM
        DO 300 I=1,L
          DO 220 J=0,M-1
            DO 200 K=0,N-1
              W(J+M*K)=A(IM,I,J+M*K)
 200        CONTINUE
 220      CONTINUE
          DO 260 J=0,M-1
            DO 240 K=0,N-1
              A(IM,I,K+N*J)=W(J+M*K)
 240        CONTINUE
 260      CONTINUE
 300    CONTINUE
 320  CONTINUE
      RETURN
      END

C**************************ZFFTBS ***************************************
      SUBROUTINE ZFFTBS(A,MA,MM,NN,ISGN)
C  FFT routine ZFFTB but with symmetrized phase
C      B(k)= Sum A(j)* exp[+-2*pi*i*(k-k0)*(j-j0)/NN]   over j=0 to NN-1
C                     for k=0 to NN-1
C    where  k0=j0=(NN-1)/2
C       (Following code is written for general (k0,j0) except for their
C        definitions.)
      IMPLICIT NONE
      INTEGER MA,NN,MM,ISGN
	COMPLEX*16 A(MA,0:NN-1)
      INTEGER J,K,I
	REAL(8) K0,J0,TH
	COMPLEX*16 C,C1
	REAL(8) PI/3.141592653589793238D0/

	K0=0.5D0*(NN-1)
	J0=K0
	TH=-2*PI*ISGN*K0/NN
	C1=DCMPLX(COS(TH),SIN(TH))
	C=1
	DO J=0,NN-1
	  DO I=1,MM
	    A(I,J)=C*A(I,J)
	  ENDDO
	  C=C*C1
	ENDDO
	CALL ZFFTB(A,MA,MM,NN,ISGN)
	TH=2*PI*ISGN*K0*J0/NN
	C=DCMPLX(COS(TH),SIN(TH))
	TH=-2*PI*ISGN*J0/NN
	C1=DCMPLX(COS(TH),SIN(TH))
	DO J=0,NN-1
	  DO I=1,MM
	    A(I,J)=C*A(I,J)
	  ENDDO
	  C=C*C1
	ENDDO
	RETURN
	END
