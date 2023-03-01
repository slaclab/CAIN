        SUBROUTINE ZFFT2(CA,MA,M,N,ISGN1,ISGN2)
C  2-dimensional FFT based on FFT routine
C       CALL ZFFT(CA,N,MDIM,ISGN)
C
C       CA      INPUT/OUTPUT MATRIX
C       N       LENGTH OF CA,CB
C       M       LENGTH OF THE FIRST INDEX OF CA AND CB
C
C  CB(I,J) = SUM CA(K,L)*EXP(sgn1*i*2*pi*(I)*(K)/M)
C                       *EXP(sgn2*i*2*pi*(J)*(L)/N)
C           over 0<=K<=M-1, 0<=L<=N-1, for 0<=I<=M-1, 0<=J<=N-1.
C      sign1(2)=1 if ISGN1>=0,  else -1.
C
        IMPLICIT NONE
        INTEGER MA,M,N,ISGN1,ISGN2
        COMPLEX*16 CA(0:MA-1,0:N-1)

C 
        CALL ZFFTA(CA,MA,M,N,ISGN1)
        CALL ZFFTB(CA,MA,M,N,ISGN2)
        RETURN
        END

        SUBROUTINE ZFFT2S(CA,MA,M,N,ISGN1,ISGN2)
C  Same as ZFFT2 except that the Fourier transform is defined by
C  CB(I,J) = SUM CA(K,L)*EXP(sgn1*i*2*pi*(I-i0)*(K-i0)/M)
C                       *EXP(sgn2*i*2*pi*(J-j0)*(L-j0)/N)
C      where  i0=(M-1)/2, j0=(N-1)/2
C
        IMPLICIT NONE
        INTEGER MA,M,N,ISGN1,ISGN2
        COMPLEX*16 CA(0:MA-1,0:N-1)

C 
        CALL ZFFTAS(CA,MA,M,N,ISGN1)
        CALL ZFFTBS(CA,MA,M,N,ISGN2)
        RETURN
        END
