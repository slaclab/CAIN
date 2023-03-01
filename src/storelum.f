      SUBROUTINE STORELUM(UNIT)
	USE LUMCOM
      IMPLICIT NONE
      INTEGER UNIT
      INCLUDE 'include/headcm.h'
      INCLUDE 'include/lumcom2.h'
      REAL*8 LUMR(-MWLUM+1:MWLUMR)
      INTEGER LUMI(MWLUMI)
      EQUIVALENCE (LUMR(-MWLUM+1),DLUM(1)),(LUMI(1),NLUM)
      INTEGER IP1,N,IL,L,I
      INTEGER NCOMPL(0:2)/1,4,16/
C
      IF(NLUM.LE.0) RETURN
C Find used length of DLUM
      IP1=0
      DO 220 IL=1,NLUM
        DO 200 L=0,2
          IF(NBNWLM(IL).GE.1) THEN
            IF(IPWLUM(L,IL).GE.IP1) THEN
              IP1=IPWLUM(L,IL)
              N=IP1+NBNWLM(IL)*NCOMPL(L)-1
            ENDIF
          ENDIF
          IF(NBNELM(1,IL).GE.1) THEN
            IF(IPELUM(L,IL).GE.IP1) THEN
              IP1=IPELUM(L,IL)
              N=IP1+NBNELM(1,IL)*NBNELM(2,IL)*NCOMPL(L)-1
            ENDIF
          ENDIF
 200    CONTINUE
 220  CONTINUE
      WRITE(UNIT,'(A)') JOBTIM
      WRITE(UNIT,'(I10)') N
      IF(N.GE.1) WRITE(UNIT,'(1P5D20.13)') (DLUM(I),I=1,N)
      WRITE(UNIT,'(1P5D20.13)') (LUMR(I),I=1,MWLUMR)
      WRITE(UNIT,'(10I10)') (LUMI(I),I=1,MWLUMI)
      RETURN
      END

