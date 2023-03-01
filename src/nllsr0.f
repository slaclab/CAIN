      SUBROUTINE NLLSR0(K)
      IMPLICIT NONE
      INTEGER K,K1,ISHIFT,I
      INCLUDE 'include/nllsrcm.h'
C
      K1=3-K
      IF(IREADY(K1).NE.IREADY0) THEN
        IP=1
      ELSE
        IF(IPINIT(K1).NE.1) THEN
          ISHIFT=IPINIT(K1)-1
          DO 200 I=IPINIT(K1),IPLAST(K1)
            R8(I-ISHIFT)=R8(I)
 200      CONTINUE
          IF(K1.EQ.1) THEN
            IPBES=IPBES-ISHIFT
            IPFF=IPFF-ISHIFT
            IPFINT=IPFINT-ISHIFT
            IPFALL=IPFALL-ISHIFT
          ELSE
            IPGG=IPGG-ISHIFT
            IPGINT=IPGINT-ISHIFT
            IPGALL=IPGALL-ISHIFT
          ENDIF
          IPINIT(K1)=IPINIT(K1)-ISHIFT
          IPLAST(K1)=IPLAST(K1)-ISHIFT
        ENDIF
        IP=IPLAST(K1)+1
      ENDIF
      IREADY(K)=IREADY0
      RETURN
      END

