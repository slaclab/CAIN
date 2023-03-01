      SUBROUTINE LUMCAL0(NMDIM,Q1,Q2,V1,V2,LUMFAC,
     %   LHEL,LUM,VLUM,DLUMW0,DLUMW1,DLUMW2,
     %   LBINW,NBNW,WMIN,WBIN,WBNEDG,
     %   DLUME0,DLUME1,DLUME2,
     %   LBINE,NBNE1,NBNE2,EMIN,EBIN,EBNEDG1,EBNEDG2,IPPFLG,
     %   MM,IPDD,NDD,NDD2,NDIPB,IPB1,IPB2,IADRS,NP,KIND,EP,WGT,SPIN,
     %   IRTN)
      IMPLICIT NONE
      INTEGER NMDIM,LHEL(0:2),NBNW,NBNE1,NBNE2,MM,
     %   IPPFLG,IPDD(0:MM),LBINW,LBINE(2),
     %   NDD(0:MM),NDD2(0:MM),NDIPB,IPB1(0:NDIPB-1),IPB2(0:NDIPB-1),
     %   NP,IADRS(2,NP),KIND(NP),IRTN
      REAL*8 Q1(0:NMDIM-1),Q2(0:NMDIM-1),V1(0:NMDIM-1),V2(0:NMDIM-1),
     %  LUMFAC,LUM(0:20),VLUM,
     %  DLUMW0(NBNW),DLUMW1(NBNW,4),DLUMW2(NBNW,16),
     %  WMIN,WBIN,WBNEDG(0:NBNW),
     %  DLUME0(NBNE1,NBNE2),DLUME1(NBNE1,NBNE2,4),
     %  DLUME2(NBNE1,NBNE2,16),EMIN(2),EBIN(2),
     %  EBNEDG1(0:NBNE1),EBNEDG2(0:NBNE2),
     %  EP(0:3,NP),WGT(NP),SPIN(3,NP)
      INTEGER M,K,K0,LL,N,KA,KB
      REAL*8 LUMF1
      REAL*8 VMIN/0.2D0/
      LOGICAL LDLUM
C
      LDLUM=LHEL(1).NE.0.OR.LHEL(2).NE.0.OR.NBNW.NE.0.OR.NBNE1.NE.0
     %      .OR.IPPFLG.NE.0
      M=0
      K=0
 320  K0=IPDD(M)+K
      IF(Q1(K0).EQ.0.OR.Q2(K0).EQ.0) THEN
        LL=0
      ELSEIF(V1(K0)/Q1(K0)**2.GE.VMIN
     %   .OR.V2(K0)/Q2(K0)**2.GE.VMIN.OR.M.EQ.MM) THEN
C          If all the particles have the same weight,
C          Q**2/V is the number of particles in the box.
C          Thus, above statement says
C          if the number of particles is less than 1/VMIN
C          calculate the luminosity of that box,
C          and, if not, further divide the box.
        LL=1
      ELSE
        M=M+1
        K=4*K
        GOTO 320
      ENDIF
      IF(LL.EQ.1) THEN
        LUMF1=NDD2(M)*LUMFAC
        LUM(0)=LUM(0)+Q1(K0)*Q2(K0)*LUMF1
        VLUM=VLUM+(Q1(K0)**2*V2(K0)+V1(K0)*Q2(K0)**2
     %            +V1(K0)*V2(K0))*LUMF1**2
        IF(LDLUM) THEN
          N=4**(MM-M)
          KA=N*K
          KB=KA+N-1
          CALL DLUMCAL(KA,KB,NDIPB,IPB1,IPB2,IADRS,NP,KIND,EP,WGT,
     %        SPIN,LBINW,NBNW,WMIN,WBIN,WBNEDG,
     %        LBINE,NBNE1,NBNE2,EMIN,EBIN,EBNEDG1,EBNEDG2,
     %        LHEL,LUM,DLUMW0,DLUMW1,DLUMW2,DLUME0,DLUME1,DLUME2,
     %        LUMF1,IPPFLG,IRTN)
          IF(IRTN.NE.0) RETURN
        ENDIF
      ENDIF
      K=K+1
 340  IF(M.NE.0) THEN
        IF(MOD(K,4).NE.0) GOTO 320
        K=K/4
        M=M-1
        GOTO 340
      ENDIF
      IRTN=0
      RETURN
      END
