      SUBROUTINE PPINT(IS,IRTN)
C           Corrected on May.17.1999
C-----  Incoherent Pair -----
C
C    IS        Longitudinal slice number to be treated here.
C    IRTN      Return code. Should be zero normally.
C              A possible error in the present subroutine may be
C              that the particle buffer saturated. IRTN must
C              be a large number in such a case.
C  Common block INCPCM  (lumcom.f)
C    KNINCP(j1,j2) (j1=1,3)(j2=1,3) is the flag to compute incoherent
C              pair from right-going j1-th kind (1:photon, 2:electron,
C              3:positron) and left-going j2-th kind.
C    ENHINCP(j1,j2) (j1=1,3)(j2=1,3)   Event rate enhancement factor.
C              If ENHINCP=1, generate
C              macro-particle pairs as many as the real pairs.
C              If >1, generate more macro-particles (with smaller
C              weight).
C  Common block LUMCOM  (lumcom.f)
C    DXY(i)    Transverse bin size in meter (i=1:x, 2:y)
C    XYMLUM(i1,i)  Lower edge of the lowest bin (i1=1) and upper edge
C              of the highest bin (i1=2) in meter. (i=1:x, 2:y)
C    MM        Number of bins per one dimension is given by 2**MM.
C                (common to x and y)
C    NDD(m)    = 2**m.
C              Thus, DXY(i)=(XYMLUN(2,i)-XYMLUM(1,i))/NDD(MM).
C    NDD2(m)   = NDD(m)**2 = 2**(2*m).
C      The numbering of MM*MM bins is a little complicated.
C      They are numbered, if MM=3 (NDD(MM)=8) for example, as 
C               42 43 46 47 58 59 62 63
C               40 41 44 45 56 57 60 61
C           y   34 35 38 39 50 51 54 55
C           ^   32 33 36 37 48 49 52 53 
C           |   10 11 14 15 26 27 30 31
C           |    8  9 12 13 24 25 28 29
C           |    2  3  6  7 18 19 22 23
C                0  1  4  5 16 17 20 21   --->x
C      This is designed such that, if you compute from an address
C      k (0<=k<=NDD(MM)**2-1) a new address k'=k/4 (in FORTRAN sense),
C      then k' is the address of the coarser mesh with the number
C      of bins NDD2(MM-1)=NDD(MM-1)**2=[NDD(MM)/2]**2.
C      The address of a given point (x,y) is computed by the
C      following way if it is in the range defined by XYMLUM.
C          ix=INT((x-XYMLUM(1,1))/DXY(1))   (0<=ix<=NDD(MM)-1)
C          iy=INT((x-XYMLUM(1,2))/DXY(2))   (0<=iy<=NDD(MM)-1)
C        Then,  k=KIJDD(ix)+2*KIJDD(iy)   (0<=k<=NDD(MM)**2-1)
C        where KIJDD is in the common block LUMCOM.
C
C    DSLUM     Width of longitudinal slice in meter.
C    DTLUM     Time interval times velocity of light (meter).
C    DIST(IPDD(MM)+k,j,l)  Number of real particles (i.e., weighted
C              sum of macro-particles) in the k-th bin of j-th kind 
C              in the l-th (1:right-going, 2:left-going) beam.
C              0<=K<=NDD(MM)**2-1.
C              In fact, DISP(IPDD(M)+k,j,l) is the similar quantity
C              in the coarse mesh system with number of bins
C              2**M times 2**M.  (M<=MM)
C    IPBN(k,j,l)  (0<=k<=NDD2(MM)-1, j=1,2,3, l=1,2)
C              If =0, k-th bin of j-th kind of l-th beam is empty.
C              If >0, it is the address in the array IADRS (see below).
C              This applies to the finest bin system (M=MM) only.
C    
C  Module BEAMCM  (beamcm.f)
C    MP      Maximum number of macro-particles that can be stored
C            in the memory.
C    MPNEW   Maximum number of new particles created in one time mesh.
C    NP      Number of macro-particles stored.
C    KIND(n) (1<=n<=NP)  1:photon, 2:electron, 3:positron.
C    LOST(n) 0: alive, 1: already lost.
C    PNAME(n)  4 byte character string. Normally '    '.
C              Test particles has 'T***' (*: some character).
C              Incoherent pair particles should have 'I    '.
C              Descendants of incoherent pairs will have 'I***'.
C    WGT(n)  Weight of n-th macro-particle. (It corresponds to WGT(n) real
C            particles.)
C    TXYS(i,n) (i=0,1,2,3)  Time and space coordinate of n-th particle.
C    EP(i,n) (i=0,1,2,3)  Energy-momentum (eV/c**2, eV/c).
C            Right(left)-going particles have EP(3,n) >0 (<0).
C    SPIN(i,n) (i=1,2,3)  For electron/positron, the polarization
C             component along (x,y,s) axis. For photons, the Stokes
C             parameter.
C    ISBIN(n)  Longitudinal bin of n-th particle. If=0, outside the
C            longitudinal mesh region. This subroutine should care
C            about only those with ISBIN(n)=IS, because otherwise
C            the particle has not yet bben transversely binned.  
C    LBBFIN(n)  Flag whether the kick by the beam-beam force has been
C            computed or not.
C    GEN(n)  Generation of n-th particle. 1 if defined by the BEAM
C            command. Secondaries have GEN>=2.
C            An incoherent pair produced from n1 and n2 should have
C            the generation GEN(n1)+GEN(n2).
C    WORK    Work area of length 2*MP. Can be used here.
C    IADRS(2,MP)  Shows the address of particles in the transverse bins.
C            If n1=IPBN(k,j,l)>=1, then IADRS(2,n1) is the particle
C            identity of one particle in the k-th bin.
C            If n2=IADRS(1,n1)=0, there is no other particle
C            in the bin. If n2>=1, IADRS(2,n2) is the second
C            particle in the bin, and so forth.
C             Thus, for example, the energy sum of the particles in the 
C            k-th bin may be written in the following way.
C             SUM=0
C             I=IPBN(K,J,L)
C             DO WHILE (I.GE.1)
C               SUM=SUM+EP(0,IADRS(1,I))
C               I=IADRS(2,I)
C             ENDDO
C
C  Virtual photons
C   Virtual photons from electrons and positrons are generated by
C   calling the subroutine VPHBFL. The following variables in the
C   common block LUMCOM and BEAMCM are defined.
C    NVPH     Number of virtual photons generated, including both
C             right- and left-going beam.
C    EPVPH(i,n) (i=0,1,2,3, n=1,NVPH) Energy-momentum of n-th virtual
C             photon.
C    WGTVPH(n) (n=1,NVPH)  Weight factor.
C    IDVPH(n) (n=1,NVPH)  Parent particle ID of the virtual photon.
C    IPBNV(k,l)  (0<=k<=NDD2(MM)-1, l=1,2)
C              If =0, k-th bin of l-th beam is empty.
C              If >0, it is the ID of a virtual photon in the bin.
C              Other pvirtual photons in the same bin are found
C              in the array IADRSV (see below).
C              This applies to the finest bin system (M=MM) only.  
C    IADRSV(*)  If IADRSV(n)>0, it is the virtual photon ID in the
C              same bin. Thus, for example, a bin-wise loop over
C              the virtual photons is
C                    EMAX=0
C                    DO *** L=1,2
C                      DO ** K=0,NDD2(MM)-1
C                         N=IPBNV(K,L)
C                         DO WHILE (N.GT.0)
C                           .... action for N-th virtual photon ...
C                           N=IADRSV(N)
C                         ENDDO
C               **     CONTINUE
C               ***  CONTINUE
C
C  To add one particle to the particle list.
C   CALL ADDONE(0,KIN,GN,PNAM,IS,WT,TXY,EP1,SP,LBBF,BFL,IRTN)
C    0      Indicates to append a particle.
C           (Replace if it is 1 to NP, but this is not needed here
C           because the incoherent pair process should not
C           affect the parent particles.)
C    KIN    Kind. 1 or 2 or 3.
C    GN     Generation.
C    PNAM   4-byte character. Should be 'I***' for incoherent pairs.
C    IS     Longitudinal bin. Should be identical to the argument
C           of the present subroutine PPINT.
C    WT     Weight. How many real particles represented by this
C           macro-particle. Should be equal to 1/ENHINCP.
C    TXY(i) (i=0,3), EP1(i) (i=0,3)  Coordinate and energy-momentum.
C    SP(i) (i=1,3)  Spin.
C    LBBF   Should be zero.
C    BFL(i) (i=1,6)  Should be all zero.
C    IRTN   Return code from ADDONE. If non-zero, should immediately
C           return from PPINT with non-zero return code.
C           This happens only when the particle buffer is saturated.
C           (Too many particles created in one time step)
C    Caution:  Calling ADDONE with zero first argument increases
C              the common block variable NP by one. Therefore,
C              you must avoid a do loop like
C                    DO   N=1,NP
C                       .......
C                       CALL ADDONE(0,....)
C                    DOEND
C              Instead, you should write
C                    NP0=NP
C                    DO   N=1,NP0
C
C  Other facts
C   *    Error messages should be written on MSGFL which is in common
C        block CTRLCM in 'ctrlcm.h'
C   *    A variable IDEBUG in 'ctrlcm.h' is available for debugging.
C        Its value can be set by 
C                SET Debug= n ;
C        in the input data. (If n>=2, massages will be printed
C        whenever a major subroutine is called.)
C   *    Output with bigger volume should be written on OUTFL and
C        output for TopDrawer on TDFL.
C   *    Uniform random number in (0,1) is created by
C              REAL*8 X,RANDCAIN
C              EXTERNAL RANDCAIN
C              X=RANDCAIN()
C
	USE BEAMCM
	USE LUMCOM
      IMPLICIT NONE
      INTEGER IS,IRTN
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/lumcom.h'
      INTEGER J1,J2,K,I1,I2,N1,N2,L,GN,IRTN1,IV,LV,KN
      REAL*8 VOL,ENH1,WGT1
      REAL*8 HELV/0D0/
C           ignore virtual photon polarization
      CHARACTER*4 NAM
C
      CALL CPUTIM('PPINT',1)
      IRTN1=0
C       (Above 1 line added Jan.27.2000 according to Ohgaki)
      IF(MM.EQ.0) GOTO 800
      VOL=DXY(1)*DXY(2)*DSLUM
C  Breit-Wheeler
      IF(LPPINT(1).LE.0) GOTO 300
      J1=1
      J2=1
      IF(NPKINL(J1,1).EQ.0.OR.NPKINL(J2,2).EQ.0) GOTO 300
      NAM='IBW '
      WGT1=1/ENHPPI(1)
      DO 280 K=0,NDD2(MM)-1
        IF(IPBN(K,J1,1).LE.0.OR.IPBN(K,J2,2).LE.0) GOTO 280
        I1=IPBN(K,J1,1)
        DO WHILE (I1.GE.1)
          N1=IADRS(1,I1)
          I1=IADRS(2,I1)
          I2=IPBN(K,J2,2)
          DO WHILE (I2.GE.1)
            N2=IADRS(1,I2)
            I2=IADRS(2,I2)
            ENH1=WGT(N1)*WGT(N2)/WGT1
            GN=GEN(N1)+GEN(N2)
            CALL ICPAIR(EP(0,N1),EP(0,N2),SPIN(2,N1),SPIN(2,N2),
     %        TXYS(0,N1),TXYS(0,N2),IS,NAM,GN,WGT1,ENH1,EMNPPI,
     %        VOL,DTLUM,MSGFL,IRTN1)
            IF(IRTN1.NE.0) GOTO 910
          ENDDO
        ENDDO
 280  CONTINUE
C  Virtual photon generation
 300  IF(KVPH0.EQ.0) GOTO 800
      CALL VPHBFL(IS,IRTN1,MSGFL)
      IF(IRTN1.NE.0) GOTO 900
      IF(NVPH.EQ.0) GOTO 800
C  Bethe-Heitler
      IF(LPPINT(2).LE.0) GOTO 400
      NAM='IBH '
      WGT1=1/ENHPPI(2)
      DO 380 L=1,2
C       L=1: right-going real photon and left-going virtual photon
C       L=2: left-going real photon and right-going virtual photon
        LV=3-L
        IF(LREPPI(2).NE.0.AND.LREPPI(2).NE.L) GOTO 380
        IF(NPKINL(1,L).EQ.0.OR.NPKINL(2,LV)+NPKINL(3,LV).EQ.0) 
     %          GOTO 380
        DO 340 K=0,NDD2(MM)-1
          I1=IPBN(K,1,L)
          DO WHILE (I1.GE.1)
            N1=IADRS(1,I1)
            I1=IADRS(2,I1)
            IV=IPBNV(K,LV)
            DO WHILE (IV.GE.1)
              N2=IDVPH(IV)
              ENH1=WGT(N1)*WGTVPH(IV)/WGT1
              GN=GEN(N1)+GEN(N2)
              CALL ICPAIR(EP(0,N1),EPVPH(0,IV),SPIN(2,N1),HELV,
     %          TXYS(0,N1),TXYVPH(0,IV),IS,NAM,GN,WGT1,ENH1,EMNPPI,
     %          VOL,DTLUM,MSGFL,IRTN1)
              IF(IRTN1.NE.0) GOTO 920 
              IV=IADRSV(IV)
            ENDDO
          ENDDO
 340    CONTINUE
 380  CONTINUE
C Landau-Lifshitz
 400  IF(LPPINT(3).LE.0) GOTO 500
      IF(NPKINL(2,1)+NPKINL(3,1).EQ.0
     %    .OR.NPKINL(2,2)+NPKINL(3,2).EQ.0) GOTO 500
      NAM='ILL '
      WGT1=1/ENHPPI(3)
      DO 480 K=0,NDD2(MM)-1
        I1=IPBNV(K,1)
        DO WHILE (I1.GE.1)
          I2=IPBNV(K,2)
          N1=IDVPH(I1)
          DO WHILE (I2.GE.1)
            N2=IDVPH(I2)
            ENH1=WGTVPH(I1)*WGTVPH(I2)/WGT1
            GN=GEN(N1)+GEN(N2)
            CALL ICPAIR(EPVPH(0,I1),EPVPH(0,I2),HELV,HELV,
     %        TXYVPH(0,I1),TXYVPH(0,I2),IS,NAM,GN,WGT1,ENH1,EMNPPI,
     %        VOL,DTLUM,MSGFL,IRTN1)
            IF(IRTN1.NE.0) GOTO 930 
            I2=IADRSV(I2)
          ENDDO
          I1=IADRSV(I1)
        ENDDO
 480  CONTINUE
C Bremsstrahlung
 500  IF(LPPINT(4).LE.0) GOTO 800
      IF(NPKINL(2,1)+NPKINL(3,1).EQ.0
     %    .OR.NPKINL(2,2)+NPKINL(3,2).EQ.0) GOTO 800
      NAM='IBR '
      WGT1=1/ENHPPI(4)
      DO 580 L=1,2
C       L=1: right-going real e+e- and left-going virtual photon
C       L=2: left-going real e+e- and right-going virtual photon
        LV=3-L
        IF(LREPPI(2).NE.0.AND.LREPPI(2).NE.L) GOTO 580
        DO 560 K=0,NDD2(MM)-1
          DO 540 KN=2,3
            I1=IPBN(K,KN,L)
            DO WHILE (I1.GE.1)
              N1=IADRS(1,I1)
              I1=IADRS(2,I1)
              IV=IPBNV(K,LV)
              DO WHILE (IV.GE.1)
                N2=IDVPH(IV)
                ENH1=WGT(N1)*WGTVPH(IV)/WGT1
                GN=GEN(N1)+GEN(N2)
                CALL ICCMPT(EP(0,N1),EPVPH(0,IV),KN,
     %            TXYS(0,N1),TXYVPH(0,IV),IS,NAM,GN,WGT1,ENH1,EMNPPI,
     %            VOL,DTLUM,MSGFL,IRTN1)
                IF(IRTN1.NE.0) GOTO 940 
                IV=IADRSV(IV)
              ENDDO
            ENDDO
 540      CONTINUE
 560    CONTINUE
 580  CONTINUE
C
 800  IRTN=0
      GOTO 990
 900  IRTN=1000
      GOTO 990
 910  IRTN=1001
      GOTO 990
 920  IRTN=1002
      GOTO 990
 930  IRTN=1003
      GOTO 990
 940  IRTN=1004
      GOTO 990
 990  CALL CPUTIM('PPINT',2)
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE ICPAIR(EG1,EG2,HEL1,HEL2,TXYS1,TXYS2,IS,NAM,GN,
     %    WGT,ENH,EMIN,VOL,DT,MSGFL,IRTN)
      IMPLICIT NONE
      INTEGER IS,GN,MSGFL,IRTN
      CHARACTER*(*) NAM
      REAL*8 EG1(0:3),EG2(0:3),HEL1,HEL2,TXYS1(0:3),TXYS2(0:3),
     %     WGT,ENH,EMIN,VOL,DT
      INTEGER NPRMAX
      PARAMETER (NPRMAX=640000)
      INTEGER I,N,NPR,KN,KIN
      REAL*8 EP1(0:3,NPRMAX,2),TXY(0:3)
      INTEGER LBBF1/0/
      REAL*8 SPIN1(3)/0,0,0/,BFL1(6)/0,0,0,0,0,0/
C
      CALL ICBWGN(EG1,EG2,HEL1,HEL2,ENH,VOL,DT,
     %           NPRMAX,NPR,EP1(0,1,1),EP1(0,1,2),MSGFL,IRTN)
      IF(IRTN.NE.0) RETURN
      IF(NPR.GE.1) THEN
        DO 200 I=0,3
          TXY(I)=0.5D0*(TXYS1(I)+TXYS2(I))
 200    CONTINUE
        DO 260 N=1,NPR
          DO 250 KN=1,2
            IF(EP1(0,N,KN).LE.EMIN) GOTO 250
            KIN=KN+1
            CALL ADDONE(0,KIN,GN,NAM,IS,WGT,TXY,
     %           EP1(0,N,KN),SPIN1,LBBF1,BFL1,IRTN)
            IF(IRTN.NE.0) THEN
              WRITE(MSGFL,220)
 220          FORMAT(' (SUBR.ICPAIR) Too many pairs in a step.')
              RETURN
            ENDIF
 250      CONTINUE
 260    CONTINUE
      ENDIF
      IRTN=0
      RETURN
      END
