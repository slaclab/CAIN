      SUBROUTINE ENDPSH(T0,T1,IFIN,IRTN)
	USE BEAMCM
	USE BBCOM
	USE LUMCOM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER IFIN,IRTN
      REAL*8 T0,T1
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/extfcm.h'
      INCLUDE 'include/tstpcm.h'
      INCLUDE 'include/lasrcm.h'
      INCLUDE 'include/lumcom2.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/cfqedcm.h'
      include 'include/pushcm.h'
      INTEGER NS,IS,N,I,L
      REAL*8 THALF,SS(2),SMM(2)
      REAL*8 CPRFAC/0.7D0/
C
      THALF=(T0+T1)/2
C----- Store test particle data
      IF(NTSTP.NE.0.AND.IWRNTS.EQ.0) CALL TSTPST(MSGFL)
 190  IF(IFIN.NE.0) GOTO 600
C-----
 200  IF(NP.GT.MP-MPNEW) THEN
        CALL CPRSBM(CPRFAC)
        GOTO 200
      ENDIF
      call chk0wgt(1.)
      DO 240 N=1,NP
        LBBFIN(N)=0
        ISBIN(N)=0
        DO 220 I=1,3
          FLD(I,1,N)=0
          FLD(I,2,N)=0
 220   CONTINUE
 240  CONTINUE
C----- External field
      IF(LEXTF.GE.1) CALL EXTFLD(T1)
C----- Laser Interaction -----
      IF(NLSR.GE.1) THEN
c	  CALL LSRQED(T1,IRTN)
         call chk0wgt(1.1)
         CALL LSRQEDBH(T1,IRTN)
         call chk0wgt(1.2)
         IF(IRTN.NE.0) GOTO 990
         CALL LSRQEDCP(T1,IRTN)
         call chk0wgt(1.3)
         IF(IRTN.NE.0) GOTO 990
         CALL LSRQEDBW(T1,IRTN)
         call chk0wgt(1.4)
         IF(IRTN.NE.0) GOTO 990
      ENDIF
      call chk0wgt(2.)
C----- S-slice -----
C     Luminosity, beam-beam, and incoherent interaction must be
C   in S-slice loop. The only constraint on the order of
C   these three is that incoherent interaction must come
C   after luminosity because the former uses the luminosity
C   bin contents. Note that beam-beam uses different mesh
C   system.  Note also that changing the order of beam-beam
C   causes a slight change of particle coordinates.
C     A new constraint: in order to take into account the
C   field suppression of virtual photons, the incoherent
C   interaction must come after beam-beam.
      IF(NLUM.LE.0.AND.BBON.LE.0) GOTO 440
      IF(DSLUM.LE.0) GOTO 920
      CALL SMESH1(THALF,DSLUM,SMM,NS,EMAX,IRTN)
      call chk0wgt(3.)
      print *, " endpsh dslum,ns=", dslum,ns
      IF(IRTN.NE.0) GOTO 440
      DO 400 IS=1,NS
        SS(1)=SMM(1)+(IS-1)*(SMM(2)-SMM(1))/NS
        SS(2)=SS(1)+(SMM(2)-SMM(1))/NS
C----- Beam-Beam Interaction -----
        IF(BBON.EQ.1) THEN
c          print *, " endpsh about to call bbfld is= ", is
          CALL BBFLD(THALF,IS,SS,IRTN)
          IF(IRTN.GE.10) GOTO 990
C            IRTN=1: no bb-field
C          IF(IRTN.EQ.0) CALL BBKICK(T1,IS,SS)
          IF(IRTN.EQ.0) CALL SLVEQM(T1,IS,SS)
        ENDIF
C----- Luminosity integration & incoherent pair -----
        IF(NLUM.GE.1.OR.LPPINT0.GE.1) THEN
          CALL LUMBFL(THALF,IS)
          IF(NLUM.GE.1) THEN
            CALL LUMCAL(IADRS,NP,KIND,EP,WGT,SPIN,IRTN)
            IF(IRTN.NE.0) GOTO 990
          ENDIF
          IF(LPPINT0.GE.1) THEN
            CALL PPINT(IS,IRTN)
            IF(IRTN.NE.0) GOTO 990
          ENDIF
        ENDIF
 400  CONTINUE
       call chk0wgt(4.)
C----- Beamstrahlung and Coherent Pair -----
C        These must come after beam-beam because the beam field
C        is needed. But they can be outside S-slice loop because
C        the field strength at each particle has been stored.
 440  IF(LBMST.GE.1) THEN
c        print *, " endpsh before bmst np= ",np
        CALL BMST(T1,IRTN)
        call chk0wgt(5.)
c        print *, " endpsh after  bmst np= ",np
        IF(IRTN.NE.0) GOTO 990
      ENDIF
      IF(LCOHP.GE.1) THEN
c        print *, " endpsh before cohpar np= ",np
        CALL COHPAR(T1,IRTN)
        call chk0wgt(6.)
c        print *, " endpsh after  cohpar np= ",np
        IF(IRTN.NE.0) GOTO 990
      ENDIF
C-----  -----
 500  CALL DRIFT1(T1,1,0,ISPIN)
      call chk0wgt(7.)
      NBBPL=0
      GOTO 800
C------ Final step of push loop
 600  IF(BBON.GE.1) THEN
        IF(WGTOUT(1).GE.0.05D0.OR.WGTOUT(2).GE.0.05D0) THEN
          WRITE(MSGFL,620) (100*WGTOUT(L),L=1,2)
 620      FORMAT(' *** Warning on beam-beam force calculation.',/,
     %     '  Significant charged particle fraction as field source ',
     %     'is outside mesh',/,
     %      5X,'Right-going beam',T25,0PF6.2,' % (max)',/,
     %      5X,'Left -going beam',T25,0PF6.2,' % (max)')
        ENDIF
      ENDIF
      IF(LBMST.GE.1.OR.LCOHP.GE.1.OR.NPHCP.GE.0.OR.NPHBW.GE.1) THEN
        WRITE(MSGFL,640)
 640    FORMAT(' --- Maximum transition probility of macro-particles',
     %   /,'      per time step in the previous PUSH ---')
        IF(LBMST.GE.1) WRITE(MSGFL,650) 'Beamstrahlung',PMMBS
        IF(LCOHP.GE.1) WRITE(MSGFL,650) 'Coherent Pair',PMMCO
        IF(NPHCP.GE.0) WRITE(MSGFL,650) 'Laser Compton',PMMCP
        IF(NPHBW.GE.1) WRITE(MSGFL,650) 'Laser Breit-Wheeler',PMMBW
 650    FORMAT(10X,A,T35,1PD11.4)
      ENDIF
      IF(LPPINT0.GE.1) CALL VPHDBG(2,0D0,0D0,0D0,0D0,0D0)
      call chk0wgt(8.)
C        temporarily inserted for debug
C--------------------------------------
 800  CALL DELLOS
      call chk0wgt(9.)
      IRTN=0
      RETURN
C
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.ENDPSH) Parameter Smesh undefined. ',
     %  'It is needed for beam-beam, luminosity etc.')
      RETURN
 990  RETURN
      END

