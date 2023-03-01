      SUBROUTINE SETCNST
	USE LUMCOM
      IMPLICIT NONE
      INCLUDE 'include/cnstcm.h'
      INCLUDE 'include/ctrlcm3.h'
C      INCLUDE 'include/lumcom.h'
C  See include/ctrlcm3.h for the definitions of MRSVP1, etc.
C  MRSVP1= Number of standard math functions
C               e.g., Sin
C  MRSVP2= Number of Unchangeable predefined parameters in eval
C               e.g., Pi
C  MRSVP3= Number of Unchangeable predefined parameters in CAIN
C               e.g., Emass
C  MRSVP4= Number of Parameters that CAIN may change but users cannot 
C               e.g., T, X, Y, ...
C List of reserved function names
      CHARACTER*16 RSVPAR0(MRSVP)/
C Predefined functions (Copy of those in evinit.f)  (MRSVP1)
     %  'Int','Nint','Sgn','Step','Abs',
     %  'Frac','Sqrt','Exp','Log','Log10',
     %  'Cos','Sin','Tan','ArcSin','ArcCos','ArcTan',
     %  'Cosh','Sinh','Tanh','ArcSinh','ArcCosh','ArcTanh',
     %  'Gamma',
     %  'Mod','Atan2',
     %  'Min','Max',
C List of reserved parameter names
C Unchangeable predefined parameters in eval        (MRSVP2)
     %  'Pi','E','Euler',
C Unchangeable predefined parameters in CAIN        (MRSVP3)
     %  'Deg','Cvel','Hbar','Hbarc','Emass','Echarge',
     %  'Reclass','LambdaC','FinStrC',
C Parameters that CAIN may change but users cannot  (MRSVP4)
     %  'T','X','Y','S','En','Px','Py','Ps',
     %  'Sx','Sy','Ss','Xi1','Xi2','Xi3',
     %  'Kind','Gen','Wgt','Incp','$PName',
     %  'Time','Sbl','Convergence',
     %  'L0','L1','L2','L3','L4',
     %  'L00','L10','L20','L30','L01','L11','L21','L31',
     %  'L02','L12','L22','L32','L03','L13','L23','L33','W',
     %  '$PrevMag','$NextMag','$InFileName','$InFilePath'/
C            When changing particle variables (T to Incp),
C            must be careful in changing SETVAR and SETVAR2.
C            (The last item is now Incp)
      REAL*8 RSVP30(MRSVP3)/
     %   1.745329251994329D-02,
C       Pi/180
     %   2.99792458D8,1.05457266D-34,1.97327053D-7,
C          meter        Joule*sec      eV*meter
     %   0.51099906D6,1.60217733D-19,
C          eV/c**2       Coulomb
     %   2.81794092D-15,3.86159323D-13,7.2973530796D-3/
C          meter         meter   
C
      CHARACTER*16 UFNAMEX(MUFN)/
     %   'NParticle','NMacro',
     %   'AvrT','AvrX','AvrY','AvrS',
     %   'AvrEn','AvrPx','AvrPy','AvrPs',
     %   'AvrSx','AvrSy','AvrSs','AvrXi1','AvrXi2','AvrXi3',
     %   'SigT','SigX','SigY','SigS',
     %   'SigEn','SigPx','SigPy','SigPs',
     %   'SigSx','SigSy','SigSs','SigXi1','SigXi2','SigXi3',
     %   'BeamMatrix',
     %   'Lum','LumH','LumP',
     %   'LumW','LumWbin','LumWbinEdge','LumWH','LumWP',
     %   'LumEE','LumEEbin','LumEEbinEdge','LumEEH','LumEEP',
     %   'LaserIntensity','LaserRange',
     %   'BesJ','DBesJ',
     %   'BesK','DBesK','BesK13','BesK23','BesKi13','BesKi53',
     %   'FuncBS','FuncCP','IntFCP',
     %   'Strlen','AtoF','$FtoA','$ItoA','$Substr','Strstr',
     %   '$ToUpper','$ToLower',
     %   'TestT','TestX','TestY','TestS',
     %   'TestEn','TestPx','TestPy','TestPs',
     %   'Beta','Alpha','Eta','Etaprime','Nu',  ! Order of Twiss params must be kept
     %   'TMat'/
      INTEGER NARGUFN0(MUFN)
      DATA NARGUFN0/
     %    2*1000,29*1000,
     %    2,3,4,
     %    3,3,3,4,5,
     %    4,4,4,5,6,
     %    1000,1000,
     %    2,2,
     %    6*1000,
     %    2,2,1000,
     %    1,1,2,1000,1000,2,1,1,
     %    1,1,1,1,1,1,1,1,
     %    6*1000/
C       Number of arguments. 1000: accept various forms
C
      CHARACTER*15 NAMPPI0(MPPI)/'Breit-Wheeler','Bethe-Heitler',
     %   'Landau-Lifshitz','Breamsstrahlung'/
C
      INTEGER I
C
      CVEL=2.99792458D8
      RE=2.81794092D-15
      CMPTNWL=3.86159323D-13
      ECHARG=1.60217733D-19
      FINSTRC=0.00729735308D0
      EMASS=0.51099906D6
      MASS(1)=0D0
      MASS(2)=0.51099906D6
      MASS(3)=MASS(2)
      CHARGE(1)=0D0
      CHARGE(2)=-1D0
      CHARGE(3)=1D0
      ANOM(1)=0D0
      ANOM(2)=0.001159652193D0
      ANOM(3)=ANOM(2)
	HBARC=1.97327053D-7
C
      DO 200 I=1,MRSVP
        RSVPAR(I)=RSVPAR0(I)
 200  CONTINUE
      DO 220 I=1,MRSVP3
        RSVP3(I)=RSVP30(I)
 220  CONTINUE
      DO 240 I=1,MUFN
        UFNAME(I)=UFNAMEX(I)
	  IF(UFNAMEX(I).EQ.'NParticle') IDUFBSF=I
        IF(UFNAMEX(I).EQ.'Lum') IDUFLM=I
        IF(UFNAMEX(I).EQ.'LaserIntensity') IDUFLSR=I
	  IF(UFNAMEX(I).EQ.'BesJ') IDUFSPF=I
	  IF(UFNAMEX(I).EQ.'Strlen') IDUFCH=I
	  IF(UFNAMEX(I).EQ.'TestT') IDUFTSTP=I
	  IF(UFNAMEX(I).EQ.'Beta') IDUFTWS=I
	  IF(UFNAMEX(I).EQ.'TMat') IDUFTMAT=I
 240  CONTINUE
      DO 250 I=1,MUFN
        NARGUFN(I)=NARGUFN0(I)
 250  CONTINUE
C
      DO 400 I=1,MPPI
        NAMPPI(I)=NAMPPI0(I)
 400  CONTINUE
      RETURN
      END
