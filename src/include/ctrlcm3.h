      INTEGER MRSVP1,MRSVP2,MRSVP3,MRSVP4,MRSVP
      PARAMETER (MRSVP1=27,MRSVP2=3,MRSVP3=9,MRSVP4=48)
C  MRSVP1= Number of standard math functions
C               e.g., Sin
C  MRSVP2= Number of Unchangeable predefined parameters in eval
C               e.g., Pi
C  MRSVP3= Number of Unchangeable predefined parameters in CAIN
C               e.g., Emass
C  MRSVP4= Number of Parameters that CAIN may change but users cannot 
C               e.g., T, X, Y, ...
      PARAMETER (MRSVP=MRSVP1+MRSVP2+MRSVP3+MRSVP4)
      INTEGER MUFN
      PARAMETER (MUFN=79)
C  MUFN    Number of CAIN functions
C               e.g., AvrT, SigX, ..
C  See setcnst.f for definitions of CAIN functions

      REAL*8 RSVP3(MRSVP3)
      CHARACTER*16 RSVPAR(MRSVP)
      CHARACTER*16 UFNAME(MUFN)
      INTEGER IDRSVP(MRSVP4),IPTXYS,IPEP,IPSXYS,IPX123,
     %   IPKIND,IPGEN,IPWGT,IPINCP,IPPNAME,IPL0
C           See lumplt1.f if you change IPL0 
C           (note also 22=total number of lum functions)
      INTEGER IDUFBSF,IDUFLM,IDUFLSR,IDUFSPF,IDUFCH,
     %    IDUFTSTP,IDUFTWS,IDUFTMAT,NARGUFN(MUFN)
      COMMON/CTRLCM3/RSVP3,
     %   IDRSVP,IPTXYS,IPEP,IPSXYS,IPX123,IPKIND,IPGEN,IPWGT,
     %   IPINCP,IPPNAME,IPL0,
     %   IDUFBSF,IDUFLM,IDUFLSR,IDUFSPF,IDUFCH,
     %   IDUFTSTP,IDUFTWS,IDUFTMAT,NARGUFN
      COMMON/CTRLCM3C/RSVPAR,UFNAME






