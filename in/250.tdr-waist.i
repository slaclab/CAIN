! ILC - TDR  waist scan
 SET   incpair=0;
 SET    travelingfocus=1;

!SET Rand=12345;
SET Rand=67890;
 ALLOCATE MP=4000000, MVPH=2000000, MLUMMESH=256, MBBXY=256, MLUMMESH=128, ;
FLAG OFF SPIN;
SET   mm=1E-3, micron=1E-6, nm=1E-9, nx=4.5, ny=4.5, nt=4.5, ne=4.5, np=100000,
   ee=125E9,  gamma=ee/Emass,  an=2.00E10, nbunch=1312, reprate=5,
   sigz=0.3*mm, betax=13*mm, betay=410*micron,
   emitx=10.0E-6/gamma,  emity=3.5E-8/gamma,
   dalphadty=0, s0=0, 
   sigx=Sqrt(emitx*betax), sigy=Sqrt(emity*betay),
   sigxp=sigx/betax,  sigyp=sigy/betay, sige=0.002,
   offstx=0.*nm,  offsty=0.*nm,
   slopex=0.007, slopey=0.000;
!
IF travelingfocus > 0;
  !SET    s0=Sqrt(3)/2*sigz, dalphdt=-0.5/betay;
  SET    s0=0.6*sigz, dalphadty=0.0;
ENDIF;
PRINT sigx,sigy;
!
SET MsgLevel=1;
!  Define electron/positron beams at IP
 BEAM  RIGHT, KIND=2, NP=np, AN=an, E0=ee,
      EUNIFORM, TXYS=(-s0,offstx,offsty,-s0), 
      BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz,GCUT=(nx,ny),
      DALPHADT=(0, dalphadty),
      GCUTT=nt, GCUTE=ne, SLOPE=(slopex,slopey), CRAB=(slopex,slopey);
 BEAM  LEFT,  KIND=3, NP=np, AN=an, E0=ee,
      EUNIFORM, TXYS=(-s0,-offstx,-offsty,s0), 
      BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz,GCUT=(nx,ny),
      DALPHADT=(0, dalphadty),
      GCUTT=nt, GCUTE=ne, SLOPE=(slopex,slopey), CRAB=(slopex,slopey);
!  Longitudinal mesh size for luminosity and beam-field calculation.
 SET Smesh=0.25*sigz;
!  Define luminosities to be calculated
! LUMINOSITY  KIND=(2,3), W=(0,2*1.001*ee,100),
  LUMINOSITY  KIND=(2,3), W=(210E9,260E9,500),
      WX=8*sigx, WY=8*sigy,  FREP=nbunch*reprate ;
! LUMINOSITY  KIND=(2,1), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(3,1), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(1,1), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(1,2), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(1,3), W=(0,2*1.001*ee,50);

!  Define parameters for beam-beam field calculation
 BBFIELD  NX=128, NY=256, WX=12*sigx, R=sigx/sigy/2;
!  Turn on beamstrahlung
 CFQED    BEAMSTRAHLUNG;
!  Turn on coherent pair (ESLOPE has to be set!!!)
! CFQED    PAIRCREATION;
!  Turn on incoherent pair
 IF incpair > 0;
   SET enhpp=1;
   PPINT VIRTUALPHOTON, FIELDSUPPRESSION, EMIN=3.0E06;
 ! PPINT VIRTUALPHOTON, FIELDSUPPRESSION, EMIN=0.0;
   PPINT BW, ENHANCE=enhpp;
   PPINT BH, ENHANCE=enhpp;
   PPINT LL, ENHANCE=enhpp;
 ENDIF;
 SET MsgLevel=0;
 FLAG OFF ECHO;
 SET it=0, nstep=600;
! SET it=0, nstep=1000;
!  Start tracking
 PUSH  Time=(-3*sigz,3*sigz,nstep);
!      plot (s,y) profile at t=0
    IF it==nstep/2;
      PLOT SCAT, KIND=2, H=S/micron, V=Y/nm,
        HSCALE=(- 1000,1000), VSCALE=(-15,15),
        TITLE='Electron Profile at t=0;',
        HTITLE='s(Mm);  G  ;',
        VTITLE='y(nm);';
    ENDIF;
!     clear opposite-charge paricles to save cpu time
!    IF incpair > 0;
!      CLEAR  BEAM, INCP, KIND=2, RIGHT;
!      CLEAR  BEAM, INCP, KIND=3, LEFT;
!    ENDIF;
    IF Mod(it,10)=0;
      PRINT it, FORMAT=(' +++ ',F5.0,'-th time step +++');
      PRINT STAT, SHORT;
      IF incpair > 0;
        PRINT STAT, INCP, SHORT;
      ENDIF;
    ENDIF;
    SET it=it+1;
 ENDPUSH;
 FLAG ON ECHO;
!  Pull all particles back to the plane s=0
DRIFT S=0;
PRINT STAT;
PRINT STAT, INCP;
! PLOT  HIST, KIND=(2,3), H=En/1E9, HSCALE=(0,1.001*ee/1E9,50), VLOG,
PLOT  HIST, KIND=(2,3), H=En/1E9, HSCALE=(100,130,300), VLOG,
        TITLE='Final Electron Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';


PLOT  HIST, KIND=2,  
        H=Py/Ps/micron,
        HSCALE=(-40*sigyp/micron,40*sigyp/micron),
        TITLE='Final Electron Angle Distribution;',
        HTITLE='Y  (Mrad);    G    ;' ;

PLOT  HIST, KIND=3,  
        H=Py/Ps/micron,
        HSCALE=(-40*sigyp/micron,40*sigyp/micron),
        TITLE='Final Positron Angle Distribution;',
        HTITLE='Y  (Mrad);    G    ;' ;

PLOT  HIST, KIND=1, H=En/1E9, HSCALE=(0,1.001*ee/1E9,50), VLOG,
        TITLE='Beamstrahlung Energy Spectrum;',
        HTITLE='E0G1  (GeV); XGX       ;';

PLOT  SCAT, KIND=1,  MAXNP=600000,
        H=Px/Ps/mm-Ps/Abs(Ps)*slopex/mm, V=Py/Ps/mm,
        HSCALE=(-2.0,2.0),
        VSCALE=(-2.0,2.0),
        TITLE='Beamstrahlung Photon Angle Distribution;',
        HTITLE='X  (mrad);        ;',
        VTITLE='Y  (mrad);        ;' ;

   PLOT  HIST, KIND=1, H=Px/Ps/mm-Ps/Abs(Ps)*slopex/mm, HSCALE=(-2,2,80), VLOG,
          TITLE='Beamstrahlung Photon  Horizontal Angle Distribution;',
        HTITLE='X  (mrad);        ;';

   PLOT  HIST, KIND=1, H=Px/Ps/mm-Ps/Abs(Ps)*slopex/mm, V=En/1E9, HSCALE=(-2,2,80), VLOG,
          TITLE='Beamstrahlung Photon Average Energy at  Horizontal Angle;',
        HTITLE='X  (mrad);        ;';


   PLOT  HIST, KIND=1, H=Py/Ps/mm, HSCALE=(-2,2,80), VLOG,
          TITLE='Beamstrahlung Photon  Vertical Angle Distribution;',
        HTITLE='Y  (mrad);        ;';

   PLOT  HIST, KIND=1, H=Py/Ps/mm, V=En/1E9, HSCALE=(-2,2,80), VLOG,
          TITLE='Beamstrahlung Photon Average Energy at Vertical Angle;',
        HTITLE='Y  (mrad);        ;';


PLOT  SCAT, KIND=2,  MAXNP=600000,
        H=Px/Ps/micron-slopex/micron, V=Py/Ps/micron,
        HSCALE=(-33*sigxp/micron,33*sigxp/micron),
        VSCALE=(-40*sigyp/micron,40*sigyp/micron),
        TITLE='Final Electron Angle Distribution;',
        HTITLE='X  (Mrad);    G    ;',
        VTITLE='Y  (Mrad);    G    ;' ;

PLOT  SCAT, KIND=3,  MAXNP=600000,
        H=Px/Ps/micron+slopex/micron, V=Py/Ps/micron,
        HSCALE=(-33*sigxp/micron,33*sigxp/micron),
        VSCALE=(-40*sigyp/micron,40*sigyp/micron),
        TITLE='Final Positron Angle Distribution;',
        HTITLE='X  (Mrad);    G    ;',
        VTITLE='Y  (Mrad);    G    ;' ;
! 
 IF incpair >0;
   PLOT  HIST, INCP, H=En/1E9, HSCALE=(0,30,50), VLOG,
          TITLE='Incoherent Pair Energy Spectrum;',
          HTITLE='E0G1  (GeV); XGX       ;';
   PLOT  SCAT, INCP,   MAXNP=40000,
          H=En/1E9, V=Sqrt[(Px^2+Py^2)/Ps^2]/mm,
          HSCALE=(0,10),
          VSCALE=(0,50),
          TITLE='Incoherent Pair Energy-Angle Distribution;',
          HTITLE='E (GeV);',
          VTITLE='Q (mrad);G       ;';
   PLOT  SCAT, INCP,   MAXNP=40000,
          H=Px/Ps/mm, V=Py/Ps/mm,
          HSCALE=(-50,50),
          VSCALE=(-50,50),
          TITLE='Incoherent Pair Angle Distribution;',
          HTITLE='X  (mrad);         ;',
          VTITLE='Y  (mrad);         ;' ;
 ENDIF;
! 
! PLOT LUMINOSITY, VLOG;
PLOT LUMINOSITY, KIND=(2,3), VLOG;
PRINT Lum(2,3),FORMAT=('luminosity_ee=',1pd15.8);
!PRINT Lum(1,1),FORMAT=('luminosity_gam gam=',1pd15.8);
!PRINT Lum(2,1),FORMAT=('luminosity_gam e1=',1pd15.8);
!PRINT Lum(3,1),FORMAT=('luminosity_gam e2=',1pd15.8);
!PRINT Lum(1,2),FORMAT=('luminosity_gam e1=',1pd15.8);
!PRINT Lum(1,3),FORMAT=('luminosity_gam e2=',1pd15.8);
! Write particle data onto file
!WRITE BEAM, KIND=1, FILE='cain242-photons.dat';
!WRITE BEAM, KIND=2, FILE='cain242-electrons.dat';
!WRITE BEAM, KIND=3, FILE=cain242-'positrons.dat';
! WRITE LUMINOSITY,KIND=(2,3),FILE='cain242-lum.dat';
! Output differential luminosity
!  SET m1=LumEEbin(2,3,1,0), m2=LumEEbin(2,3,2,0);
!WRITE ((LumEEbin(2,3,1,n1),LumEEbin(2,3,2,n2),LumEE(2,3,n1,n2),n1=1,m1),n2=1,m2),
!  FORMAT=(G20.7," ",G20.7," ",G20.7),FILE='cain242-dif-lum.dat' ;
!IF incpair > 0;
! WRITE BEAM, INCP, FILE='cain242-pairs.dat' ;
!ENDIF;
STOP;
