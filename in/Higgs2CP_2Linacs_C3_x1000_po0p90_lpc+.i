!  ILC  Gamma-Gamma Collider Conversion point
!  Takes 24 sec on DEC workstation at KEK
 HEADER  'ILC G-G  for a Higgs Factory 3;    G G;';
 ALLOCATE MP=90000000, MVPH=1000000, MLUMMESH=256, ;
SET Rand=9885452;
!SET Rand=4819832;
!SET Rand=5223024;
!SET Rand=2986453;
 SET   photon=1, electron=2, positron=3, ntcut=2.5,
   mm=1D-3, micron=1D-6, nm=1D-9, mu0=4*Pi*1D-7,
!! Beam parameters (C^3)
ee=62.8D9,  gamma=ee/Emass,  
an=0.63D10,  
!sigz=7.5*micron, 
!sigz=10*micron, 
sigz=30*micron, 
!sigz=15*micron, 
betax=0.03*mm, 
betay=0.03*mm, 
emitx=0.12D-6/gamma, 
emity=0.12D-6/gamma,
nbunch=75, reprate=120,
!
sigx=Sqrt(emitx*betax), sigy=Sqrt(emity*betay), 
sigxp=sigx/betax,  sigyp=sigy/betay,
slopex=0.000, slopey=0.000,
Smesh=sigz/3,
!emax=1.001*ee, wmax=2*emax,
emax=1.01*ee, wmax=2*emax,
!
   off=0, ! off=sigy, ! to reduce the e- e- collisions with an offset
!! Laser parameters
laserwl=1.19*nm, lambar=laserwl/(2*Pi), omegal=Hbarc/lambar,
!pulseE=0.5,
!pulseE=0.58,
!pulseE=0.59,
!pulseE=0.6,
!pulseE=0.65,
!pulseE=0.075,
!pulseE=0.15,
pulseE=0.72,
!pulseE=8000.,
!pulseE=0.45D9,
!pulseE=1.31,
!pulseE=1.75,
!pulseE=1.,
!pulseE=2.18,
!pulseE=5.12,
!pulseE=1.332,
!pulseE=5.,
!pulseE=140.4,
!pulseE=7.2,
!agamma=3.42*micron,
!agamma=800*micron,
!agamma=10.*nm,
!agamma=20.*nm,
!agamma=25.15*nm,
agamma=37.69*nm,
!agamma=53.30*nm,
!agamma=75.378*nm,
!agamma=33*nm,
!agamma=35.38*nm,
!agamma=40*nm,
!agamma=18.85*nm,
betagamma=2*Pi*agamma^2/laserwl,
   rlx=betagamma,
   rly=betagamma,
   sigt=betagamma,
!rlx=3.2*mm, rly=0.016*mm, 
   ! sigt=120*micron,!sigt=5.2*micron,
!  sigt=30*micron,
!   sigt=7.5*micron,
!  sigt=15*micron,
  
angle=0.000, ! angle=0.001, !angle=0.005, 
tcp=-0.00*mm,  ! tcp=time laser pulse is focussed at dcp
!tcp=-0.01*mm,  ! tcp=time laser pulse is focussed at dcp
!tcp=-0.02*mm,  ! tcp=time laser pulse is focussed at dcp
dcp=0.0*mm,  ! dcp=CP-to-IP distance
!dcp=0.1*mm,  ! dcp=CP-to-IP distance
!dcp=0.15*mm,  ! dcp=CP-to-IP distance
!dcp=0.2*mm,  ! dcp=CP-to-IP distance
! powerd=pulseE*Cvel/[Pi*lambar*sigt*Sqrt(2*Pi*rlx*rly)],
powerd=pulseE*Cvel/[2*Pi*agamma^2*betagamma],
xisq=powerd*mu0*Cvel*(lambar/Emass)^2, xi=Sqrt(xisq),
eta=omegal*ee/Emass^2, lambda=4*eta; 
!!

SET lgeo=1D-4/4/Pi*nbunch*reprate*an^2/sigx/sigy, wee=2*ee, wlow=0.95*wee, wnbin=50, wbinsize=(wee-wlow)/wnbin;

PRINT lgeo, FORMAT=(' geometric luminosity = ', 1P,E12.4,' cm-2 s-1');
PRINT wee/1d9, FORMAT=(' Wee = ', F10.4,' GeV');
PRINT wlow/1d9, FORMAT=(' Wlow = ', F10.4,' GeV');
PRINT wnbin, FORMAT=(' Wnbin = ', I8);
PRINT wbinsize/1d9, FORMAT=(' Wbinsize = ', F10.4,' GeV');


DEBUGFLAG ID=8;

! SET MsgLevel=1000000,spinleft=0; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
! SET MsgLevel=1000000,spinleft=-0.5; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
  SET MsgLevel=1000000,spinleft=-0.9; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
! SET MsgLevel=1000000,spinleft=-1.0; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
!!
BEAM  RIGHT, KIND=electron, NP=50000, AN=an, E0=ee,
  TXYS=(0,0,off/2,0),  GCUTT=ntcut,
 BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,-spinleft);
!
BEAM  LEFT,  KIND=electron, NP=50000, AN=an, E0=ee,
  TXYS=(0,0,-off/2,0),  GCUTT=ntcut,
  BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,spinleft);
!
!! LASER
! TXYS is the laser focal point
! E3 is the unit vector of laser beam, so the vertical crossing, E1 is the unit vector perpendicular to E3
! STOKES; (0,1,0) circular pol. along the direction of the laser
!

SET incLasers=1;


IF incLasers > 0;
LASER LEFT, WAVEL=laserwl, POWERD=powerd,
     TXYS=(tcp,0,off/2,-dcp),
     E3=(0,-Sin(angle),-Cos(angle)), E1=(1,0,0), 
     RAYLEIGH=(rlx,rly), TTOT=2*sigt, GCUTT=ntcut,
     TDL=(2,2), STOKES=(0,1,0) ;

 LASER RIGHT, WAVEL=laserwl, POWERD=powerd,
      TXYS=(tcp,0,-off/2,dcp),
      E3=(0,-Sin(angle),Cos(angle)), E1=(1,0,0), 
      RAYLEIGH=(rlx,rly), TTOT=2*sigt, GCUTT=ntcut,
      TDL=(2,2), STOKES=(0,1,0) ;
!
LASERQED  COMPTON, NPH=0, XIMAX=1.1*xi, LAMBDAMAX=1.1*lambda, PSTOP=1000 ;
!LASERQED  BREITW, NPH=0, XIMAX=1.1*xi, ETAMAX=1.1*eta, PSTOP=1000 ;
ENDIF;

 SET MsgLevel=0;  FLAG OFF ECHO;
 SET  it=0;
 PRINT CPUTIME;






SET   incpair=0;
!!
SET xpar=4*ee*omegal/Emass/Emass,
egmax=xpar/(xpar+1)*ee;
 SET tmin=-ntcut*sigz, tmax=ntcut*sigz, nstep=300;
 SET wmin=2.*egmax*0.95,wgmax=2*egmax,wbin=50;
LUMINOSITY  KIND=(photon,photon), W=(wlow,wee,wnbin), HELICITY,
        WX=(8*sigx,16*sigx),WY=(10*sigy,16*sigy), FREP=nbunch*reprate ;
LUMINOSITY  KIND=(electron,electron), W=(0,wmax,wbin);
LUMINOSITY  KIND=(photon,electron), W=(0,wmax,wbin);
LUMINOSITY  KIND=(electron,photon), W=(0,wmax,wbin);
LUMINOSITY  KIND=(photon,positron), W=(0,wmax,50);
LUMINOSITY  KIND=(positron,photon), W=(0,wmax,50);
LUMINOSITY  KIND=(electron,positron), W=(0,wmax,50), HELICITY ;
LUMINOSITY  KIND=(positron,electron), W=(0,wmax,50), HELICITY ;
 BBFIELD  NX=32, NY=32, WX=8*sigx, R=sigx/sigy/8,WXMAX=16*sigx;
!  Define parameters for beam-beam field calculation
!! BBFIELD  NX=32, NY=64, WX=12*sigx, R=sigx/sigy/2;
!CFQED BEAMSTRAHLUNG, POL, PMAX=0.8;
!CFQED PAIR, POL;
! FLAG OFF SPIN;

!  Turn on incoherent pair
 IF incpair > 0;
   SET enhpp=1;
   PPINT VIRTUALPHOTON, FIELDSUPPRESSION, EMIN=3.0E06;
   PPINT BW, ENHANCE=enhpp;
   PPINT BH, ENHANCE=enhpp;
   PPINT LL, ENHANCE=enhpp;
 ENDIF;
 PUSH  Time=(tmin,tmax,nstep) ;
    IF Mod(it,1)=0;
PRINT it, Time/1e-3, NParticle(1,1,'En>30e9'),  SigX(1,1,'En>30e9')/nm, FORMAT=(' +++ ',F6.0,'-th time step   t=',F7.2,' mm',' Right-going E> 30 GeV',' nPhotons=',E12.4,' SigX (nm)= ',E12.4);
PRINT it, Time/1e-3, NParticle(1,1,'En>60e9'),  SigX(1,1,'En>60e9')/nm, FORMAT=(' +++ ',F6.0,'-th time step   t=',F7.2,' mm',' Right-going E> 60 GeV',' nPhotons=',E12.4,' SigX (nm)= ',E12.4);
PRINT it, Time/1e-3, NParticle(2,1,'En>30e9'),  SigX(2,1,'En>30e9')/nm, FORMAT=(' +++ ',F6.0,'-th time step   t=',F7.2,' mm',' Left-going E> 30 GeV',' nPhotons=',E12.4,' SigX (nm)= ',E12.4);
PRINT it, Time/1e-3, NParticle(2,1,'En>60e9'),  SigX(2,1,'En>60e9')/nm, FORMAT=(' +++ ',F6.0,'-th time step   t=',F7.2,' mm',' Left-going E> 60 GeV',' nPhotons=',E12.4,' SigX (nm)= ',E12.4);
      PRINT STAT;
    ENDIF;
    IF it=Nint(nstep/2);
!!      PLOT  SCAT, KIND=electron, RIGHT, H=S/micron, V=Y/nm,
      PLOT  SCAT, KIND=electron, H=S/micron, V=Y/nm,
            FILE='HiggsIP.top',
            HSCALE=(-ntcut*sigz/micron,ntcut*sigz/micron),
            VSCALE=(-50*sigy/nm,50*sigy/nm), MAXNP=4000,
            TITLE='Electron at IP;',
            HTITLE='S (Mm);   G  ;', VTITLE='Y (nm);' ;
!!
PLOT  BBFIELD, S=0,
FILE='HiggsIP.top', APPEND;
!!
    ENDIF;


IF Mod(it,5)=0;

PRINT it, Time/1e-3, NParticle(3,1), FORMAT=(F6.0,'-th time step   t=',F7.2,' mm',' nPhotons=',E12.4); PRINT STAT;
!
PLOT SCAT, KIND=2, COLOR='BLUE', MAXNP=10000, H=S/micron, V=X/nm,
HTITLE='S (micron);', VTITLE='X (nm);',
HSCALE=(-200,200), VSCALE=(-250,250),
TITLE='t='+$FtoA(Time/1e-3,'(F10.5)')+'mm;',
FILE='ComptonXPlots.top', APPEND;

PLOT SCAT, KIND=2, COLOR='BLUE', MAXNP=10000, H=S/micron, V=Y/nm,
HTITLE='S (micron);', VTITLE='Y (nm);',
HSCALE=(-200,200), VSCALE=(-250,250),
TITLE='t='+$FtoA(Time/1e-3,'(F10.5)')+'mm;',
FILE='ComptonYPlots.top', APPEND;

   IF NParticle(3,1)>0.5;
      PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=100000;
   ENDIF;

ENDIF;









    SET it=it+1;
 ENDPUSH;












 PRINT CPUTIME;
 FLAG ON ECHO;
!  Pull all particles back to the plane s=0
DRIFT S=0;
!
PRINT STAT;
PRINT STAT, INCP;
PLOT  HIST, KIND=electron, H=En/1E9, HSCALE=(0,emax/1E9,50), VLOG,
  FILE='HiggsIP.top', APPEND,
        TITLE='Final Electron Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';

SET angmax=200.;

PLOT  HIST, KIND=electron,RIGHT,
  FILE='HiggsIP.top', APPEND,
        H=Py/Ps/mm,
!        HSCALE=(-3,3),
        HSCALE=(-angmax,angmax),
        TITLE='Final Right-going Electron Angle Distribution;',
!        HTITLE='Y (Mrad);   G    ;' ;
        HTITLE='Y (mrad);        ;' ;
PLOT  HIST, KIND=electron,LEFT,  
  FILE='HiggsIP.top', APPEND,
        H=Py/Abs(Ps)/mm,
!        HSCALE=(-3,3),
        HSCALE=(-angmax,angmax),
        TITLE='Final Left-going electron Angle Distribution;',
        HTITLE='Y (mrad);        ;' ;

PLOT  SCAT, KIND=electron,RIGHT,  MAXNP=60000,
  FILE='HiggsIP.top', APPEND,
        H=Px/Ps/mm-slopex/mm, V=Py/Ps/mm,
!        HSCALE=(-15*sigxp/mm,15*sigxp/mm),
        HSCALE=(-angmax,angmax),
        VSCALE=(-angmax,angmax),
        TITLE='Final Right-going Electron Angle Distribution;',
        HTITLE='X (mrad);      ;',
        VTITLE='Y (mrad);      ;' ;

PLOT  SCAT, KIND=electron,LEFT,  MAXNP=60000,
  FILE='HiggsIP.top', APPEND,
        H=Px/Abs(Ps)/mm+slopex/mm, V=Py/Abs(Ps)/mm,
!        HSCALE=(-15*sigxp/micron,15*sigxp/micron),
!        VSCALE=(-5000,5000),
        HSCALE=(-angmax,angmax),
        VSCALE=(-angmax,angmax),
        TITLE='Final Left-going Electron Angle Distribution;',
!        HTITLE='X (Mrad);   G    ;',
!        VTITLE='Y (Mrad);   G    ;' ;
        HTITLE='X (mrad);      ;',
        VTITLE='Y (mrad);      ;' ;
!!
 PLOT  SCAT, KIND=electron,VLOG, HLOG,
  FILE='HiggsIP.top', APPEND,
        V=En/1D9, H=Sqrt[(Px^2+Py^2)/Ps^2]/mm,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Electron Angle vs. Energy after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

   PLOT  SCAT, KIND=electron,VLOG, HLOG,
  FILE='HiggsIP.top', APPEND,
          V=Sqrt[(Px^2+Py^2)]/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          HSCALE=(1,1500),
          VSCALE=(0.005,0.2),
          TITLE='I Electron Angle-Pt Distribution;',
          VTITLE='Pt (GeV);',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  HIST, KIND=electron,VLOG,
  FILE='HiggsIP.top', APPEND,
H=Sqrt[(Px^2+Py^2)]/1E9,HSCALE=(0.0,0.2),
          TITLE='Electron Pt Spectrum;',
          HTITLE='P0t1  (GeV); X X       ;';

!PLOT  HIST, KIND=electron,VLOG,
!  FILE='HiggsIP.top', APPEND,
!H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,HSCALE=(0.,angmax/2),
!           SELECT=(Sqrt[(Px^2+Py^2)] >10e6),
!          TITLE='Electron Angle Spectrum for Pt>10MeV;',
!        HTITLE='Q0e1 (mrad);GX X       ;' ;
!!
PLOT LUMINOSITY, KIND=(photon,photon), FILE='HiggsLumi.top';
PLOT LUMINOSITY, KIND=(photon,electron), FILE='HiggsLumi.top', APPEND;
PLOT LUMINOSITY, KIND=(electron,photon), FILE='HiggsLumi.top', APPEND;
PLOT LUMINOSITY, KIND=(electron,electron), FILE='HiggsLumi.top', APPEND;
PLOT LUMINOSITY, KIND=(photon,positron), FILE='HiggsLumi.top', APPEND;
PLOT LUMINOSITY, KIND=(positron,photon), FILE='HiggsLumi.top', APPEND;
PLOT LUMINOSITY, KIND=(electron,positron), FILE='HiggsLumi.top', APPEND;
PLOT LUMINOSITY, KIND=(positron,electron), FILE='HiggsLumi.top', APPEND;
!
PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='Plots.top',
        V=En/1D9, H=Sqrt[(Px^2+Py^2)/Ps^2]/mm,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Electron Angle vs. Energy after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

 PLOT  HIST, RIGHT, KIND=electron, H=En/1D9, HSCALE=(0,emax/1e9,50),
        FILE='Plots.top', APPEND,
        TITLE='Right-Going Electron Energy Spectrum after CP;',
        HTITLE='E0e1 (GeV); X X      ;';
 PLOT  HIST, LEFT, KIND=electron, H=En/1D9, HSCALE=(0,emax/1e9,50),
        FILE='Plots.top', APPEND,
        TITLE='Left-Going Electron Energy Spectrum after CP;',
        HTITLE='E0e1 (GeV); X X      ;';

IF incLasers > 0;

PLOT  HIST, KIND=photon, H=En/1E9, HSCALE=(0,emax/1E9,50), VLOG,
  FILE='HiggsIP.top', APPEND,
        TITLE='Photon Energy Spectrum;',
        HTITLE='E0G1  (GeV); XGX       ;';
 PLOT  HIST, KIND=photon, H=En/1D9, HSCALE=(0,emax/1e9,50),
        FILE='Plots.top', APPEND,
        TITLE='All Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;
 PLOT  HIST, KIND=photon, RIGHT, SELECT=(Gen==2), H=En/1D9, HSCALE=(0,emax/1e9,50),
        FILE='Plots.top', APPEND,
       TITLE='Right-Going Primary Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;
 PLOT  HIST, LEFT,KIND=photon, SELECT=(Gen==2),H=En/1D9, HSCALE=(0,emax/1e9,50),
        FILE='Plots.top', APPEND,
       TITLE='Left-Going Primary Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;

PLOT  SCAT, RIGHT, KIND=photon, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-5*sigx/nm,5*sigx/nm),
        VSCALE=(-10*sigy/nm,10*sigy/nm),
        TITLE='Right-Going Primary Photon IP Spot;',
        HTITLE='X (nm);',  VTITLE='Y (nm);' ;

 PLOT  SCAT, KIND=photon, RIGHT, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=En/1D9, V=Xi2,
        HSCALE=(0,emax/1e9), VSCALE=(-1.5,1.5),
        TITLE='Right-Going Primary Photon Energy vs. Polarization after CP;',
        HTITLE='E0G1 (GeV); XGX      ;',
        VTITLE='X021; X X;',;
PLOT  SCAT, KIND=photon, LEFT, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=En/1D9, V=Xi2,
        HSCALE=(0,emax/1e9), VSCALE=(-1.5,1.5),
        TITLE='Left-Going Primary Photon Energy vs. Polarization after CP;',
        HTITLE='E0G1 (GeV); XGX      ;',
        VTITLE='X021; X X;',;

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=0, H=X/nm, V=Y/nm,
        FILE='Plots.top', APPEND,
HTITLE='X (nm);', VTITLE='Y (nm);',
HSCALE=(-250,250), VSCALE=(-250,250),
  TITLE=' X vs S;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=0, H=En/1D9, V=X/nm,
        FILE='Plots.top', APPEND,
HTITLE='Photon energy (GeV);', VTITLE='X (nm);',
  HSCALE=(0,emax/1e9), VSCALE=(-250,250),
  TITLE=' X vs photon Energy;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=0, H=En/1D9, V=X/nm,
        FILE='Plots.top', APPEND,
HTITLE='Photon energy (GeV);', VTITLE='X (nm);',
  HSCALE=(0.5*emax/1e9,emax/1e9), VSCALE=(-40,40),
  TITLE=' X vs photon Energy v2;';


ENDIF;



 IF incpair >0;

   PLOT  HIST, INCP, H=En/1E9, HSCALE=(0,emax/1E9), VLOG,
  FILE='HiggsIP.top', APPEND,
          TITLE='Incoherent Pair Energy Spectrum;',
          HTITLE='E0e1  (GeV); X X       ;';
 
  PLOT  SCAT, INCP,  MAXNP=140000, VLOG, HLOG,
  FILE='HiggsIP.top', APPEND,
          V=En/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          VSCALE=(0.001,emax/1e9),
          HSCALE=(1,1500),
          TITLE='Incoherent Pair Angle-Energy Distribution;',
          VTITLE='E (GeV);',
          HTITLE='Q (mrad);G       ;';

   PLOT  SCAT, INCP,   MAXNP=140000,
  FILE='HiggsIP.top', APPEND,
          H=ArcTan[Px/Abs(Ps)]/mm, V=ArcTan[Py/Abs(Ps)]/mm,
          HSCALE=(-500,500),
          VSCALE=(-500,500),
          TITLE='Incoherent Pair Angle Distribution;',
          HTITLE='X (mrad);         ;',
          VTITLE='Y (mrad);         ;' ;

   PLOT  SCAT, INCP,   MAXNP=140000, VLOG, HLOG,
  FILE='HiggsIP.top', APPEND,
          V=Sqrt[(Px^2+Py^2)]/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          HSCALE=(1,1500),
          VSCALE=(0.005,0.2),
          TITLE='Incoherent Pair Angle-Pt Distribution;',
          VTITLE='Pt (GeV);',
          HTITLE='Q (mrad);G       ;';

! Detector solenoid magnet, B-field in positive z direction, distance of BeamCal from IP
SET bmag=3.5, zbcal=3.6, phibm=0.3*bmag*zbcal*0.5*1E9, ptb=0.3*bmag*1E9;

   PLOT  SCAT, INCP,   MAXNP=140000,
  FILE='HiggsIP.top', APPEND,
          V=En/1E9, H=Abs[2*Sqrt(Px^2+Py^2)/1E9/0.3/bmag*Sin{0.3*bmag*zbcal/Abs(Ps)*1E9*0.5}*100], 
          SELECT=(Ps>0.0),
          VSCALE=(0,3),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Radius-Energy Distribution (Pz>0);',
          VTITLE='E (GeV);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';

   PLOT  SCAT, INCP,   MAXNP=140000,
  FILE='HiggsIP.top', APPEND,
          V=En/1E9, H=Abs[2*Sqrt(Px^2+Py^2)/1E9/0.3/bmag*Sin{0.3*bmag* zbcal/Abs(Ps)*1E9*0.5}*100], 
          SELECT=(Ps<0.0),
          VSCALE=(0,3),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Radius-Energy Distribution (Pz<0);',
          VTITLE='E (GeV);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';

!!SET bmag=3.5, zbcal=3.6, phibm=0.3*bmag*zbcal*0.5*1E9, ptb=0.3*bmag*1E9;


   PLOT  SCAT, INCP,   MAXNP=140000, 
  FILE='HiggsIP.top', APPEND,
          H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]*Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), 
          V=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]*Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)),
!         SELECT=(2.0<2*Sqrt(Px^2+Py^2)/ptb*Abs[Sin{phibm/Abs(Ps)}]*100), 
          SELECT=(Ps>0.0),
         VSCALE=(-10,10),
          HSCALE=(-10,10),
          TITLE='Incoherent Pair Distribution (Pz>0);',
          VTITLE='y (cm);',
          HTITLE='x (cm) in R> 2cm at B=3.5T, L=3.6m;';
   PLOT  SCAT, INCP,   MAXNP=140000, 
  FILE='HiggsIP.top', APPEND,
          H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]*Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), 
          V=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]*Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)),
!         SELECT=(2.0<2*Sqrt(Px^2+Py^2)/ptb*Abs[Sin{phibm/Abs(Ps)}]*100), 
          SELECT=(Ps<0.0),
         VSCALE=(-10,10),
          HSCALE=(-10,10),
          TITLE='Incoherent Pair Distribution (Pz<0);',
          VTITLE='y (cm);',
!          HTITLE='x (cm) in R> 2cm at B=3.5T, L=3.6m;';
          HTITLE='x (cm)  at B=3.5T, L=3.6m;';


!!
    PLOT  SCAT, INCP,   MAXNP=140000, 
  FILE='HiggsIP.top', APPEND,
          V=Atan2[Sin(2.*phibm/Ps),Cos(2.*phibm/Ps)],
          H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100],
          SELECT=(Ps>0.0),
          VSCALE=(-Pi,Pi),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Distribution (Pz>0);',
          VTITLE='Rotation angle of the helix (rad);',
          HTITLE='r (cm) at B=3.5T, L=3.6m;';
    PLOT  SCAT, INCP,   MAXNP=140000, 
  FILE='HiggsIP.top', APPEND,
          V=Atan2[Sin(2.*phibm/Ps),Cos(2.*phibm/Ps)],
          H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100],
          SELECT=(Ps<0.0),
          VSCALE=(-Pi,Pi),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Distribution (Pz<0);',
          VTITLE='Rotation angle of the helix (rad);',
          HTITLE='r (cm) at B=3.5T, L=3.6m;';


   PLOT  HIST, INCP, H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100],
          SELECT=(Ps>0.0),
          HSCALE=(0,10),  VLOG,
          TITLE='Incoherent Pair Radial Spectrum (Pz>0);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';
   PLOT  HIST, INCP, H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100],
  FILE='HiggsIP.top', APPEND,
          SELECT=(Ps<0.0),
          HSCALE=(0,10),  VLOG,
          TITLE='Incoherent Pair Radial Spectrum (Pz<0);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';
!!

!!
    PLOT  HIST, INCP, 
  FILE='HiggsIP.top', APPEND,
         H=($PName='ILL ')*1+($PName='IBH ')*2+($PName='IBW ')*3,
         HSCALE=(0,5), VLOG,
         TITLE='Incoherent Pair creation processes;',
         HTITLE='1 for LL, 2 for BH and 3 for BW;';
!!
    PLOT  HIST, INCP, 
  FILE='HiggsIP.top', APPEND,
          H=( (Kind==3)*Step(Ps)+2*(Kind==2)*Step(Ps)+3*(Kind==2)*Step(-Ps)+4*(Kind==3)*Step(-Ps) ),
          HSCALE=(0,5),
          TITLE='Incoherent Pair kinds;',
          HTITLE='1 for e+,Ps>0, 2 for e-,Ps<0, 3 for e-,Ps>0, 4 for e+<Ps<0;';
!!

SET rmin=3.0,rmax=4.0,rmin2=4.0,rmax2=5.0;  !  for radial region in the pair monitor 

    PLOT  HIST, INCP,
  FILE='HiggsIP.top', APPEND,
H=Atan2[ Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)),Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
          HSCALE=(-Pi,Pi), VLOG,
         SELECT=(rmin<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax&&Ps>0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz>0);',
          HTITLE='Azimuthal angle in 3<R<4cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP,
  FILE='HiggsIP.top', APPEND,
H=Atan2[Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
         HSCALE=(-Pi,Pi), VLOG,
         SELECT=(rmin<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax&&Ps<0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz<0);',
          HTITLE='Azimuthal angle in 3<R<4cm at B=3.5T, L=3.6m;';
!!
    PLOT  HIST, INCP,
  FILE='HiggsIP.top', APPEND,
H=Atan2[ Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
         HSCALE=(-Pi,Pi), VLOG,
         SELECT=(rmin2<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax2&&Ps>0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz>0);',
          HTITLE='Azimuthal angle in 4<R<5cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP,
  FILE='HiggsIP.top', APPEND,
H=Atan2[ Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
         HSCALE=(-Pi,Pi), VLOG,
         SELECT=(rmin2<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax2&&Ps<0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz<0);',
          HTITLE='Azimuthal angle in 4<R<5cm at B=3.5T, L=3.6m;';

!!
    PLOT  HIST, INCP, H=En/1E9,
  FILE='HiggsIP.top', APPEND,
        HSCALE=(0,5),VLOG,
         SELECT=(rmin<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax&&Ps>0.0),
          TITLE='Incoherent Pair Energy (Pz>0);',
         HTITLE='E (GeV) in 3<R<4cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP, H=En/1E9,
  FILE='HiggsIP.top', APPEND,
          HSCALE=(0,5),VLOG,
         SELECT=(rmin<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax&&Ps>0.0),
          TITLE='Incoherent Pair Energy (Pz<0);',
          HTITLE='E (GeV) in 3<R<4cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP, H=En/1E9,
  FILE='HiggsIP.top', APPEND,
          HSCALE=(0,5),VLOG,
          SELECT=(rmin2<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax2&&Ps>0.0),
          TITLE='Incoherent Pair Energy (Pz>0);',
          HTITLE='E (GeV) in 4<R<5cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP, H=En/1E9,
  FILE='HiggsIP.top', APPEND,
          HSCALE=(0,5),VLOG,
         SELECT=(rmin2<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<rmax2&&Ps>0.0),
          TITLE='Incoherent Pair Energy (Pz<0);',
          HTITLE='E (GeV) in 4<R<5cm at B=3.5T, L=3.6m;';

ENDIF;
!!
PRINT Lum(1,1),FORMAT=('luminosity_gam gam=',1pd15.8);
PRINT Lum(1,2),FORMAT=('luminosity_gam e-=',1pd15.8);
PRINT Lum(2,1),FORMAT=('luminosity_gam e-=',1pd15.8);
PRINT Lum(2,2),FORMAT=('luminosity_e-e-=',1pd15.8);
PRINT Lum(1,3),FORMAT=('luminosity_gam e+=',1pd15.8);
PRINT Lum(2,3),FORMAT=('luminosity_e-e+=',1pd15.8);
! 
 STOP;




