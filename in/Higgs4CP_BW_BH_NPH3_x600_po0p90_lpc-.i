!  ILC  Gamma-Gamma Collider Conversion point
!  Takes 24 sec on DEC workstation at KEK
 HEADER  'ILC G-G  for a Higgs Factory 3;    G G;';
 ALLOCATE MP=360000000, MVPH=2000000, MLUMMESH=256, ;
!SET Rand=7627499;
!SET Rand=9885452;
!SET Rand=4819832;
!SET Rand=6528341;
!SET Rand=9605844;
SET Rand=12303083;
 SET   photon=1, electron=2, positron=3, ntcut=2.5, pushcut=1.07,
   mm=1D-3, micron=1D-6, nm=1D-9, mu0=4*Pi*1D-7,
sige=0.0005,
ee=62.7D9,  gamma=ee/Emass,
an=0.63D10,  
sigz=15.4*micron,
!sigz=10*micron, 
!sigz=30*micron, 
dcp=0.1*mm,  ! dcp=CP-to-IP distance
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
bdipole=50,
emax=1.01*ee, wmax=2*emax,
!
   off=0, ! off=sigy, ! to reduce the e- e- collisions with an offset
bfieldext=5,
laserwl=1.98333*nm, lambar=laserwl/(2*Pi), omegal=Hbarc/lambar,
incpair=1,
cfbeamstr=1,
cfpair=1,
pushexternal=0,
!pulseE=0.65,
!pulseE=0.72,
!pulseE=0.72,
!pulseE=1.31,
!pulseE=1.75,
!pulseE=1.,
!pulseE=2.18,
!pulseE=5.12,
!pulseE=1.332,
!pulseE=5.,
!pulseE=140.4,
!pulseE=7.2,
!agamma=3.77*nm,
!agamma=25.15*nm,
!agamma=37.69*nm,
agamma=60.45*nm,
!agamma=75.378*nm,
!agamma=33*nm,
nstepip=200,
d1=1,
d2=1,
   agammax=agamma/Sqrt(2),
   agammay=agamma/Sqrt(2),
betagamma=4*Pi*agamma^2/laserwl/Sqrt(d1*d2),
betagammax=4*Pi*agammax^2/laserwl/d1,
betagammay=4*Pi*agammay^2/laserwl/d2,
   rlx=betagammax,
   rly=betagammay,
   sigt=betagamma,
pulseE=0.72,
crossangle=0.000,
  
crabangle=0.000,
laserangle=0.000,
! powerd=pulseE*Cvel/[Pi*lambar*sigt*Sqrt(2*Pi*rlx*rly)],
powerd=pulseE*Cvel/[2*Pi*agamma^2*betagamma],
xisq=powerd*mu0*Cvel*(lambar/Emass)^2, xi=Sqrt(xisq),
eta=omegal*ee/Emass^2, lambda=4*eta; 
!!

SET lgeo=1D-4/4/Pi*nbunch*reprate*an^2/sigx/sigy, wee=2*ee, wlow=0.98*wee, wnbin=100, wbinsize=(wee-wlow)/wnbin;

PRINT lgeo, FORMAT=(' geometric luminosity = ', 1P,E12.4,' cm-2 s-1');
PRINT wee/1d9, FORMAT=(' Wee = ', F10.4,' GeV');
PRINT wlow/1d9, FORMAT=(' Wlow = ', F10.4,' GeV');
PRINT wnbin, FORMAT=(' Wnbin = ', I8);
PRINT wbinsize/1d9, FORMAT=(' Wbinsize = ', F10.4,' GeV');


DEBUGFLAG ID=8;

! SET MsgLevel=1000000,spinleft=0; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
! SET MsgLevel=1000000,spinleft=0.5; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
 SET MsgLevel=1000000,spinleft=0.9; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
! SET MsgLevel=1000000,spinleft=1.0; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
!!
BEAM  RIGHT, KIND=electron, NP=400000, AN=an, E0=ee, SIGE=sige,
  TXYS=(0,0,off/2,0),  GCUTT=ntcut, SLOPE=(crossangle/2,0), CRAB=(crabangle,0),
 BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,-spinleft);
!
BEAM  LEFT,  KIND=electron, NP=400000, AN=an, E0=ee, SIGE=sige,
  TXYS=(0,0,-off/2,0),  GCUTT=ntcut, SLOPE=(crossangle/2,0), CRAB=(crabangle,0),
  BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,spinleft);
!
!! LASER
! TXYS is the laser focal point
! E3 is the unit vector of laser beam, so the vertical crossing, E1 is the unit vector perpendicular to E3
! STOKES; (0,1,0) circular pol. along the direction of the laser
!
LASER LEFT, WAVEL=laserwl, POWERD=powerd,
     TXYS=(-dcp,-dcp*Sin(laserangle),off/2,-dcp*Cos(laserangle)),
     E3=(-Sin(laserangle),0,-Cos(laserangle)), E1=(Cos(laserangle),0,-Sin(laserangle)),
     RAYLEIGH=(rlx,rly), TTOT=2*sigt, GCUTT=ntcut,
     TDL=(d1,d2), STOKES=(0,1,0) ;

 LASER RIGHT, WAVEL=laserwl, POWERD=powerd,
      TXYS=(-dcp,-dcp*Sin(laserangle),-off/2,dcp*Cos(laserangle)),
      E3=(-Sin(laserangle),0,Cos(laserangle)), E1=(Cos(laserangle),0,Sin(laserangle)),
      RAYLEIGH=(rlx,rly), TTOT=2*sigt, GCUTT=ntcut,
      TDL=(d1,d2), STOKES=(0,1,0) ;
!
LASERQED  COMPTON, NPH=3, NY=200, NXI=200, NLAMBDA=200, XIMAX=1.1*xi, LAMBDAMAX=1.1*lambda, PSTOP=1000 ;
LASERQED  BREITW, NPH=3, NY=200, NXI=200, NQ=200, XIMAX=1.1*xi, ETAMAX=1.1*eta, PSTOP=1000 ;
LASERQED  BETHEHEITLER, NPH=0, PSTOP=1000 ;
! SET MsgLevel=0;  FLAG OFF ECHO;
 SET MsgLevel=1000000;  FLAG OFF ECHO;
 SET  it=0;
 PRINT CPUTIME;
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,4*pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,2.2*pushcut*(sigt+sigz)-dcp,500);
!PUSH  Time=(-pushcut*(sigt+sigz)-dcp,2*pushcut*(sigt+sigz)-dcp,500);
PUSH  Time=(-pushcut*(sigt+sigz)-dcp,0.75*pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,20000*dcp*pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,1.8*pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,1.5*pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,pushcut*(sigt+sigz)-dcp,500);
! PUSH  Time=(-pushcut*(sigt+sigz)-dcp,0.5*pushcut*(sigt+sigz)-dcp,500);

IF Mod(it,5)=0;

PRINT it, Time/1e-3, NParticle(3,1), FORMAT=(F6.0,'-th time step   t=',F7.2,' mm',' nPhotons=',E12.4); PRINT STAT;
!PLOT SCAT, LASER, T=Time, COLOR=RED, H=S/micron, V=X/nm,
!HTITLE='S (micron);', VTITLE='X (nm);',
!HSCALE=(-200,200), VSCALE=(-200,200),
!TITLE='t='+$FtoA(Time/1e-3,'(F7.2)')+'mm;',
!SET splotvalue=0;
!PLOT SCAT, LASER, S=splotvalue, COLOR=RED, H=X/nm, V=Y/nm,
!HTITLE='X (nm);', VTITLE='Y (nm);',
!HSCALE=(-200,200), VSCALE=(-200,200),
!TITLE='t='+$FtoA(Time/1e-3,'(F7.2)')+' at s='+$FtoA(splotvalue/1e-3,'(F7.2)')+'mm;',
!PLOT SCAT, LASER, T=Time, COLOR=RED, H=X/nm, V=Y/nm,
!HTITLE='X (nm);', VTITLE='Y (nm);',
!HSCALE=(-200,200), VSCALE=(-200,200),
!TITLE='t='+$FtoA(Time/1e-3,'(F7.2)')+'mm;',
!FILE='ComptonPlots.top', APPEND;
!PLOT SCAT, NONEWPAGE, KIND=2, COLOR='BLUE', MAXNP=0;
!
PLOT SCAT, KIND=2, COLOR='BLUE', MAXNP=10000, H=S/micron, V=X/nm,
HTITLE='S (micron);', VTITLE='X (nm);',
HSCALE=(-2*dcp/micron,2*dcp/micron), VSCALE=(-72*Sqrt(1+dcp^2/betax^2)-crossangle*0.5e5,72*Sqrt(1+dcp^2/betax^2)+crossangle*0.5e5),
TITLE='t='+$FtoA(Time/1e-3,'(F10.5)')+'mm;',
FILE='ComptonPlots.top', APPEND;

   IF NParticle(3,1)>0.5;
      PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=100000;
   ENDIF;

ENDIF;

!      IF Mod(it,50)=0;
!        PRINT it, FORMAT=(F6.0,'-th time step'); PRINT STAT, SHORT;
!      ENDIF;
      SET it=it+1;
 ENDPUSH;
 PRINT CPUTIME;

!  Pull all particles to the IP
!DRIFT S=0;
!  Write particle data onto a file for the job for IP
! WRITE BEAM, KIND=(electron,positron), FILE='higgs.dat';
 WRITE BEAM, KIND=(photon,electron,positron), FILE='higgs.dat';
STORE FILE='higgs.txt';
 PRINT STAT;

PLOT SCAT,MAXNP=10000, KIND=electron,VLOG, HLOG,
        FILE='Plots.top',
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Electron Energy vs. Angle after Compton IP;',
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

PLOT SCAT,MAXNP=10000, RIGHT, KIND=photon, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-5*sigx/nm,5*sigx/nm),
        VSCALE=(-10*sigy/nm,10*sigy/nm),
        TITLE='Right-Going Primary Photon IP Spot;',
        HTITLE='X (nm);',  VTITLE='Y (nm);' ;

PLOT SCAT,MAXNP=10000, KIND=photon, RIGHT, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=En/1D9, V=Xi2,
        HSCALE=(0,emax/1e9), VSCALE=(-1.5,1.5),
        TITLE='Right-Going Primary Photon Energy vs. Polarization after CP;',
        HTITLE='E0G1 (GeV); XGX      ;',
        VTITLE='X021; X X;',;
PLOT SCAT,MAXNP=10000, KIND=photon, LEFT, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=En/1D9, V=Xi2,
        HSCALE=(0,emax/1e9), VSCALE=(-1.5,1.5),
        TITLE='Left-Going Primary Photon Energy vs. Polarization after CP;',
        HTITLE='E0G1 (GeV); XGX      ;',
        VTITLE='X021; X X;',;

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=X/nm, V=Y/nm,
        FILE='Plots.top', APPEND,
HTITLE='X (nm);', VTITLE='Y (nm);',
HSCALE=(-250,250), VSCALE=(-250,250),
  TITLE=' X vs Y;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=En/1D9, V=X/nm,
        FILE='Plots.top', APPEND,
HTITLE='Photon energy (GeV);', VTITLE='X (nm);',
  HSCALE=(0,emax/1e9), VSCALE=(-250,250),
  TITLE=' X vs photon Energy;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=En/1D9, V=X/nm,
        FILE='Plots.top', APPEND,
HTITLE='Photon energy (GeV);', VTITLE='X (nm);',
  HSCALE=(0.5*emax/1e9,emax/1e9), VSCALE=(-40,40),
  TITLE=' X vs photon Energy v2;';


 STOP;
