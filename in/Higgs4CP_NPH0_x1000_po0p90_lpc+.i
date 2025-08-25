!  ILC  Gamma-Gamma Collider Conversion point
!  Takes 24 sec on DEC workstation at KEK
 HEADER  'XCC G-G Higgs Factory xxxxxxxx;    G G;';
 ALLOCATE MP=360000000, MVPH=1600000, MLUMMESH=256, ;
!SET Rand=7627499;
!SET Rand=9885452;
!SET Rand=4819832;
!SET Rand=6528341;
!SET Rand=9605844;
SET Rand=12303083;
 SET   photon=1, electron=2, positron=3, ntcut=2.5, pushcut=1.07,
   mm=1D-3, micron=1D-6, nm=1D-9, mu0=4*Pi*1D-7, mrad=1D-3,
sige=0.0010,
sigesep=0.005,
ee=62.8584D9,  gamma=ee/Emass,
eesep=10.0D9,  gammasep=eesep/Emass,
sigzsep=3.0*micron,
ansep=2.00D10,
an=0.63D10,
sigz=20.0*micron,
!sigz=10*micron, 
!sigz=30*micron, 
dcp=0.06*mm,  ! dcp=CP-to-IP distance
betax=0.03*mm,
betay=0.03*mm,
alphax=dcp/betax,
alphay=dcp/betay,
emitx=120.0*nm/gamma,
emity=120.0*nm/gamma,
emitxsep=120.0*nm*40./gammasep,
emitysep=120.0*nm/40./gammasep,
nbunch=75, reprate=120,
!
sigxsep=Sqrt(emitxsep*betax), sigysep=Sqrt(emitysep*betay), 
sigx=Sqrt(emitx*betax), sigy=Sqrt(emity*betay), 
sigxp=sigx/betax,  sigyp=sigy/betay,
slopex=0.000, slopey=0.000,
bdipole=0.0,
emax=1.01*ee, wmax=2*emax,
offy=0.00*sigy,
!
bfieldext=5,
ifldbe=0,
rzfsgm=0.05,
ibealt=-1,
wenhbeam=1.0,
wenhpair=1.0,
wenhpp=1.00,
pmaxbeam=0.8,
pmaxpair=0.80,
outputip=0,
smeshval=sigz/3,
Smesh=smeshval,
thfieldext=0.,
xnumber=1000.,
laserwl=1.900745923e-17*ee/xnumber,
lambar=laserwl/(2*Pi), omegal=Hbarc/lambar,
incpair=1,
cfbeamstr=1,
egamma=0,
cfpair=1,
pushexternal=1,
eplotmax=1000.,
eplotmin=0.,
plotincp=-1,
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
!agamma=43.52*nm*Sqrt(1000./xnumber),
!agamma=75.378*nm,
!agamma=33*nm,
nstepip=300,
d1=1,
d2=1,
agammax=21.21*nm,
agammay=21.21*nm,
rlx=4*Pi*agammax^2/laserwl/d1,
rly=4*Pi*agammay^2/laserwl/d2,
sigt=sigz,
pulseE=0.72,
nsigbb=2,
nsigsep=2,
noffxshadow=1.0,
offt=2*nsigbb*(sigzsep+sigz)+2.*0.75*pushcut*(sigt+sigz),
thr=0.014/2,
thl=0.014/2,
sud=dcp-0.5*offt,
gu=-sud*(thr+thl),
sigxdefl=sigx*Sqrt(1.+sud^2/betax^2),
sigxsepdefl=sigxsep*Sqrt(1.+sud^2/betax^2),
sigydefl=sigy*Sqrt(1.+sud^2/betay^2),
sigysepdefl=sigysep*Sqrt(1.+sud^2/betay^2),
capsigx=Sqrt(sigxdefl^2+sigxsepdefl^2),
!capsigx=Sqrt(sigxdefl^2+sigxsep^2),
capsigxdefl=Sqrt(2.*sigxsep^2),
!capsigxdefl=Sqrt(2.*sigxsepdefl^2),
deltar=0.00,
deltal=0.00,
suq=350D-2,
gr=suq*thr-deltar,
gl=suq*thl-deltal,
yy=(gu+gl+gr)/(sud+suq),
alphasep=2.,
   betasep=gl+gr-suq*yy,
xisep=1.+sud/suq,
   psisep=(thl+gr/suq)*sud,
etasep=psisep/xisep,
lambdasep=betasep/alphasep,
gammaone=1.D9/Emass,
kksep=2.*Reclass*ansep/xisep/gammaone,
qqsep=2.*sud*Reclass*ansep/xisep/gammasep/alphasep,
dorigin=-66.0*nm,
theta=dorigin/sud*gamma/gammaone,
bbsep=etasep+lambdasep-kksep/theta,
   ccsep=etasep*lambdasep+qqsep-kksep*lambdasep/theta;
 FLAG OFF ECHO;
PRINT gammaone, FORMAT=(' gammaone = ', d14.6,'  ');
PRINT Reclass, FORMAT=(' Reclass = ', d14.6,' m ');
PRINT an, FORMAT=(' an = ', d14.6,'  ');
PRINT offt, FORMAT=(' offt = ', d14.6,'  ');
PRINT thr, FORMAT=(' thr = ', d14.6,'  ');
PRINT thl, FORMAT=(' thl = ', d14.6,'  ');
PRINT sud, FORMAT=(' sud = ', d14.6,'  ');
PRINT gu, FORMAT=(' gu = ', d14.6,'  ');
PRINT deltar, FORMAT=(' deltar = ', d14.6,'  ');
PRINT deltal, FORMAT=(' deltal = ', d14.6,'  ');
PRINT suq, FORMAT=(' suq = ', d14.6,'  ');
PRINT gr, FORMAT=(' gr = ', d14.6,'  ');
PRINT gl, FORMAT=(' gl = ', d14.6,'  ');
PRINT yy, FORMAT=(' yy = ', d14.6,'  ');
PRINT alphasep, FORMAT=(' alphasep = ', d14.6,'  ');
PRINT betasep, FORMAT=(' betasep = ', d14.6,'  ');
PRINT xisep, FORMAT=(' xisep = ', d14.6,'  ');
PRINT psisep, FORMAT=(' psisep = ', d14.6,'  ');
PRINT etasep, FORMAT=(' etasep = ', d14.6,'  ');
PRINT lambdasep, FORMAT=(' lambdasep = ', d14.6,'  ');
PRINT kksep, FORMAT=(' kksep = ', d14.6,'  ');
PRINT qqsep, FORMAT=(' qqsep = ', d14.6,'  ');
PRINT dorigin, FORMAT=(' dorigin = ', d14.6,'  ');
PRINT theta, FORMAT=(' theta = ', d14.6,'  ');
PRINT bbsep, FORMAT=(' bbsep = ', d14.6,'  ');
PRINT ccsep, FORMAT=(' ccsep = ', d14.6,'  ');
 FLAG ON ECHO;
SET offxr=0.5*(-bbsep+Sqrt(Max(bbsep^2-4.*ccsep,0.))),
offxl=(alphasep-1.)*offxr+betasep,
thdr=(offxr+gr)/suq,
thdl=(gl-offxl)/suq,
  delta0pltest=xisep*offxr+psisep,
  delta0pl=offxr+(thdr+thl)*sud,
  delta0pr=offxl-(thdl+thr)*sud,
  thetadp=2.*Reclass*ansep/gammasep/(offxr+offxl),
  deltapl=delta0pl+sud*thetadp,
  deltapr=delta0pr+sud*thetadp,
  thetatest=kksep/(offxr+etasep+qqsep/(offxr+lambdasep)),
wscaler=10,
wscalel=10,
  wxvalr=wscaler*sigx,
  wxvall=wscalel*sigx,
  wyvalr=wscaler*sigy,
  wyvall=wscalel*sigy,
crabon=1,
laserthloff=0.0,
laserthroff=0.0,
crabthr=crabon*thr,
laserthl=laserthloff,
crabthl=crabon*thl,
laserthr=laserthroff,
crabonsep=0,
crabthrsep=crabonsep*thr,
crabthlsep=-crabonsep*thl,
powerd=pulseE*Cvel/[4*Pi*agammax*agammay*sigt],
xisq=powerd*mu0*Cvel*(lambar/Emass)^2, xi=Sqrt(xisq),
eta=omegal*ee/Emass^2, lambda=4*eta;
!!

SET lgeo=1D-4/4/Pi*nbunch*reprate*an^2/sigx/sigy, wee=2*ee, wlow=0.98*wee, wnbin=100, wbinsize=(wee-wlow)/wnbin;

 FLAG OFF ECHO;
PRINT thetatest, FORMAT=(' thetatest = ', d14.6,' m ');
PRINT offxr, FORMAT=(' offxr = ', d14.6,' m ');
PRINT offxl, FORMAT=(' offxl = ', d14.6,' m ');
PRINT thdr, FORMAT=(' thdr = ', d14.6,'  ');
PRINT thdl, FORMAT=(' thdl = ', d14.6,'  ');
PRINT delta0pltest, FORMAT=(' deltapltest = ', d14.6,'  ');
PRINT delta0pl, FORMAT=(' delta0pl = ', d14.6,'  ');
PRINT delta0pr, FORMAT=(' delta0pr = ', d14.6,'  ');
PRINT thetadp, FORMAT=(' thetadp = ', d14.6,'  ');
PRINT deltapl, FORMAT=(' deltapl = ', d14.6,'  ');
PRINT deltapr, FORMAT=(' deltapr = ', d14.6,'  ');
PRINT lgeo, FORMAT=(' geometric luminosity = ', 1P,E12.4,' cm-2 s-1');
PRINT wee/1d9, FORMAT=(' Wee = ', F10.4,' GeV');
PRINT wlow/1d9, FORMAT=(' Wlow = ', F10.4,' GeV');
PRINT wnbin, FORMAT=(' Wnbin = ', I8);
PRINT wbinsize/1d9, FORMAT=(' Wbinsize = ', F10.4,' GeV');
 FLAG ON ECHO;


DEBUGFLAG ID=8;

! SET MsgLevel=1000000,spinleft=0; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
! SET MsgLevel=1000000,spinleft=-0.5; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
SET MsgLevel=1000000,spinleft=-0.9; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
! SET MsgLevel=1000000,spinleft=-1.0; ! spinleft=+1/-1  for left-handed/right-handed electron helicty
!!
SET ntcutip=2.5;

SET nx=256;
SET ny=256;
SET rval=wxvalr/nx*ny/wyvalr;


SET pushstart=-dcp-pushcut*(sigt+sigz);
SET pushend=0.75*pushcut*(sigt+sigz)-dcp;
!SET pushend=0;

IF dorigin>=0.;


SET pushstart=Min(-dcp-pushcut*(sigt+sigz),-2*dcp+offt-2*nsigsep*sigzsep);
SET pushendcp=nsigbb*(sigzsep+sigz)+0.5*offt-dcp;
SET pushendip=-ntcutip*sigz;
SET pushend=Min(pushendcp,pushendip);

BBFIELD FLDBE=ifldbe, BEALT=ibealt, ZFSGM=rzfsgm, NX=nx, NY=ny, WX=(wxvalr,wxvall), R=rval, WXMAX=(wxvalr,wxvall);
BEAM  RIGHT, KIND=electron, NP=64000, AN=ansep, E0=eesep, SIGE=sigesep,
 TXYS=(-2*dcp+offt,offxr,0,0),  GCUTT=ntcut, SLOPE=(thdr,0), CRAB=(crabthrsep,0),
 BETA=(betax,betay), EMIT=(emitxsep,emitysep), SIGT=sigzsep, SPIN=(0,0,0);
!
IF egamma=0;
BEAM  LEFT, KIND=electron, NP=64000, AN=ansep, E0=eesep, SIGE=sigesep,
 TXYS=(-2*dcp+offt,-offxl,0,0),  GCUTT=ntcut, SLOPE=(thdl,0), CRAB=(crabthlsep,0),
 BETA=(betax,betay), EMIT=(emitxsep,emitysep), SIGT=sigzsep, SPIN=(0,0,0);
ENDIF;

IF noffxshadow>1.5;
BEAM  RIGHT, KIND=electron, NP=64000, AN=ansep, E0=eesep, SIGE=sigesep,
 TXYS=(-2*dcp+offt,-offxr*noffxshadow,0,0),  GCUTT=ntcut, SLOPE=(thdr,0), CRAB=(0,0),
 BETA=(betax,betay), EMIT=(emitxsep,emitysep), SIGT=sigzsep, SPIN=(0,0,0);
!
BEAM  LEFT, KIND=electron, NP=64000, AN=ansep, E0=eesep, SIGE=sigesep,
 TXYS=(-2*dcp+offt,offxl*noffxshadow,0,0),  GCUTT=ntcut, SLOPE=(thdl,0), CRAB=(0,0),
 BETA=(betax,betay), EMIT=(emitxsep,emitysep), SIGT=sigzsep, SPIN=(0,0,0);
ENDIF;
ENDIF;
!
BEAM RIGHT, AN=1.00000*an,
  KIND=electron, NP=64000, AN=an, E0=ee, SIGE=sige,
  TXYS=(0,0,offy/2,0),
! TXYS=(-dcp,-dcp*Sin(thr),0,-dcp*Cos(thr)), ALPHA=(alphax,alphay),
  BETA=(betax,betay),
! BETA=(betax*(1+alphax^2),betay*(1+alphay^2)), 
  GCUTT=ntcut, SLOPE=(thr,0), CRAB=(crabthr,0),
  EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,-spinleft);
!
BEAM LEFT, AN=1.00000*an,
  KIND=electron, NP=64000, AN=1.00000*an, E0=ee, SIGE=sige,
  TXYS=(0,0,-offy/2,0),
!  TXYS=(-dcp,-dcp*Sin(thl),0,dcp*Cos(thl)),ALPHA=(alphax,alphay), 
  BETA=(betax,betay),
!  BETA=(betax*(1+alphax^2),betay*(1+alphay^2)), 
  GCUTT=ntcut, SLOPE=(thl,0), CRAB=(crabthl,0),
  EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,spinleft);
!
!! LASER
! TXYS is the laser focal point
! E3 is the unit vector of laser beam, so the vertical crossing, E1 is the unit vector perpendicular to E3
! STOKES; (0,1,0) circular pol. along the direction of the laser
!
IF egamma=0;
LASER LEFT, POWERD=1.0000*powerd,
     WAVEL=laserwl,
     TXYS=(-dcp,-dcp*Sin(thr),0,-dcp*Cos(thr)),
     E3=(-Sin(laserthl),0,-Cos(laserthl)), E1=(Cos(laserthl),0,-Sin(laserthl)),
     RAYLEIGH=(rlx,rly), TTOT=2*sigt, GCUTT=ntcut,
     TDL=(d1,d2), STOKES=(0,1,0) ;
ENDIF;

LASER RIGHT, POWERD=powerd,
     WAVEL=laserwl,
     TXYS=(-dcp,-dcp*Sin(thl),0,dcp*Cos(thl)),
     E3=(-Sin(laserthr),0,Cos(laserthr)), E1=(Cos(laserthr),0,Sin(laserthr)),
     RAYLEIGH=(rlx,rly), TTOT=2*sigt, GCUTT=ntcut,
     TDL=(d1,d2), STOKES=(0,1,0) ;
!
LASERQED  COMPTON, NPH=0, NY=200, NXI=200, NLAMBDA=200, XIMAX=1.1*xi, LAMBDAMAX=1.1*lambda, PSTOP=1000 ;
!LASERQED  BREITW, NPH=0, NY=200, NXI=200, NQ=200, XIMAX=1.1*xi, ETAMAX=1.1*eta, PSTOP=1000 ;
!LASERQED  BETHEHEITLER, NPH=0, PSTOP=1000 ;
! SET MsgLevel=0;  FLAG OFF ECHO;
 SET MsgLevel=1000000;  FLAG OFF ECHO;
 SET  it=0;
 PRINT CPUTIME;
IF pushexternal > 0;
EXTERNALFIELD B=(bfieldext*Sin(thfieldext),0,bfieldext*Cos(thfieldext));
ENDIF;
SET nstepcp=1000;
!PUSH  Time=(-pushcut*(sigt+sigz)-dcp,0.75*pushcut*(sigt+sigz)-dcp,500);
PUSH  Time=(pushstart,pushend,nstepcp);

IF Mod(it,nstepcp/50)=0;

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

SET vscalexmin=Min(-72*Sqrt(1+dcp^2/betax^2)-Max(thr,thl)*1e5,-Sgn(dorigin)*3.0*(Max(Abs(offxr),Abs(offxl))+capsigxdefl)/nm);
SET vscalexmax=Max(72*Sqrt(1+dcp^2/betax^2)+Max(thr,thl)*1e5,Sgn(dorigin)*3.0*(Max(Abs(offxr),Abs(offxl))+capsigxdefl)/nm);
!SET vscalexmax=72*Sqrt(1+dcp^2/betax^2);
PLOT SCAT, KIND=2, COLOR='BLUE', MAXNP=10000, H=S/micron, V=X/nm,
HTITLE='S (micron);', VTITLE='X (nm);',
  HSCALE=(-2*dcp/micron,2*dcp/micron), VSCALE=(vscalexmin,vscalexmax),
TITLE='t='+$FtoA(Time/1e-3,'(F10.5)')+'mm;',
FILE='ComptonPlots.top', APPEND;
   IF NParticle(3,1)>0.5;
      PLOT SCAT, NONEWPAGE, KIND=photon, COLOR='RED', MAXNP=100000;
   ENDIF;


SET vscaleymin=-72*Sqrt(1+dcp^2/betay^2);
SET vscaleymax=72*Sqrt(1+dcp^2/betay^2);
PLOT SCAT, KIND=2, COLOR='BLUE', MAXNP=10000, H=S/micron, V=Y/nm,
HTITLE='S (micron);', VTITLE='Y (nm);',
  HSCALE=(-2*dcp/micron,2*dcp/micron), VSCALE=(vscaleymin,vscaleymax),
TITLE='t='+$FtoA(Time/1e-3,'(F10.5)')+'mm;',
FILE='ComptonPlots2.top', APPEND;
   IF NParticle(3,1)>0.5;
      PLOT SCAT, NONEWPAGE, KIND=photon, COLOR='RED', MAXNP=100000;
   ENDIF;



!SET vscalexmin3=-50;
!SET vscalexmax3=50;
SET vscalexmin3=-2.5*wxvalr/micron;
SET vscalexmax3=2.5*wxvalr/micron;
PLOT SCAT, KIND=2, COLOR='BLUE', MAXNP=10000, H=S/micron, V=X/micron,
HTITLE='S (micron);', VTITLE='X (micron);',
  HSCALE=(-2*dcp/micron,2*dcp/micron), VSCALE=(vscalexmin3,vscalexmax3),
TITLE='t='+$FtoA(Time/1e-3,'(F10.5)')+'mm;',
FILE='ComptonPlots3.top', APPEND;



ENDIF;

!      IF Mod(it,50)=0;
!        PRINT it, FORMAT=(F6.0,'-th time step'); PRINT STAT, SHORT;
!      ENDIF;
IF it=0;
WRITE BEAM, KIND=(photon,electron,positron), FILE='it0higgs.dat';
ENDIF;
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
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
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

   IF NParticle(3,1)>0.5;


 PLOT  HIST, KIND=photon, RIGHT, H=En/1D9, HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9,500),
        FILE='Plots.top', APPEND,
        TITLE='All Right-Going Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;
 PLOT  HIST, KIND=photon, LEFT, H=En/1D9, HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9,500),
        FILE='Plots.top', APPEND,
        TITLE='All Left-Going Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;
 PLOT  HIST, KIND=photon, RIGHT, SELECT=(Gen==2), H=En/1D9, HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9,500),
        FILE='Plots.top', APPEND,
       TITLE='Right-Going Primary Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;
 PLOT  HIST, LEFT,KIND=photon, SELECT=(Gen==2),H=En/1D9, HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9,500),
        FILE='Plots.top', APPEND,
       TITLE='Left-Going Primary Photon Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;

PLOT SCAT,MAXNP=10000, LEFT, KIND=photon, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-20*sigx/nm,20*sigx/nm),
        VSCALE=(-20*sigy/nm,20*sigy/nm),
        TITLE='Left-Going Primary Photon IP Spot;',
        HTITLE='X (nm);',  VTITLE='Y (nm);' ;

PLOT SCAT,MAXNP=10000, KIND=photon, RIGHT, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=En/1D9, V=Xi2,
        HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9), VSCALE=(-1.5,1.5),
        TITLE='Right-Going Primary Photon Energy vs. Polarization after CP;',
        HTITLE='E0G1 (GeV); XGX      ;',
        VTITLE='X021; X X;',;
PLOT SCAT,MAXNP=10000, KIND=photon, LEFT, SELECT=(Gen==2),
        FILE='Plots.top', APPEND,
        H=En/1D9, V=Xi2,
        HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9), VSCALE=(-1.5,1.5),
        TITLE='Left-Going Primary Photon Energy vs. Polarization after CP;',
        HTITLE='E0G1 (GeV); XGX      ;',
        VTITLE='X021; X X;',;

PLOT SCAT, KIND=photon, COLOR='RED', MAXNP=10000, H=X/nm, V=Y/nm,
        FILE='Plots.top', APPEND,
HTITLE='X (nm);', VTITLE='Y (nm);',
HSCALE=(-20*sigx/nm,20*sigx/nm), VSCALE=(-20*sigx/nm,20*sigx/nm),
  TITLE=' photon X vs Y;';

PLOT SCAT, KIND=photon, COLOR='RED', MAXNP=10000, H=En/1D9, V=X/nm,
        FILE='Plots.top', APPEND,
HTITLE='Photon energy (GeV);', VTITLE='X (nm);',
  HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9), VSCALE=(-20*sigx/nm,20*sigx/nm),
  TITLE=' X vs photon Energy;';

PLOT SCAT, KIND=photon, COLOR='RED', MAXNP=10000, H=En/1D9, V=X/nm,
        FILE='Plots.top', APPEND,
HTITLE='Photon energy (GeV);', VTITLE='X (nm);',
  HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9), VSCALE=(-5*sigx/nm,5*sigx/nm),
  TITLE=' X vs photon Energy v2;';

 ENDIF;

   IF NParticle(3,3)>0.5;


 PLOT  HIST, KIND=positron, RIGHT, SELECT=(Gen==3), H=En/1D9, HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9,500),
        FILE='Plots.top', APPEND,
       TITLE='Right-Going Primary Positron Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;
 PLOT  HIST, LEFT,KIND=positron, SELECT=(Gen==3),H=En/1D9, HSCALE=(0,1.01*xnumber*ee/(xnumber+1)/1e9,500),
        FILE='Plots.top', APPEND,
       TITLE='Left-Going Primary Positron Energy Spectrum after CP;',
        HTITLE='E0G1 (GeV); XGX      ;'  ;

 ENDIF;

 STOP;
