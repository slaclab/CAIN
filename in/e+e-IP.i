ALLOCATE MP=2000000, MVPH=20000, MLUMMESH=256 ;
SET Rand=5223024;
HEADER  'ILC e+e- Collider  xxxxxxxx;';
SET photon=1, electron=2, positron=3, 
mm=1D-3, micron=1D-6, nm=1D-9, cm=1D-2,
bfieldext=5,
ee=125D9,  gamma=ee/Emass,
  emax=ee,
an=2.0D10,
sigz=300.0*micron,
betax=13.0*mm,
betay=0.41*mm,
emitx=5.0*micron/gamma,
emity=35.0*nm/gamma,
nbunch=1312,
sigee=0.0005,
sigep=0.0005,
reprate=5,
nstepip=200,
eplotmax=1000.,
eplotmin=0.,
plotincp=1,
ifldbe=1,
rzfsgm=-1,
ibealt=1,
smeshval=sigz/3,
wenhbeam=1.0,
wenhpair=1.0,
crossangle=0.000,
crabangle=0.000/2,
sigx=Sqrt(emitx*betax), sigy=Sqrt(emity*betay), 
wscaler=10,
wscalel=10,
  wxvalr=wscaler*sigx,
  wxvall=wscalel*sigx,
  wyvalr=wscaler*sigy,
  wyvall=wscalel*sigy,
   sigxp=sigx/betax,  sigyp=sigy/betay;
 SET MsgLevel=1000000,spinleft=-1; 
SET   cfbeamstr=1;
SET   cfpair=1;
SET   incpair=1;
!SET   incpair=0;
 BEAM  RIGHT, KIND=electron, NP=4000, AN=an, E0=ee, SIGE=sigee,
   TXYS=(0,0,0,0),  SLOPE=(crossangle/2,0), CRAB=(crabangle,0),
   BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,-0.8*spinleft);
!
 BEAM  LEFT,  KIND=positron, NP=4000, AN=an, E0=ee, SIGE=sigep,
   TXYS=(0,0,0,0),  SLOPE=(crossangle/2,0), CRAB=(crabangle,0),
   BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,-0.3*spinleft);
SET Smesh=smeshval;
SET nx=256;
SET ny=256;
SET rval=wxvalr/nx*ny/wyvalr;
SET lgeo=1D-4/4/Pi*nbunch*reprate*an^2/sigx/sigy;
BBFIELD FLDBE=ifldbe, BEALT=ibealt, ZFSGM=rzfsgm, NX=nx, NY=ny, WX=(wxvalr,wxvall), R=rval, WXMAX=(wxvalr,wxvall);
LUMINOSITY  KIND=(electron,positron), W=(0,2.1*ee,50), HELICITY,
        WX=8*sigx, WY=8*sigy, FREP=nbunch*reprate ;
LUMINOSITY  KIND=(photon,photon), W=(0,2.1*ee,50);
LUMINOSITY  KIND=(electron,photon), W=(0,2.1*ee,50);
LUMINOSITY  KIND=(photon,positron), W=(0,2.1*ee,50);


 IF cfbeamstr > 0;
   CFQED BEAMSTRAHLUNG, POL, PMAX=0.8, WENHANCE=wenhbeam;
 ENDIF;

 IF cfpair > 0;
   CFQED PAIR, POL, PMAX=0.8, WENHANCE=wenhpair;
 ENDIF;


 IF incpair > 0;
SET enhpp=1;
PPINT VIRTUALPHOTON, FIELDSUPPRESSION;
!PPINT VIRTUALPHOTON, FIELDSUPPRESSION, EMIN=3.0E06;
PPINT BW, ENHANCE=enhpp;
PPINT BH, ENHANCE=enhpp;
PPINT LL, ENHANCE=enhpp;
!PPINT BREMSSTRAHLUNG, ENHANCE=enhpp;
 ENDIF;


FLAG OFF ECHO;
 SET  it=0;
PUSH Time=(-2.5*sigz,2.5*sigz,nstepip);
IF Mod(it,5)=0;
PRINT it, Time/1e-3, NParticle(3,1), FORMAT=(F6.0,'-th time step   t=',F7.2,' mm',' nPhotons=',E12.4); PRINT STAT;
      SET it=it+1;
ENDIF;

ENDPUSH;

STORE FILE='higgs.txt';


DRIFT T=0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/nm, V=Y/nm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (nm);', VTITLE='Y (nm);', HSCALE=(-500,500), VSCALE=(-500,500), TITLE=' Right-going T=0 X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/nm, V=Y/nm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (nm);', VTITLE='Y (nm);', HSCALE=(-500,500), VSCALE=(-500,500), TITLE=' Left-going T=0 X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT  HIST, KIND=1, H=Ps/Sqrt[Px^2+Py^2+Ps^2], HSCALE=(-1,1,200), VSCALE=(1,1E11), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS.top', APPEND,
  TITLE='Photon Flux ;',
  HTITLE='cos(theta);', VTITLE=' Nphotons / 0.01;';


PLOT  HIST, KIND=(2,3), H=Ps/Sqrt[Px^2+Py^2+Ps^2], HSCALE=(-1,1,200), VSCALE=(1,1E11), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', APPEND,
  TITLE='e+/- Flux ;',
  HTITLE='cos(theta);', VTITLE=' Ne+- / 0.01;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/nm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (nm);', VTITLE='Pt (GeV);', HSCALE=(0,5), VSCALE=(.000001,1), TITLE=' Charged S= 0 Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/nm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (nm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=0 Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/nm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND, 
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=0;',
        HTITLE='R  (nm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';

PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/nm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND, 
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=0;',
        HTITLE='R  (nm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';

  IF incpair > 0;
PLOT  HIST, INCP, H=Ps/Sqrt[Px^2+Py^2+Ps^2], HSCALE=(-1,1,200), VSCALE=(1,1E11), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', APPEND,
  TITLE='incp Flux ;',
  HTITLE='cos(theta);', VTITLE=' Ne+- / 0.01;';


  PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/nm, V=Y/nm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (nm);', VTITLE='Y (nm);', HSCALE=(-500,500), VSCALE=(-500,500), TITLE=' Right-going Incp T=0 X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/nm, V=Y/nm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (nm);', VTITLE='Y (nm);', HSCALE=(-500,500), VSCALE=(-500,500), TITLE=' Left-going Incp T=0 X vs Y;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/nm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND, 
  TITLE='Radius Right-going Incp T=0;',
        HTITLE='R  (nm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/nm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND, 
  TITLE='Radius Left-going Incp T=0;',
        HTITLE='R  (nm);', VTITLE=' Nparticles;';

ENDIF;




EXTERNALFIELD S=(-3.5,3.5),B=(0,0,bfieldext);


DRIFT EXTERNAL, T=0.5*cm;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1600),
        TITLE='T=0.5 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=0.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=0.5 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=0.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=0.5 cm X vs Y;';





DRIFT EXTERNAL, T=1.0*cm;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1600),
        TITLE='T=1.0 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.0 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.0 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=1.0 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=1.0 cm X vs Y;';





DRIFT EXTERNAL, T=1.5*cm;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1600),
        TITLE='T=1.5 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.5 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=1.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=1.5 cm X vs Y;';





DRIFT EXTERNAL, T=2.0*cm;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1600),
        TITLE='T=2.0 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.0 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.0 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=2.0 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=2.0 cm X vs Y;';





DRIFT EXTERNAL, T=2.5*cm;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1600),
        TITLE='T=2.5 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.5 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=2.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=2.5 cm X vs Y;';





DRIFT EXTERNAL, T=3.0*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=3.0 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=3.0 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=3 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=3 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=3.0 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=3.0 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,5), VSCALE=(.000001,130), TITLE=' Charged T=3.0 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=3.0 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';





DRIFT EXTERNAL, T=6.25*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=6.25 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=6.25 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,6), VSCALE=(.000001,130), TITLE=' Right-going Incp T=6.25 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,130), TITLE=' Left-going Incp T=6.25 cm E vs R;';


ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-10,10), VSCALE=(-20,20), TITLE=' T=6.25 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-10,10), VSCALE=(-20,20), TITLE=' T=6.25 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=6.25 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=6.25 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,130), TITLE=' Right-going Charged T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,6), VSCALE=(.000001,130), TITLE=' Right-going Photons T=6.25 cm Energy vs R;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,130), TITLE=' Left-going Charged T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,6), VSCALE=(.000001,130), TITLE=' Left-going Photons T=6.25 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';




DRIFT EXTERNAL, T=10.00*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=10 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=10 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=10 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=10 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-15,15), VSCALE=(-20,20), TITLE=' T=10 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-15,15), VSCALE=(-20,20), TITLE=' T=10 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=10 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=10 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=10.00 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=10.00 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



DRIFT EXTERNAL, T=20*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=20 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=20 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-30,30), VSCALE=(-20,20), TITLE=' T=20 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-30,30), VSCALE=(-20,20), TITLE=' T=20 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=20 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=20 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=20 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=20 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';






DRIFT EXTERNAL, T=40*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=40 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=40 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-60,60), VSCALE=(-20,20), TITLE=' T=40 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-60,60), VSCALE=(-20,20), TITLE=' T=40 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=40 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=40 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=40 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=40 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';





DRIFT EXTERNAL, T=60*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=60 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=60 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-80,80), VSCALE=(-20,20), TITLE=' T=60 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-80,80), VSCALE=(-20,20), TITLE=' T=60 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=60 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=60 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=60 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=60 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';





DRIFT EXTERNAL, T=80*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=80 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=80 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-100,100), VSCALE=(-20,20), TITLE=' T=80 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-100,100), VSCALE=(-20,20), TITLE=' T=80 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=80 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=80 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=80 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=80 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';






DRIFT EXTERNAL, T=250*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=250 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=250 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Right-going Incp T=250 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Left-going Incp T=250 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=250 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=250 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Right-going T=250 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Left-going T=250 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=250 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=250 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';






DRIFT EXTERNAL, T=300*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=300 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=300 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Right-going Incp T=300 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Left-going Incp T=300 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-350,350), VSCALE=(-20,20), TITLE=' T=300 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-350,350), VSCALE=(-20,20), TITLE=' T=300 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=300 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=300 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=300 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=300 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';






DRIFT EXTERNAL, T=350*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=350 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=350 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Right-going Incp T=350 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Left-going Incp T=350 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-400,400), VSCALE=(-20,20), TITLE=' T=350 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && Incp == plotincp),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-400,400), VSCALE=(-20,20), TITLE=' T=350 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=350 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=350 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Charged T=350 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,130), TITLE=' Photons T=350 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';







PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';








DRIFT EXTERNAL, T=0;
!
PRINT STAT;
PRINT STAT, INCP;

  IF incpair > 0;
PLOT  HIST, INCP, H=En/1E9, HSCALE=(0,emax/1E9,250), VLOG,
  FILE='IncPair.top', APPEND,
        TITLE='Final Incp Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';
PLOT  SCAT, INCP,LEFT,  MAXNP=60000,
  FILE='IncPair.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Left-going Incp Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, INCP,RIGHT,  MAXNP=60000,
  FILE='IncPair.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Right-going Incp Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, INCP,RIGHT,VLOG, HLOG,
        FILE='IncPair.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Right-going Incp Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  SCAT, INCP,LEFT,VLOG, HLOG,
        FILE='IncPair.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Left-going Incp Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

 PLOT  SCAT, INCP,VLOG, 
  FILE='IncPair.top', APPEND,
        V=En/1D9, H=Ps/Sqrt[Px^2+Py^2+Ps^2],  MAXNP=400000,
  VSCALE=(0.001,emax/1e9), HSCALE=(-1,1),
  TITLE='Incp Energy vs cos(theta) after IP;',
        VTITLE='E0e1 (GeV); X X      ;',HTITLE='cos(theta);' ;

ENDIF;
  


PLOT  HIST, KIND=photon, H=En/1E9, HSCALE=(0,emax/1E9,50), VLOG,
  FILE='HiggsIP.top', APPEND,
        TITLE='Photon Energy Spectrum;',
        HTITLE='E0G1  (GeV); XGX       ;';
PLOT  HIST, KIND=electron, H=En/1E9, HSCALE=(0,emax/1E9,250), VLOG,
  FILE='HiggsIP.top', APPEND,
        TITLE='Final Electron Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';

SET angmax=200.;

PLOT  HIST, KIND=electron,RIGHT,
  FILE='HiggsIP.top', APPEND,
        H=Py/Ps/mm,
        HSCALE=(-angmax,angmax),
        TITLE='Final Right-going Electron Angle Distribution;',
        HTITLE='Y (mrad);        ;' ;
PLOT  HIST, KIND=electron,LEFT,  
  FILE='HiggsIP.top', APPEND,
        H=Py/Abs(Ps)/mm,
        HSCALE=(-angmax,angmax),
        TITLE='Final Left-going electron Angle Distribution;',
        HTITLE='Y (mrad);        ;' ;

!
PLOT  SCAT, KIND=electron,LEFT,  MAXNP=60000,
  FILE='HiggsIP.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Left-going Electron Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, KIND=electron,RIGHT,  MAXNP=60000,
  FILE='HiggsIP.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Right-going Electron Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, KIND=electron,RIGHT,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Right-going Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  SCAT, KIND=electron,LEFT,VLOG, HLOG,
        FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Left-going Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

 PLOT  SCAT, KIND=electron,VLOG, 
  FILE='ElectronEnergy.top', APPEND,
        V=En/1D9, H=Ps/Sqrt[Px^2+Py^2+Ps^2],  MAXNP=400000,
  VSCALE=(0.001,emax/1e9), HSCALE=(-1,1),
  TITLE='Electron Energy vs cos(theta) after IP;',
        VTITLE='E0e1 (GeV); X X      ;',HTITLE='cos(theta);' ;

 PLOT  SCAT, KIND=photon,VLOG,
  FILE='PhotonEnergy.top', APPEND,
   V=En/1D9, H=Ps/Sqrt[Px^2+Py^2+Ps^2], MAXNP=400000,
   VSCALE=(0.0000001,emax/1e9), HSCALE=(-1,1),
   TITLE='Photon Energy vs cos(theta) after IP;',
        VTITLE='E0G1 (GeV); XGX      ;',HTITLE='cos(theta);' ;

PLOT  SCAT, KIND=photon,VLOG, HLOG,
        FILE='PhotonEnergy.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm, MAXNP=400000,
        VSCALE=(0.0000001,emax/1e9), HSCALE=(0.001,1500),
        TITLE='Photon Energy vs. Angle after IP;',
        VTITLE='E0G1 (GeV); XGX      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;



   PLOT  SCAT, KIND=electron,VLOG, HLOG,
  FILE='HiggsIP.top', APPEND,
          V=Sqrt[(Px^2+Py^2)]/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          HSCALE=(1,1571),
          VSCALE=(0.005,0.2),
          TITLE='I Electron Angle-Pt Distribution;', MAXNP=60000,
          VTITLE='Pt (GeV);',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  HIST, KIND=electron,VLOG,
  FILE='HiggsIP.top', APPEND,
H=Sqrt[(Px^2+Py^2)]/1E9,HSCALE=(0.0,0.2),
          TITLE='Electron Pt Spectrum;',
          HTITLE='P0t1  (GeV); X X       ;';



PLOT LUMINOSITY, KIND=(electron,positron), FILE='e+e-Lumi.top';
PLOT LUMINOSITY, KIND=(photon,photon), FILE='e+e-Lumi.top', APPEND;
PLOT LUMINOSITY, KIND=(electron,photon), FILE='e+e-Lumi.top', APPEND;
PLOT LUMINOSITY, KIND=(photon,positron), FILE='e+e-Lumi.top', APPEND;
! 

FLAG OFF ECHO;
PRINT Lum(2,3),FORMAT=('luminosity e- e+=',1pd15.8);
PRINT Lum(1,1),FORMAT=('luminosity gam gam=',1pd15.8);
PRINT Lum(2,1),FORMAT=('luminosity e- gam=',1pd15.8);
PRINT Lum(1,3),FORMAT=('luminosity gam e+=',1pd15.8);
! 
 STOP;

