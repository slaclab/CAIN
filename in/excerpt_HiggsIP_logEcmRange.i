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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=0.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=0.5 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=0.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax),
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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.0 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.0 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=1.0 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax),
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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=1.5 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=1.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax),
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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.0 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.0 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=2.0 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax),
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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=2.5 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=2.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax),
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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' T=3 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,5), VSCALE=(.000001,100), TITLE=' Charged T=3.0 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=3.0 cm Energy vs R;';


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
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Right-going Incp T=6.25 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Left-going Incp T=6.25 cm E vs R;';


ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-10,10), VSCALE=(-20,20), TITLE=' T=6.25 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Right-going Charged T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Right-going Photons T=6.25 cm Energy vs R;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Left-going Charged T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Left-going Photons T=6.25 cm Energy vs R;';


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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-15,15), VSCALE=(-20,20), TITLE=' T=10 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-30,30), VSCALE=(-20,20), TITLE=' T=20 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=20 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=20 cm Energy vs R;';


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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-60,60), VSCALE=(-20,20), TITLE=' T=40 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=40 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=40 cm Energy vs R;';


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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-80,80), VSCALE=(-20,20), TITLE=' T=60 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=60 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=60 cm Energy vs R;';


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
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-100,100), VSCALE=(-20,20), TITLE=' T=80 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=80 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=80 cm Energy vs R;';


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
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=250 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=250 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=250 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=250 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=250 cm Energy vs R;';


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
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=300 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=300 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-350,350), VSCALE=(-20,20), TITLE=' T=300 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=300 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=300 cm Energy vs R;';


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
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=350 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=350 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-400,400), VSCALE=(-20,20), TITLE=' T=350 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots.top', APPEND, 
SELECT=(En/1D9 < eplotmax),
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
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=350 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=350 cm Energy vs R;';


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






