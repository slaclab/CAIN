!  ILC  Gamma-Gamma Collider   Interaction point
!
!  Read variables written by HiggsnCP.i (corresponds to the
!  command STORE in HiggsnCP.i)
ALLOCATE MP=360000000, MVPH=1000000, MLUMMESH=256, ;
PRINT FORMAT=(' e+e-IP point 000');
RESTORE FILE='higgsIP.txt';
HEADER  'ILC e+e-  Higgs Factory xxxxxxxx;    G G;';
SET MsgLevel=100000;
SET cm=1D-2;
!!
 BEAM  FILE='outputip.dat';
!!

! plots
 FLAG ON ECHO;
EXTERNALFIELD B=(Sin(thfieldext),0,bfieldext*Cos(thfieldext));

DRIFT EXTERNAL, T=0.005*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=0.005 cm R vs S;';
  ENDIF;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=0.005 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-0.005*5.0,0.005*5.0), VSCALE=(-0.015,0.015), TITLE=' T=0.005 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-0.005*5.0,0.005*5.0), VSCALE=(-0.015,0.015), TITLE=' T=0.005 cm Y vs S;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-0.015,0.015), VSCALE=(-0.015,0.015), TITLE=' Right-going T=0.005 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-0.015,0.015), VSCALE=(-0.015,0.015), TITLE=' Left-going T=0.005 cm X vs Y;';




DRIFT EXTERNAL, T=0.01*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=0.01 cm R vs S;';
  ENDIF;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=0.01 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-0.01*3.0,0.01*3.0), VSCALE=(-0.02,0.02), TITLE=' T=0.01 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-0.01*3.0,0.01*3.0), VSCALE=(-0.02,0.02), TITLE=' T=0.01 cm Y vs S;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-0.02,0.02), VSCALE=(-0.02,0.02), TITLE=' Right-going T=0.01 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-0.02,0.02), VSCALE=(-0.02,0.02), TITLE=' Left-going T=0.01 cm X vs Y;';



DRIFT EXTERNAL, T=0.5*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=0.5 cm R vs S;';
  ENDIF;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=0.5 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-0.5*1.4,0.5*1.4), VSCALE=(-0.7,0.7), TITLE=' T=0.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-0.5*1.4,0.5*1.4), VSCALE=(-0.7,0.7), TITLE=' T=0.5 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-0.5*1.4,0.5*1.4), VSCALE=(-0.7,0.7), TITLE=' Right-going T=0.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-0.5*1.4,0.5*1.4), VSCALE=(-0.7,0.7), TITLE=' Left-going T=0.5 cm X vs Y;';





DRIFT EXTERNAL, T=1.0*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=1.0 cm R vs S;';
  ENDIF;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=1.0 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-1.0*1.4,1.0*1.4), VSCALE=(-2.,2.), TITLE=' T=1.0 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-1.0*1.4,1.0*1.4), VSCALE=(-2.,2.), TITLE=' T=1.0 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-1.0*1.4,1.0*1.4), VSCALE=(-2.,2.), TITLE=' Right-going T=1.0 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-1.0*1.4,1.0*1.4), VSCALE=(-2.,2.), TITLE=' Left-going T=1.0 cm X vs Y;';





DRIFT EXTERNAL, T=1.5*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=1.5 cm R vs S;';
  ENDIF;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=1.5 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-1.5*1.4,1.5*1.4), VSCALE=(-2.5,2.5), TITLE=' T=1.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-1.5*1.4,1.5*1.4), VSCALE=(-2.5,2.5), TITLE=' T=1.5 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-1.5*1.4,1.5*1.4), VSCALE=(-2.5,2.5), TITLE=' Right-going T=1.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-1.5*1.4,1.5*1.4), VSCALE=(-2.5,2.5), TITLE=' Left-going T=1.5 cm X vs Y;';





DRIFT EXTERNAL, T=2.0*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=2.0 cm R vs S;';
  ENDIF;


PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=2.0 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-2.0*1.4,2.0*1.4), VSCALE=(-2.5,2.5), TITLE=' T=2.0 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-2.0*1.4,2.0*1.4), VSCALE=(-2.5,2.5), TITLE=' T=2.0 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-2.0*1.4,2.0*1.4), VSCALE=(-2.5,2.5), TITLE=' Right-going T=2.0 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-2.0*1.4,2.0*1.4), VSCALE=(-2.5,2.5), TITLE=' Left-going T=2.0 cm X vs Y;';





DRIFT EXTERNAL, T=2.5*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=2.5 cm R vs S;';
  ENDIF;



PLOT  SCAT, KIND=electron,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=10000,
        VSCALE=(0.001,emax/1e9), HSCALE=(0.01,1600),
        TITLE='T=2.5 cm Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-2.5*1.4,2.5*1.4), VSCALE=(-3.0,3.0), TITLE=' T=2.5 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-2.5*1.4,2.5*1.4), VSCALE=(-3.0,3.0), TITLE=' T=2.5 cm Y vs S;';




PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-2.5*1.4,2.5*1.4), VSCALE=(-3.0,3.0), TITLE=' Right-going T=2.5 cm X vs Y;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-2.5*1.4,2.5*1.4), VSCALE=(-3.0,3.0), TITLE=' Left-going T=2.5 cm X vs Y;';





DRIFT EXTERNAL, T=3.0*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=3.0 cm R vs S;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-3.0*1.4,3.0*1.4), VSCALE=(-3.5,3.5), TITLE=' Right-going Incp T=3.0 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-3.0*1.4,3.0*1.4), VSCALE=(-3.5,3.5), TITLE=' Left-going Incp T=3.0 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-3.0*1.4,3.0*1.4), VSCALE=(-3.5,3.5), TITLE=' T=3 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-3.0*1.4,3.0*1.4), VSCALE=(-3.5,3.5), TITLE=' T=3 cm Y vs S;';



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-3.0*1.4,3.0*1.4), VSCALE=(-3.5,3.5), TITLE=' Right-going T=3.0 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-3.0*1.4,3.0*1.4), VSCALE=(-3.5,3.5), TITLE=' Left-going T=3.0 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,5), VSCALE=(.000001,100), TITLE=' Charged T=3.0 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=3.0 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';





DRIFT EXTERNAL, T=6.25*cm;
  IF incpair > 0;
PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Sqrt[X^2+Y^2]/cm,
  FILE='IncPairRvsS_post.top', APPEND, 
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='R (cm);', HSCALE=(-30,30), VSCALE=(-2.9,2.9), TITLE=' T=6.25 cm R vs S;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-6.25*1.4,6.25*1.4), VSCALE=(-4.0,4.0), TITLE=' Right-going Incp T=6.25 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-6.25*1.4,6.25*1.4), VSCALE=(-4.0,4.0), TITLE=' Left-going Incp T=6.25 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Right-going Incp T=6.25 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Left-going Incp T=6.25 cm E vs R;';


ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-6.25*1.4,6.25*1.4), VSCALE=(-4.0,4.0), TITLE=' T=6.25 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-6.25*1.4,6.25*1.4), VSCALE=(-4.0,4.0), TITLE=' T=6.25 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-6.25*1.4,6.25*1.4), VSCALE=(-4.0,4.0), TITLE=' Right-going T=6.25 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-6.25*1.4,6.25*1.4), VSCALE=(-4.0,4.0), TITLE=' Left-going T=6.25 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Right-going Charged T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Right-going Photons T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Left-going Charged T=6.25 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,6), VSCALE=(.000001,100), TITLE=' Left-going Photons T=6.25 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=6.25 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';




DRIFT EXTERNAL, T=10.00*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-10*1.4,10*1.4), VSCALE=(-20,20), TITLE=' Right-going Incp T=10 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-10*1.4,10*1.4), VSCALE=(-20,20), TITLE=' Left-going Incp T=10 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=10 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=10 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-15,15), VSCALE=(-20,20), TITLE=' T=10 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-15,15), VSCALE=(-20,20), TITLE=' T=10 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-10*1.4,10*1.4), VSCALE=(-20,20), TITLE=' Right-going T=10 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-10*1.4,10*1.4), VSCALE=(-20,20), TITLE=' Left-going T=10 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=10.00 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=10.00 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



DRIFT EXTERNAL, T=20*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20*1.4,20*1.4), VSCALE=(-20,20), TITLE=' Right-going Incp T=20 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20*1.4,20*1.4), VSCALE=(-20,20), TITLE=' Left-going Incp T=20 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-30,30), VSCALE=(-20,20), TITLE=' T=20 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-30,30), VSCALE=(-20,20), TITLE=' T=20 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20*1.4,20*1.4), VSCALE=(-20,20), TITLE=' Right-going T=20 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20*1.4,20*1.4), VSCALE=(-20,20), TITLE=' Left-going T=20 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=20 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=20 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=20 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';






DRIFT EXTERNAL, T=40*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=40 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=40 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-60,60), VSCALE=(-20,20), TITLE=' T=40 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-60,60), VSCALE=(-20,20), TITLE=' T=40 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=40 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=40 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=40 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=40 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=40 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';





DRIFT EXTERNAL, T=60*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=60 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=60 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-80,80), VSCALE=(-20,20), TITLE=' T=60 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-80,80), VSCALE=(-20,20), TITLE=' T=60 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=60 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=60 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=60 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=60 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=60 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';





DRIFT EXTERNAL, T=80*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=80 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=80 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-100,100), VSCALE=(-20,20), TITLE=' T=80 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-100,100), VSCALE=(-20,20), TITLE=' T=80 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=80 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=80 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=80 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=80 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=80 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';







DRIFT EXTERNAL, T=100*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=100 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=100 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=100 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=100 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=100 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=100 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=100 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=100 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Right-going T=100 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Left-going T=100 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=100 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=100 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=100 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=100 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=1, H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Photon (Red)  Right-going T=100 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=100 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=100 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=100 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=100 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=100 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=100 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=100 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=100 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';







DRIFT EXTERNAL, T=150*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=150 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=150 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=150 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=150 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=150 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=150 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=150 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=150 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Right-going T=150 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Left-going T=150 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=150 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=150 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=150 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=150 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';




PLOT  HIST, KIND=1, H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Photon (Red)  Right-going T=150 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';



PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=150 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=150 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=1, H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Photon (Red)  Left-going T=150 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=150 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=150 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=150 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=150 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=150 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=150 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';










DRIFT EXTERNAL, T=250*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=250 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=250 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=250 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=250 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=250 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' T=250 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Right-going T=250 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-300,300), VSCALE=(-20,20), TITLE=' Left-going T=250 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=250 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=250 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=250 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';


PLOT  HIST, KIND=1, H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Photon (Red)  Right-going T=250 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';





PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=250 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=250 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=250 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=250 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=250 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=250 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=250 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=250 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';







DRIFT EXTERNAL, T=300*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=300 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=300 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=300 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=300 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-350,350), VSCALE=(-20,20), TITLE=' T=300 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-350,350), VSCALE=(-20,20), TITLE=' T=300 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=300 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=300 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=300 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=300 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=300 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';


PLOT  HIST, KIND=1, H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Photon (Red)  Right-going T=300 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';





PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=300 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=300 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=300 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=300 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=300 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=300 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=300 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=300 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';







DRIFT EXTERNAL, T=350*cm;
  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going Incp T=350 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='IncPair_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going Incp T=350 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair_post.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', RIGHT, APPEND,
 HTITLE='R (cm);', VTITLE='E(GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Right-going Incp T=350 cm E vs R;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', LEFT, APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Left-going Incp T=350 cm E vs R;';




ENDIF;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-400,400), VSCALE=(-20,20), TITLE=' T=350 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-400,400), VSCALE=(-20,20), TITLE=' T=350 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Right-going T=350 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-20,20), VSCALE=(-20,20), TITLE=' Left-going T=350 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsChgPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='E (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Charged T=350 cm Energy vs R;';

PLOT SCAT, KIND=1, COLOR='RED', MAXNP=10000, H=Sqrt[X^2+Y^2]/cm, V=En/1E9, VLOG,
  FILE='BgndPlotsPhPtvsR_post.top', APPEND,
 HTITLE='R (cm);', VTITLE='En (GeV);', HSCALE=(0,20), VSCALE=(.000001,100), TITLE=' Photons T=350 cm Energy vs R;';


PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Right-going T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';



PLOT  HIST, KIND=(2,3), H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  TITLE='Radius Charged (Blue)  and  Photons (Red) Left-going T=350 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
PLOT HIST, NONEWPAGE, KIND=1, COLOR='RED';




PLOT  HIST, KIND=1, H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='RED', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Photon (Red)  Right-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';





PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=350 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=350 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


EXTERNALFIELD B=(Sin(thfieldext),bdipole,bfieldext*Cos(thfieldext));
DRIFT EXTERNAL, RIGHT, T=450*cm;

EXTERNALFIELD B=(Sin(thfieldext),-bdipole,bfieldext*Cos(thfieldext));
DRIFT EXTERNAL, LEFT, T=450*cm;



PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=X/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='X (cm);', HSCALE=(-500,500), VSCALE=(-40,40), TITLE=' T=450 cm X vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=S/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', APPEND, 
SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  HTITLE='S (cm);', VTITLE='Y (cm);', HSCALE=(-500,500), VSCALE=(-40,40), TITLE=' T=450 cm Y vs S;';


PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-40,40), VSCALE=(-40,40), TITLE=' Right-going T=450 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;

PLOT SCAT, KIND=(2,3), COLOR='BLUE', MAXNP=10000, H=X/cm, V=Y/cm,
  FILE='BackgroundPlots_post.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-40,40), VSCALE=(-40,40), TITLE=' Left-going T=450 cm X vs Y;';
PLOT SCAT, NONEWPAGE, KIND=1, COLOR='RED', MAXNP=10000;





PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=450 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=450 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';


PLOT  HIST, KIND=(2,3), H=X/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=450 cm;',
  HTITLE='X  (cm);', VTITLE=' Energy (GeV);';

PLOT  HIST, KIND=(2,3), H=Y/cm, V=(En/1D9), HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=450 cm;',
  HTITLE='Y  (cm);', VTITLE=' Energy (GeV);';




PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Right-going T=450 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', RIGHT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Right-going T=450 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';


PLOT  HIST, KIND=(2,3), H=X/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='X Charged (Blue)  Left-going T=450 cm;',
  HTITLE='X  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, KIND=(2,3), H=Y/cm, HSCALE=(-20,20,200), VLOG, COLOR='BLUE', 
  FILE='BackgroundPlotsRvsS_post.top', LEFT, APPEND,
  SELECT=(En/1D9 < eplotmax && En/1D9 > eplotmin && (plotincp < 0 || Incp == plotincp)),
  TITLE='Y Charged (Blue)  Left-going T=450 cm;',
  HTITLE='Y  (cm);', VTITLE=' Nparticles;';









DRIFT EXTERNAL, T=0;
!
PRINT STAT;
PRINT STAT, INCP;

  IF incpair > 0;
PLOT  HIST, INCP, H=En/1E9, HSCALE=(0,emax/1E9,250), VLOG,
  FILE='IncPair_post.top', APPEND,
        TITLE='Final Incp Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';
PLOT  SCAT, INCP,LEFT,  MAXNP=60000,
  FILE='IncPair_post.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Left-going Incp Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, INCP,RIGHT,  MAXNP=60000,
  FILE='IncPair_post.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Right-going Incp Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, INCP,RIGHT,VLOG, HLOG,
        FILE='IncPair_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Right-going Incp Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  SCAT, INCP,LEFT,VLOG, HLOG,
        FILE='IncPair_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Left-going Incp Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

 PLOT  SCAT, INCP,VLOG, 
  FILE='IncPair_post.top', APPEND,
        V=En/1D9, H=Ps/Sqrt[Px^2+Py^2+Ps^2],  MAXNP=400000,
  VSCALE=(0.001,emax/1e9), HSCALE=(-1,1),
  TITLE='Incp Energy vs cos(theta) after IP;',
        VTITLE='E0e1 (GeV); X X      ;',HTITLE='cos(theta);' ;

ENDIF;
  


PLOT  HIST, KIND=photon, H=En/1E9, HSCALE=(0,emax/1E9,50), VLOG,
  FILE='HiggsIP_post.top', APPEND,
        TITLE='Photon Energy Spectrum;',
        HTITLE='E0G1  (GeV); XGX       ;';
PLOT  HIST, KIND=electron, H=En/1E9, HSCALE=(0,emax/1E9,250), VLOG,
  FILE='HiggsIP_post.top', APPEND,
        TITLE='Final Electron Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';

SET angmax=200.;

PLOT  HIST, KIND=electron,RIGHT,
  FILE='HiggsIP_post.top', APPEND,
        H=Py/Ps/mm,
        HSCALE=(-angmax,angmax),
        TITLE='Final Right-going Electron Angle Distribution;',
        HTITLE='Y (mrad);        ;' ;
PLOT  HIST, KIND=electron,LEFT,  
  FILE='HiggsIP_post.top', APPEND,
        H=Py/Abs(Ps)/mm,
        HSCALE=(-angmax,angmax),
        TITLE='Final Left-going electron Angle Distribution;',
        HTITLE='Y (mrad);        ;' ;

!
PLOT  SCAT, KIND=electron,LEFT,  MAXNP=60000,
  FILE='HiggsIP_post.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Left-going Electron Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, KIND=electron,RIGHT,  MAXNP=60000,
  FILE='HiggsIP_post.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Right-going Electron Y vs X at T=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, KIND=electron,RIGHT,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Right-going Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  SCAT, KIND=electron,LEFT,VLOG, HLOG,
        FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm,  MAXNP=400000,
        VSCALE=(0.001,emax/1e9), HSCALE=(1,1500),
        TITLE='Left-going Electron Energy vs. Angle after IP;',
        VTITLE='E0e1 (GeV); X X      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

 PLOT  SCAT, KIND=electron,VLOG, 
  FILE='ElectronEnergy_post.top', APPEND,
        V=En/1D9, H=Ps/Sqrt[Px^2+Py^2+Ps^2],  MAXNP=400000,
  VSCALE=(0.001,emax/1e9), HSCALE=(-1,1),
  TITLE='Electron Energy vs cos(theta) after IP;',
        VTITLE='E0e1 (GeV); X X      ;',HTITLE='cos(theta);' ;

 PLOT  HIST, KIND=1, H=Ps/Sqrt[Px^2+Py^2+Ps^2], HSCALE=(-1,1,200), VSCALE=(1,1E11), VLOG, COLOR='RED', 
  FILE='PhotonEnergy_post.top', APPEND,
  TITLE='Photon Flux ;',
  HTITLE='cos(theta);', VTITLE=' Nphotons / 0.01;';


 PLOT  SCAT, KIND=photon,VLOG,
  FILE='PhotonEnergy_post.top', APPEND,
   V=En/1D9, H=Ps/Sqrt[Px^2+Py^2+Ps^2], MAXNP=400000,
   VSCALE=(0.0000001,emax/1e9), HSCALE=(-1,1),
   TITLE='Photon Energy vs cos(theta) after IP;',
        VTITLE='E0G1 (GeV); XGX      ;',HTITLE='cos(theta);' ;

PLOT  SCAT, KIND=photon,VLOG, HLOG,
        FILE='PhotonEnergy_post.top', APPEND,
        V=En/1D9, H=ArcTan(Sqrt[(Px^2+Py^2)/Ps^2])/mm, MAXNP=400000,
        VSCALE=(0.0000001,emax/1e9), HSCALE=(0.001,1500),
        TITLE='Photon Energy vs. Angle after IP;',
        VTITLE='E0G1 (GeV); XGX      ;',
        HTITLE='Q0e1 (mrad);GX X       ;' ;



   PLOT  SCAT, KIND=electron,VLOG, HLOG,
  FILE='HiggsIP_post.top', APPEND,
          V=Sqrt[(Px^2+Py^2)]/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          HSCALE=(1,1571),
          VSCALE=(0.005,0.2),
          TITLE='I Electron Angle-Pt Distribution;', MAXNP=60000,
          VTITLE='Pt (GeV);',
        HTITLE='Q0e1 (mrad);GX X       ;' ;

PLOT  HIST, KIND=electron,VLOG,
  FILE='HiggsIP_post.top', APPEND,
H=Sqrt[(Px^2+Py^2)]/1E9,HSCALE=(0.0,0.2),
          TITLE='Electron Pt Spectrum;',
          HTITLE='P0t1  (GeV); X X       ;';

!PLOT  HIST, KIND=electron,VLOG,
!  FILE='HiggsIP_post.top', APPEND,
!H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,HSCALE=(0.,angmax/2),
!           SELECT=(Sqrt[(Px^2+Py^2)] >10e6),
!          TITLE='Electron Angle Spectrum for Pt>10MeV;',
!        HTITLE='Q0e1 (mrad);GX X       ;' ;
!!
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(photon,photon), FILE='HiggsLumi_post.top';
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(photon,electron), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(electron,photon), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(electron,electron), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(photon,positron), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(positron,photon), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(electron,positron), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(positron,electron), FILE='HiggsLumi_post.top', APPEND;
PLOT LUMINOSITY, VLOG, PERBIN, KIND=(positron,positron), FILE='HiggsLumi_post.top', APPEND;

PRINT LUMINOSITY, KIND=(photon,photon), FILE='photonphotonlumi.txt';


! 
!!
FLAG OFF ECHO;
PRINT Lum(1,1),FORMAT=('luminosity gam gam=',1pd15.8);
PRINT Lum(1,2),FORMAT=('luminosity gam e-=',1pd15.8);
PRINT Lum(2,1),FORMAT=('luminosity e- gam=',1pd15.8);
PRINT Lum(2,2),FORMAT=('luminosity e- e-=',1pd15.8);
PRINT Lum(1,3),FORMAT=('luminosity gam e+=',1pd15.8);
PRINT Lum(3,1),FORMAT=('luminosity e+ gam=',1pd15.8);
PRINT Lum(2,3),FORMAT=('luminosity e- e+=',1pd15.8);
PRINT Lum(3,2),FORMAT=('luminosity e+ e-=',1pd15.8);
PRINT Lum(3,3),FORMAT=('luminosity e+ e+=',1pd15.8);
! 
 STOP;
