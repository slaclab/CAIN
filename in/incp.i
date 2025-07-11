!!!!s=0:
  IF incpair > 0;
PLOT  HIST, INCP, H=Ps/Sqrt[Px^2+Py^2+Ps^2], HSCALE=(-1,1,200), VSCALE=(1,1E11), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', APPEND,
  TITLE='incp Flux ;',
  HTITLE='cos(theta);', VTITLE=' Ne+- / 0.01;';


  PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-4,4), VSCALE=(-4,4), TITLE=' Right-going Incp S=0 X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-4,4), VSCALE=(-4,4), TITLE=' Left-going Incp S=0 X vs Y;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND, 
  TITLE='Radius Right-going Incp S=0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND, 
  TITLE='Radius Left-going Incp S=0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

ENDIF;
!!!!S=3 cm

  IF incpair > 0;
PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', RIGHT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-4,4), VSCALE=(-4,4), TITLE=' Right-going Incp S=3.0 cm X vs Y;';

PLOT SCAT, INCP, COLOR='BLUE', MAXNP=100000, H=X/cm, V=Y/cm,
  FILE='IncPair.top', LEFT, APPEND,
  HTITLE='X (cm);', VTITLE='Y (cm);', HSCALE=(-4,4), VSCALE=(-4,4), TITLE=' Left-going Incp S=3.0 cm X vs Y;';

 PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', RIGHT, APPEND,
  TITLE='Radius Right-going Incp S=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';

PLOT  HIST, INCP, H=Sqrt[X^2+Y^2]/cm, HSCALE=(0,20,100), VSCALE=(1,2E10), VLOG, COLOR='BLUE', 
  FILE='IncPair.top', LEFT, APPEND,
  TITLE='Radius Left-going Incp S=3.0 cm;',
        HTITLE='R  (cm);', VTITLE=' Nparticles;';
ENDIF;


!!!!Back to S=0

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
        TITLE='Left-going Incp Y vs X at S=0;',
        HTITLE='X (nm);      ;',
        VTITLE='Y (nm);      ;' ;
!
PLOT  SCAT, INCP,RIGHT,  MAXNP=60000,
  FILE='IncPair.top', APPEND,
        H=X/nm, V=Y/nm,
        HSCALE=(-200*sigx/nm,200*sigx/nm),
        VSCALE=(-200*sigy/nm,200*sigy/nm),
        TITLE='Right-going Incp Y vs X at S=0;',
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
  
