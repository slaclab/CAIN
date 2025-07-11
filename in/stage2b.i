!  BNL Compton chamber experiment. 2nd stage
 HEADER  'BNL Compton stage.2';
 ALLOCATE MP=100000;
!  Restore variables
 RESTORE FILE='../out/temp2.dat';
 SET egmax=lambda/(1+lambda)*ee*3;
 PRINT xi, FORMAT=('  Xi=',1PE9.3);
 PRINT egmax/1e3, FORMAT=('  Egmax=',1PE9.3,'keV');
!  Read particle data from file
 BEAM  FILE='../out/temp.dat';
 PRINT STAT;
 PLOT  HIST, RIGHT, KIND=electron, H=En/1D6, HSCALE=(0,emax/1e6,50),
        TITLE='Final Electron Energy Spectrum;',
        HTITLE='E0e1 (MeV); X X      ;';
 PLOT  HIST, KIND=photon, RIGHT, H=En/1D3, HSCALE=(0,egmax/1e3,50),
        TITLE='Photon Energy Spectrum;',
        HTITLE='E0G1 (keV); XGX      ;'  ;
 PLOT  SCAT, KIND=photon, RIGHT,
        H=En/1D3, V=Sqrt((Px^2+Py^2)/Ps^2)/1D-3,
        HSCALE=(0,egmax/1e3), VSCALE=(0,20),
        TITLE='Photon Energy vs. Angle;',
        HTITLE='E0G1 (keV); XGX      ;',
        VTITLE='Q0r1 (mrad);GX X       ';
 PLOT  SCAT, KIND=photon, RIGHT,
        H=Px/Ps/1D-3, V=Py/Ps/1D-3,
        HSCALE=(-20,20), VSCALE=(-20,20),
        TITLE='Photon (x,y) Angle;',
        HTITLE='Q0x1 (mrad);GX X       ',
        VTITLE='Q0y1 (mrad);GX X       ';
 PLOT  SCAT, KIND=photon, RIGHT,
        H=Px/Ps/1D-3, V=Py/Ps/1D-3, SELECT=(En>7e3),
        HSCALE=(-20,20), VSCALE=(-20,20),
        TITLE='Photon (x,y) Angle,  E>7keV;',
        HTITLE='Q0x1 (mrad);GX X       ',
        VTITLE='Q0y1 (mrad);GX X       ';
 PLOT  SCAT, KIND=photon, RIGHT,
        H=En/1D3, V=Xi3,
        HSCALE=(0,egmax/1e3), VSCALE=(-1,1),
        TITLE='Photon Energy vs. X031;                  GX X',
        HTITLE='E0G1 (keV); XGX      ;',
        VTITLE='X031;GX X;';
 STOP;
