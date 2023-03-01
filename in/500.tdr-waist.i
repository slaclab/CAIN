! ILC - TDR  waist scan
 SET   incpair=1;
 SET    travelingfocus=1;

SET Rand=12345;
 ALLOCATE MP=2000000, MVPH=1000000, MLUMMESH=256, ;
FLAG OFF SPIN;
SET   mm=1E-3, micron=1E-6, nm=1E-9, nx=4.5, ny=4.5, nt=4.5, ne=4.5,
   ee=250E9,  gamma=ee/Emass,  an=2.00E10, nbunch=1312, reprate=5,
   sigz=0.3*mm, betax=11*mm, betay=480*micron,
   emitx=1.0E-5/gamma,  emity=3.5E-8/gamma,
   dalphadty=0, s0=0, 
   sigx=Sqrt(emitx*betax), sigy=Sqrt(emity*betay),
   sigxp=sigx/betax,  sigyp=sigy/betay,
   offstx=0.0,  offsty=0.*nm,
   slopex=0.007, slopey=0.000;
!
IF travelingfocus > 0;
  !SET    s0=Sqrt(3)/2*sigz, dalphdt=-0.5/betay;
  SET    s0=0.6*sigz, dalphadty=0.0;
ENDIF;
PRINT sigx,sigy;
!
SET MsgLevel=1;

!  Define electron beams at IP
 BEAM  RIGHT, KIND=2, NP=60000, AN=an, E0=ee,
      EUNIFORM, TXYS=(-s0,offstx,offsty,-s0), 
      BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz,GCUT=(nx,ny),
      DALPHADT=(0, dalphadty),
      GCUTT=nt, GCUTE=ne, SLOPE=(slopex,slopey), CRAB=(slopex,slopey);

!  Define positron beams at IP
 BEAM  LEFT,  KIND=3, NP=60000, AN=an, E0=ee,
      EUNIFORM, TXYS=(-s0,-offstx,-offsty,s0), 
      BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz,GCUT=(nx,ny),
      DALPHADT=(0, dalphadty),
      GCUTT=nt, GCUTE=ne, SLOPE=(slopex,slopey), CRAB=(slopex,slopey);

!  Longitudinal mesh size for luminosity and beam-field calculation.
!  SET Smesh=0.25*sigz;
 SET Smesh=0.1*sigz;
!  Define luminosities to be calculated
 LUMINOSITY  KIND=(2,3), W=(0,2*1.001*ee,100),
      WX=8*sigx, WY=8*sigy,  FREP=nbunch*reprate ;
! LUMINOSITY  KIND=(2,1), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(3,1), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(1,1), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(1,2), W=(0,2*1.001*ee,50);
! LUMINOSITY  KIND=(1,3), W=(0,2*1.001*ee,50);
!  Define parameters for beam-beam field calculation
 BBFIELD  NX=32, NY=64, WX=12*sigx, R=sigx/sigy/2;
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
 SET it=0, nstep=300;

!  Start tracking
 PUSH  Time=(-3*sigz,3*sigz,nstep);

!      plot (s,y) profile at t=0
    IF it==nstep/2;
 
     PLOT SCAT, KIND=2, H=S/micron, V=Y/nm,
        HSCALE=(-1000,1000), VSCALE=(-5*sigy/nm-offsty/nm,5*sigy/nm+offsty/nm),
        TITLE='Electron Profile at t=0;',
        HTITLE='s(Mm);  G  ;',
        VTITLE='y(nm);';

      PLOT SCAT, KIND=3, H=S/micron, V=Y/nm,
        HSCALE=(-1000,1000), VSCALE=(-5*sigy/nm-offsty/nm,5*sigy/nm+offsty/nm),
        TITLE='Positron Profile at t=0;',
        HTITLE='s(Mm);  G  ;',
        VTITLE='y(nm);';

    ENDIF;
!     clear opposite-charge particles to save cpu time
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

     PLOT SCAT, KIND=2, H=S/micron, V=Y/nm,
        HSCALE=(0,2000), VSCALE=(-300,300),
        TITLE='Electron Profile after collision;',
        HTITLE='s(Mm);  G  ;',
        VTITLE='y(nm);';

     PLOT SCAT, KIND=2, H=Y/nm, V=Py/Ps/micron,
        HSCALE=(-300,300), VSCALE=(-500,500),
        TITLE='Electron Profile after collision;',
        HTITLE='y(nm);',
        VTITLE='Y (Mrad);   G    ;' ;



 FLAG ON ECHO;
!  Pull all particles back to the plane s=0
DRIFT S=0;
PRINT STAT;
PRINT STAT, INCP;
PLOT  HIST, KIND=1, H=En/1E9, HSCALE=(0,1.001*ee/1E9,50), VLOG,
        TITLE='Beamstrahlung Energy Spectrum;',
        HTITLE='E0G1  (GeV); XGX       ;';
PLOT  HIST, KIND=(2,3), H=En/1E9, HSCALE=(0,1.001*ee/1E9,50), VLOG,
        TITLE='Final Electron Energy Spectrum;',
        HTITLE='E0e1  (GeV); X X       ;';

PLOT  HIST, KIND=2,  
        H=Py/Ps/micron,
        HSCALE=(-300,300),
        TITLE='Final Electron Angle Distribution;',
        HTITLE='Y (Mrad);   G    ;' ;
!        HTITLE='Y (urad)';

PLOT  HIST, KIND=3,  
        H=Py/Ps/micron,
        HSCALE=(-300,300),
        TITLE='Final Positron Angle Distribution;',
        HTITLE='Y (Mrad);   G    ;' ;
!        HTITLE='Y (urad)';


PLOT  SCAT, KIND=2,  MAXNP=60000,
        H=Px/Ps/micron-slopex/micron, V=Py/Ps/micron,
        HSCALE=(-15*sigxp/micron,15*sigxp/micron),
        VSCALE=(-500,500),
        TITLE='Final Electron Angle Distribution;',
        HTITLE='X (Mrad);   G    ;',
        VTITLE='Y (Mrad);   G    ;' ;
!        HTITLE='X (urad)',
!        VTITLE='Y (urad)' ;

PLOT  SCAT, KIND=3,  MAXNP=60000,
        H=Px/Ps/micron+slopex/micron, V=Py/Ps/micron,
        HSCALE=(-15*sigxp/micron,15*sigxp/micron),
!        VSCALE=(-120*sigyp/micron,120*sigyp/micron),
        VSCALE=(-500,500),
        TITLE='Final Positron Angle Distribution;',
!        HTITLE='X (urad)',
!        VTITLE='Y (urad)' ;
        HTITLE='X (Mrad);   G    ;',
        VTITLE='Y (Mrad);   G    ;' ;
! 
 IF incpair >0;
   PLOT  HIST, INCP, H=En/1E9, HSCALE=(0,30), VLOG,
          TITLE='Incoherent Pair Energy Spectrum;',
          HTITLE='E0e1  (GeV); X X       ;';
 
  PLOT  SCAT, INCP,   MAXNP=140000, VLOG, HLOG,
          V=En/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          VSCALE=(0.001,10),
          HSCALE=(10,1500),
          TITLE='Incoherent Pair Angle-Energy Distribution;',
          VTITLE='E (GeV);',
          HTITLE='Q (mrad);G       ;';

   PLOT  SCAT, INCP,   MAXNP=140000,
          H=ArcTan[Px/Ps]/mm, V=ArcTan[Py/Ps]/mm,
          HSCALE=(-500,500),
          VSCALE=(-500,500),
          TITLE='Incoherent Pair Angle Distribution;',
          HTITLE='X'' (mrad);         ;',
          VTITLE='Y'' (mrad);         ;' ;

   PLOT  SCAT, INCP,   MAXNP=140000, VLOG, HLOG,
          V=Sqrt[(Px^2+Py^2)]/1E9, H=ArcTan[Sqrt{(Px^2+Py^2)/Ps^2}]/mm,
          HSCALE=(10,1500),
          VSCALE=(0.005,0.2),
          TITLE='Incoherent Pair Angle-Pt Distribution;',
          VTITLE='Pt (GeV);',
          HTITLE='Q (mrad);G       ;';

   PLOT  SCAT, INCP,   MAXNP=140000,
          V=En/1E9, H=Abs[2*Sqrt(Px^2+Py^2)/1E9/0.3/3.5*Sin{0.3*3.5*3.6/Abs(Ps)*1E9*0.5}*100], 
          SELECT=(Ps>0.0),
          VSCALE=(0,3),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Radius-Energy Distribution (Pz>0);',
          VTITLE='E (GeV);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';
   PLOT  SCAT, INCP,   MAXNP=140000,
          V=En/1E9, H=Abs[2*Sqrt(Px^2+Py^2)/1E9/0.3/3.5*Sin{0.3*3.5*3.6/Abs(Ps)*1E9*0.5}*100], 
          SELECT=(Ps<0.0),
          VSCALE=(0,3),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Radius-Energy Distribution (Pz<0);',
          VTITLE='E (GeV);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';

! Detector solenoid magnet, B-field in positive z direction, distance of BeamCal from IP
SET bmag=3.5, zbcal=3.6, phibm=0.3*bmag*zbcal*0.5*1E9, ptb=0.3*bmag*1E9;


   PLOT  SCAT, INCP,   MAXNP=140000, 
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


    PLOT  SCAT, INCP,   MAXNP=140000, 
          V=Atan2[Sin(2.*phibm/Ps),Cos(2.*phibm/Ps)],
          H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100],
          SELECT=(Ps>0.0),
          VSCALE=(-Pi,Pi),
          HSCALE=(0,10),
          TITLE='Incoherent Pair Distribution (Pz>0);',
          VTITLE='Rotation angle of the helix (rad);',
          HTITLE='r (cm) at B=3.5T, L=3.6m;';
    PLOT  SCAT, INCP,   MAXNP=140000, 
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
          HSCALE=(0,10), VLOG,
          TITLE='Incoherent Pair Radial Spectrum (Pz>0);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';
   PLOT  HIST, INCP, H=Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100],
          SELECT=(Ps<0.0),
          HSCALE=(0,10), VLOG,
          TITLE='Incoherent Pair Radial Spectrum (Pz<0);',
          HTITLE='R (cm) at B=3.5T, L=3.6m;';
!!
    PLOT  HIST, INCP,
H=Atan2[ Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)),Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
          HSCALE=(-Pi,Pi), VLOG,
         SELECT=(5.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<6.0&&Ps>0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz>0);',
          HTITLE='Azimuthal angle in 5<R<6cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP,
H=Atan2[Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
         HSCALE=(-Pi,Pi), VLOG,
         SELECT=(5.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<6.0&&Ps<0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz<0);',
          HTITLE='Azimuthal angle in 5<R<6cm at B=3.5T, L=3.6m;';
!!
    PLOT  HIST, INCP,
H=Atan2[ Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
         HSCALE=(-Pi,Pi), VLOG,
         SELECT=(6.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<8.0&&Ps>0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz>0);',
          HTITLE='Azimuthal angle in 6<R<8cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP,
H=Atan2[ Sin(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)), Cos(Atan2(Py,Px)-[{Kind==3}-{Kind==2}]*phibm/Abs(Ps)) ],
         HSCALE=(-Pi,Pi), VLOG,
         SELECT=(6.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<8.0&&Ps<0.0),
          TITLE='Incoherent Pair Azimuthal angles (Pz<0);',
          HTITLE='Azimuthal angle in 6<R<8cm at B=3.5T, L=3.6m;';
!!

    PLOT  HIST, INCP, 
!          H={Kind==3&&Step(Ps)}-{Kind==2&&Step(Ps)}+{Kind==2&&Step(-Ps)}-{Kind==3&&Step(-Ps)},
          H=( (Kind==3)*Step(Ps)+2*(Kind==2)*Step(Ps)+3*(Kind==2)*Step(-Ps)+4*(Kind==3)*Step(-Ps) ),
          HSCALE=(0,5),
          TITLE='Incoherent Pair kinds;',
          HTITLE='1 for e+,Ps>0, 2 for e-,Ps<0, 3 for e-,Ps>0, 4 for e+<Ps<0;';

    PLOT  HIST, INCP, 
          H=($PName='ILL ')*1+($PName='IBH ')*2+($PName='IBW ')*3,
          HSCALE=(0,5),
          TITLE='Incoherent Pair creation processes;',
          HTITLE='1 for LL, 2 for BH and 3 for BW;';

    PLOT  HIST, INCP, H=En/1E9,
          HSCALE=(0,3),VLOG,
         SELECT=(5.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<6.0&&Ps>0.0),
          TITLE='Incoherent Pair Energy (Pz>0);',
          HTITLE='E (GeV) in 5<R<6cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP, H=En/1E9,
          HSCALE=(0,3),VLOG,
         SELECT=(5.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<6.0&&Ps<0.0),
          TITLE='Incoherent Pair Energy (Pz<0);',
          HTITLE='E (GeV) in 5<R<6cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP, H=En/1E9,
          HSCALE=(0,3),VLOG,
         SELECT=(6.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<8.0&&Ps>0.0),
          TITLE='Incoherent Pair Energy (Pz>0);',
          HTITLE='E (GeV) in 6<R<8cm at B=3.5T, L=3.6m;';
    PLOT  HIST, INCP, H=En/1E9,
          HSCALE=(0,3),VLOG,
         SELECT=(6.0<Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]&&Abs[2*Sqrt(Px^2+Py^2)/ptb*Sin{phibm/Abs(Ps)}*100]<8.0&&Ps<0.0),
          TITLE='Incoherent Pair Energy (Pz<0);',
          HTITLE='E (GeV) in 6<R<8cm at B=3.5T, L=3.6m;';

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
WRITE BEAM, KIND=1, FILE='../out/500.waist.beamstrahlung.dat';
WRITE BEAM, KIND=2, FILE='../out/500.waist.e.dat';
WRITE BEAM, KIND=3, FILE='../out/500.waist.p.dat';
! WRITE LUMINOSITY,KIND=(2,3),FILE='../out/500.waist.lum';
! Output differential luminosity
!  SET m1=LumEEbin(2,3,1,0), m2=LumEEbin(2,3,2,0);
!WRITE ((LumEEbin(2,3,1,n1),LumEEbin(2,3,2,n2),LumEE(2,3,n1,n2),n1=1,m1),n2=1,m2),
!  FORMAT=(G20.7," ",G20.7," ",G20.7),FILE='../out/500.waist.2dlum' ;
!IF incpair > 0;
! WRITE BEAM, INCP, FILE='../out/500.waist_pair.dat' ;
!ENDIF;
STOP;
