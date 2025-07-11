!  ILC  Gamma-Gamma Collider   Interaction point
!
!  Read variables written by HiggsnCP.i (corresponds to the
!  command STORE in HiggsnCP.i)
ALLOCATE MP=360000000, MVPH=1000000, MLUMMESH=256 ;
PRINT FORMAT=(' HiggsIP point 000');
RESTORE FILE='higgs.txt';
HEADER  'ILC G-G  for a Higgs Factory;    G G;';
! SET sigxp=sigx/betax,  sigyp=sigy/betay,slopex=0.000, slopey=0.000;
SET MsgLevel=100000;
!!
!SET   incpair=1;
SET   incpair=0;
!!
SET xpar=4*ee*omegal/Emass/Emass,
egmax=xpar/(xpar+1)*ee;
!!
!  Read particle data from the file written by HiggsnCP.i
 BEAM  FILE='higgs.dat';
!!
 SET Rand=123456789;
 SET ntcutip=2.5;
 SET MsgLevel=0;  FLAG OFF ECHO;
 SET tmin=-ntcutip*sigz, tmax=ntcutip*sigz, nstep=300;
! SET tmin=-ntcutip*sigz, tmax=ntcutip*sigz, nstep=500;
!!
! SET wmin=wmax*0.8,wbin=40;
! SET wmin=2.*egmax*0.8,wgmax=2*egmax,wbin=50;
! SET wmin=2.*egmax*0.5,wgmax=2*egmax,wbin=50;
! SET wmin=2.*egmax*0.8,wgmax=2*egmax,wbin=50;
 SET wmin=2.*egmax*0.95,wgmax=2*egmax,wbin=50;
!!
!LUMINOSITY  KIND=(photon,photon), W=(0,wee*lambda/(lambda+1),wnbin), HELICITY,
LUMINOSITY  KIND=(photon,photon), W=(123d9,125.5d9,500), HELICITY,
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

 SET MsgLevel=0;
 SET it=0;
 PRINT CPUTIME;
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
            HSCALE=(-ntcutip*sigz/micron,ntcutip*sigz/micron),
            VSCALE=(-50*sigy/nm,50*sigy/nm), MAXNP=4000,
            TITLE='Electron at IP;',
            HTITLE='S (Mm);   G  ;', VTITLE='Y (nm);' ;
!!
PLOT  BBFIELD, S=0,
FILE='HiggsIP.top', APPEND;
!!
    ENDIF;
    SET it=it+1;
 ENDPUSH;
 PRINT CPUTIME;
!!
! plots
 FLAG ON ECHO;
!  Pull all particles back to the plane s=0
DRIFT S=0;
!
PRINT STAT;
PRINT STAT, INCP;
!PLOT  HIST, KIND=photon, H=En/1E9, HSCALE=(0,62.73126,136), VLOG,
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
FLAG OFF ECHO;
PRINT Lum(1,1),FORMAT=('luminosity gam gam=',1pd15.8);
PRINT Lum(1,2),FORMAT=('luminosity gam e-=',1pd15.8);
PRINT Lum(2,1),FORMAT=('luminosity e- gam=',1pd15.8);
PRINT Lum(2,2),FORMAT=('luminosity e- e-=',1pd15.8);
PRINT Lum(1,3),FORMAT=('luminosity gam e+=',1pd15.8);
PRINT Lum(3,1),FORMAT=('luminosity e+ gam=',1pd15.8);
PRINT Lum(2,3),FORMAT=('luminosity e- e+=',1pd15.8);
PRINT Lum(3,2),FORMAT=('luminosity e+ e-=',1pd15.8);
! 
 STOP;
