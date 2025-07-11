!  BNL Compton chamber experiment. 2nd stage
 HEADER  'BNL Compton stage.2';
 ALLOCATE MP=100000;
 SET   photon=1, electron=2, positron=3,
   mm=1D-3, micron=1D-6, nm=1D-9, mu0=4*Pi*1D-7, psec=1e-12*Cvel,
   ee=60D6,  gamma=ee/Emass,  an=0.5e-9/Echarge,  zFWHM=3.5*psec, 
   emitx=2D-6/gamma, emity=2D-6/gamma,
   sigz=zFWHM/(2*Sqrt(2*Log(2))),
   sigx=32*micron, sigy=32*micron, sige=0.0015, 
   betax=sigx^2/emitx, betay=sigy^2/emity,
   ntcut=3.0,
   laserwl=10*micron, pulseE=30.0, lambar=laserwl/(2*Pi),
   omegal=Hbarc/lambar, 
   sigLr=32*micron, rayl=2/lambar*sigLr^2,
   pulsel=30*psec, sigt=pulsel/Sqrt(12),
   angle=0, tdl=1.0,
   powerd=pulseE*Cvel/[Pi*lambar*sigt*Sqrt(2*Pi*rayl^2*tdl^2)],
   xisq=powerd*mu0*Cvel*(lambar/Emass)^2,   xi=Sqrt(xisq),
   lambda=4*omegal*ee/Emass^2;
 SET MsgLevel=1;
 BEAM  RIGHT, KIND=electron, NP=5000, AN=an, E0=ee,
   TXYS=(0,0,0,0),  GCUTT=ntcut, SIGE=sige,
   BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,0);
 LASER LEFT, WAVEL=laserwl, POWERD=powerd,
      TXYS=(0,0,0,0),
      E3=(0,-Sin(angle),-Cos(angle)), E1=(1,0,0), 
      RAYLEIGH=(rayl,rayl), SIGT=sigt, GCUTT=ntcut, STOKES=(0,0,1),
      TDL=(tdl,tdl) ;
 LASERQED  COMPTON, LINEARPOL, NPH=5, XIMAX=1.1*xi, LAMBDAMAX=1.1*lambda,
   PMAX=0.5;
! LASERQED  COMPTON, NPH=0;
 SET MsgLevel=0;  FLAG OFF ECHO;
 SET Smesh=sigt/3;
 SET emax=1.001*ee, wmax=emax;
 SET  it=0;
 PRINT CPUTIME;
 PUSH  Time=(-ntcut*(sigt+sigz),ntcut*(sigt+sigz),200);
      IF Mod(it,20)=0;
        PRINT it, FORMAT=(F6.0,'-th time step'); PRINT STAT, SHORT;
      ENDIF;
      SET it=it+1;
 ENDPUSH;
 PRINT CPUTIME;
!  Pull all particles to the IP
 DRIFT S=0;
!  Write particle data onto a file for the job for IP
 WRITE BEAM, KIND=(electron,photon), FILE='../out/temp.dat';
!  Store variables
 STORE FILE='../out/temp2.dat';
 PRINT STAT;
 STOP;
