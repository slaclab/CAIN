ALLOCATE MP=2000000, MVPH=1000000, MLUMMESH=256 ;
SET photon=1, electron=2, positron=3, 
  mm=1D-3, micron=1D-6, nm=1D-9, 
ee=62.8D9,  gamma=ee/Emass,  
an=0.63D10,  
sigz=30*micron, 
betax=0.03*mm, 
betay=0.03*mm, 
emitx=0.12D-6/gamma, 
emity=0.12D-6/gamma,
nbunch=75, reprate=120,
sigx=Sqrt(emitx*betax), sigy=Sqrt(emity*betay), 
   sigxp=sigx/betax,  sigyp=sigy/betay;
 SET MsgLevel=1000000,spinleft=-1; 
 BEAM  RIGHT, KIND=electron, NP=10000, AN=an, E0=ee,
   BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,-spinleft);
!
 BEAM  LEFT,  KIND=electron, NP=10000, AN=an, E0=ee,
   BETA=(betax,betay), EMIT=(emitx,emity), SIGT=sigz, SPIN=(0,0,spinleft);
SET Smesh=sigz/2;
BBFIELD  NX=32, NY=32, WX=8*sigx, R=sigx/sigy/8, WXMAX=16*sigx;
LUMINOSITY  KIND=(electron,electron), W=(0,2*ee,50),
        WX=8*sigx, WY=8*sigy, FREP=nbunch*reprate ;
CFQED BEAMSTRAHLUNG, POL, PMAX=0.8;
FLAG OFF ECHO;
PUSH Time=(-2.5*sigz,2.5*sigz,200);
ENDPUSH;
PLOT LUMINOSITY, KIND=(electron,electron), FILE='../out/e-e-IP.top';
!PLOT LUMINOSITY, KIND=(electron,electron), FILE='../out/e-e-IP.top', APPEND;
