#
EXEC= ../exec/cain.exe
FC=gfortran
FOPT= -ffixed-line-length-80 -I./module -Jmodule
# Create and search for *.mod files in src/module/
# Note that all INCLUDE statements use relative path.
LOPT=

OBJS=  obj/arraymod.o obj/bbcom.o obj/bbpkcm.o obj/beamcm.o obj/beamln.o \
   obj/evlmod.o obj/flchtyp.o obj/lasrdata.o obj/lumcom.o obj/matchmod.o \
   obj/nlcplptab.o obj/readmod.o obj/aamain.o obj/acosh.o obj/addone.o obj/addtstp.o \
   obj/allocmem.o obj/altrue.o obj/anome.o obj/apsoff.o obj/arrayind.o \
   obj/arraystr.o obj/asinh.o obj/atanh.o obj/bballoc.o obj/bbbin.o \
   obj/bbcoul.o obj/bbctab.o obj/bbdphi.o obj/bbellp.o obj/bbfker.o \
   obj/bbfld.o obj/bbfpsn.o obj/bbharm.o obj/bekck.o obj/bbkck.o obj/bbkck0.o \
   obj/bbkick0.o obj/bbkkmm.o obj/bbmesh.o obj/bbout.o obj/wofz.o \
   obj/bbphi.o obj/bbqmom.o obj/bbxpnd.o obj/beamalloc.o obj/besjdj.o \
   obj/besk.o obj/binrng.o obj/bk13.o obj/bk23.o obj/bki13.o obj/bki53.o \
   obj/blalloc.o obj/blgeom.o obj/blkrec.o obj/blkrhs.o obj/blkrhs0.o \
   obj/bloptics.o obj/bltrack.o obj/bmclr.o obj/bmfile.o obj/bmfile2.o \
   obj/bmfile2c.o obj/bmfunc.o obj/bmini.o obj/bmst.o obj/bmstat.o \
   obj/bmstgn.o obj/bmteq.o obj/catcod.o obj/checknest.o \
   obj/chkmagname.o obj/clear8.o obj/clock1.o obj/cmdblk.o obj/cohpar.o \
   obj/cohpgn.o obj/copybm.o obj/cprnd1.o obj/cprnd2.o obj/cprsbm.o \
   obj/cputim.o obj/dbesik.o obj/dbesjy.o obj/defbl.o obj/defmag.o obj/deint.o \
   obj/dellos.o obj/derfc.o obj/dgamma.o obj/dgausc.o obj/dgausn.o \
   obj/distdbg.o obj/dlumcal.o obj/donut.o obj/drandu.o obj/drfext.o \
   obj/drift0.o obj/drift1.o obj/driftx.o obj/endpsh.o obj/enhfbw.o \
   obj/enhfcp.o obj/ermsg1.o obj/eval.o obj/eval0.o obj/evarrget.o \
   obj/evarrset.o obj/evchkvar.o obj/evcmpl.o obj/evcmpl0.o \
   obj/evdefarr.o obj/evdeff.o obj/evdefp.o obj/evendb.o obj/evendbc.o \
   obj/evfchk.o obj/evfin.o obj/evfree.o obj/evfun.o obj/evinit.o \
   obj/evinq.o obj/evlhs.o obj/evload.o obj/evpchk.o obj/evprarray.o \
   obj/evrdnm.o obj/evredc.o obj/evredcc.o obj/evufn.o obj/evufnbl.o \
   obj/evufnbs.o obj/evufnchar.o obj/evufnlaser.o obj/evufnlum.o \
   obj/evufnmath.o obj/evufntst.o obj/exflrz.o obj/exfspn.o obj/exlogc.o \
   obj/expandbl.o obj/expdis.o obj/extfev.o obj/extfld.o obj/findch.o \
   obj/flchset.o obj/flot1.o obj/fmaxbn.o obj/fmtcheck.o obj/fndch2.o \
   obj/fndend.o obj/ftreb.o obj/fvlin1.o obj/fvline.o obj/getmagid.o \
   obj/getnp.o obj/harmonic.o obj/hist.o obj/icbwgn.o obj/iccmpt.o \
   obj/idrel.o obj/ifarray.o obj/ifcmd.o obj/ifflag.o obj/iffun.o \
   obj/iflrkn.o obj/ifname.o obj/ifpar.o obj/ifreli.o obj/ifufn.o \
   obj/initlz.o obj/j0ton.o obj/jobdat.o obj/kinematics.o obj/lboost.o obj/lbwevent.o \
   obj/lchcod.o obj/lcpevent.o obj/lnbwgn.o obj/lncpgn.o obj/lorntz.o obj/lsrcoord.o \
   obj/lsrfltdl.o obj/lsrflush.o obj/lsrfocus.o obj/lsrgeo.o obj/lsrlrz.o obj/lsrphoton.o \
   obj/lsrprop0.o obj/lsrprop.o obj/lsrqedbh.o obj/lsrqedbw.o obj/lsrqedcp.o obj/lsrqed.o \
   obj/lsrrdfl.o obj/lsrtab.o obj/lsrwarn.o obj/lumalloc.o obj/lumbfl.o \
   obj/lumcal.o obj/lumcal0.o obj/lumini.o obj/lumplt.o obj/lumplt0.o \
   obj/lumplt1.o obj/lumprt.o obj/magset.o obj/matchfun.o obj/matching.o \
   obj/matchset.o obj/matmul.o obj/matvec.o obj/nextmag.o obj/nlbwfn.o \
   obj/nlbwfn0.o obj/nlbwgn.o obj/nlbwgn0.o obj/nlbwpe.o obj/nlbwst.o \
   obj/nlbwst0.o obj/nlbwstp.o obj/nlcpfn.o obj/nlcpfn0.o obj/nlcpgn.o \
   obj/nlcpgn00.o obj/nlcpgn0.o obj/nlcplfun.o obj/nlcplgn.o obj/nlcplpwn.o \
   obj/nlcplst4.o obj/nlcpst.o obj/nlcpst0.o obj/nllsr0.o obj/npcount.o \
   obj/nxtblk.o obj/nxtpos.o obj/openfl.o obj/openlogo.o obj/opticsint.o \
   obj/orthvc.o obj/paroff.o obj/plbbfl.o obj/plblgeom.o obj/plblopt.o \
   obj/plfunc.o obj/plhist.o obj/plscat.o obj/pltlaser.o obj/pltstp.o \
   obj/polarization.o obj/ppint.o obj/ppint2.o obj/prblgeom.o \
   obj/prblmag.o obj/prcmd.o obj/precho.o obj/prmaglist.o obj/prvpos.o \
   obj/push.o obj/pweight.o obj/randcain.o obj/ranini.o obj/rdall.o \
   obj/rdalloc.o obj/rdarray.o obj/rdbbfl.o obj/rdbeam.o obj/rdblopt.o \
   obj/rdbmln.o obj/rdcfqd.o obj/rdclr.o obj/rdcycext.o obj/rddbgflg.o obj/rddo.o \
   obj/rddrft.o obj/rdeddo.o obj/rdedif.o obj/rdelse.o obj/rdelseif.o \
   obj/rdendp.o obj/rdendtr.o obj/rdextf.o obj/rdfile.o obj/rdflag.o \
   obj/rdhead.o obj/rdif.o obj/rdlasr.o obj/rdlqed.o obj/rdlrnz.o \
   obj/rdlum.o obj/rdmagnet.o obj/rdmatch.o obj/rdplot.o obj/rdppi.o \
   obj/rdpush.o obj/rdrest.o obj/rdset.o obj/rdstor.o obj/rdtrans.o \
   obj/rdttl.o obj/rdwrdo.o obj/rdwrit.o obj/restolum.o obj/rndfr2.o \
   obj/rngktt.o obj/rotmat.o obj/scalarp4.o obj/scat.o obj/set00.o \
   obj/setcnst.o obj/setvar.o obj/setvar2.o obj/simeq.o obj/slveqm.o \
   obj/smesh1.o obj/spcoff.o obj/sqp.o obj/startbl.o obj/stopcain.o \
   obj/storelum.o obj/tdcnt1.o obj/tdhead.o obj/tmatrix.o obj/tolower.o \
   obj/toupper.o obj/trace.o obj/transport.o obj/trapz2.o obj/tstpst.o \
   obj/typttl.o obj/unit8.o obj/vphbfl.o obj/vphgen.o obj/vphoton.o \
   obj/wcohp.o obj/wrbeam.o obj/wrfmtfc.o obj/zfft.o obj/zfft2.o \
   obj/zffta.o obj/zfftb.o obj/closefiles.o obj/deffiles.o \
   obj/dogleg.o obj/dpmpar.o obj/enorm.o obj/fdjac1.o obj/hybrd.o \
   obj/qform.o obj/qrfac.o obj/r1mpyq.o obj/r1updt.o \
   obj/fcnemxy.o obj/fcnllmg.o obj/zemxy.o obj/calemxy.o \
   obj/multiga.o obj/initsig.o obj/chk0wgt.o
all : $(EXEC)

$(EXEC): $(OBJS)
	$(FC) -o $(EXEC) $(OBJS) $(LOPT)


obj/arraymod.o : module/arraymod.f
	$(FC) -c -o obj/arraymod.o $(FOPT) module/arraymod.f
obj/bbcom.o : module/bbcom.f
	$(FC) -c -o obj/bbcom.o $(FOPT) module/bbcom.f
obj/bbpkcm.o : module/bbpkcm.f
	$(FC) -c -o obj/bbpkcm.o $(FOPT) module/bbpkcm.f
obj/beamcm.o : module/beamcm.f
	$(FC) -c -o obj/beamcm.o $(FOPT) module/beamcm.f
obj/beamln.o : module/beamln.f obj/flchtyp.o
	$(FC) -c -o obj/beamln.o $(FOPT) module/beamln.f
obj/evlmod.o : module/evlmod.f obj/flchtyp.o
	$(FC) -c -o obj/evlmod.o $(FOPT) module/evlmod.f
obj/flchtyp.o : module/flchtyp.f
	$(FC) -c -o obj/flchtyp.o $(FOPT) module/flchtyp.f
obj/lasrdata.o : module/lasrdata.f
	$(FC) -c -o obj/lasrdata.o $(FOPT) module/lasrdata.f
obj/lumcom.o : module/lumcom.f
	$(FC) -c -o obj/lumcom.o $(FOPT) module/lumcom.f
obj/matchmod.o : module/matchmod.f obj/flchtyp.o
	$(FC) -c -o obj/matchmod.o $(FOPT) module/matchmod.f
obj/nlcplptab.o : module/nlcplptab.f
	$(FC) -c -o obj/nlcplptab.o $(FOPT) module/nlcplptab.f
obj/readmod.o : module/readmod.f obj/flchtyp.o
	$(FC) -c -o obj/readmod.o $(FOPT) module/readmod.f
obj/aamain.o : aamain.f obj/beamcm.o obj/bbcom.o obj/lasrdata.o \
   include/alloccm.h include/cmdnam.h include/ctrlcm.h include/nestcm.h \
   include/lasrcm.h
	$(FC) -c -o obj/aamain.o $(FOPT) aamain.f
obj/acosh.o : acosh.f
	$(FC) -c -o obj/acosh.o $(FOPT) acosh.f
obj/addone.o : addone.f obj/beamcm.o include/ctrlcm.h
	$(FC) -c -o obj/addone.o $(FOPT) addone.f
obj/addtstp.o : addtstp.f obj/flchtyp.o obj/readmod.o obj/beamcm.o include/ctrlcm.h \
   include/nestcm.h include/pushcm.h include/tstpcm.h \
   include/cnstcm.h
	$(FC) -c -o obj/addtstp.o $(FOPT) addtstp.f
obj/allocmem.o : allocmem.f obj/nlcplptab.o include/alloccm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/allocmem.o $(FOPT) allocmem.f
obj/altrue.o : altrue.f
	$(FC) -c -o obj/altrue.o $(FOPT) altrue.f
obj/anome.o : anome.f
	$(FC) -c -o obj/anome.o $(FOPT) anome.f
obj/apsoff.o : apsoff.f
	$(FC) -c -o obj/apsoff.o $(FOPT) apsoff.f
obj/arrayind.o : arrayind.f
	$(FC) -c -o obj/arrayind.o $(FOPT) arrayind.f
obj/arraystr.o : arraystr.f
	$(FC) -c -o obj/arraystr.o $(FOPT) arraystr.f
obj/asinh.o : asinh.f
	$(FC) -c -o obj/asinh.o $(FOPT) asinh.f
obj/atanh.o : atanh.f
	$(FC) -c -o obj/atanh.o $(FOPT) atanh.f
obj/bballoc.o : bballoc.f obj/bbcom.o obj/bbpkcm.o
	$(FC) -c -o obj/bballoc.o $(FOPT) bballoc.f
obj/bbbin.o : bbbin.f obj/beamcm.o obj/bbcom.o
	$(FC) -c -o obj/bbbin.o $(FOPT) bbbin.f
obj/bbcoul.o : bbcoul.f
	$(FC) -c -o obj/bbcoul.o $(FOPT) bbcoul.f
obj/bbctab.o : bbctab.f
	$(FC) -c -o obj/bbctab.o $(FOPT) bbctab.f
obj/bbdphi.o : bbdphi.f obj/bbpkcm.o
	$(FC) -c -o obj/bbdphi.o $(FOPT) bbdphi.f
obj/bbellp.o : bbellp.f
	$(FC) -c -o obj/bbellp.o $(FOPT) bbellp.f
obj/bbfker.o : bbfker.f obj/bbpkcm.o
	$(FC) -c -o obj/bbfker.o $(FOPT) bbfker.f
obj/bbfld.o : bbfld.f obj/beamcm.o obj/bbcom.o include/ctrlcm.h
	$(FC) -c -o obj/bbfld.o $(FOPT) bbfld.f
obj/bbfpsn.o : bbfpsn.f obj/bbpkcm.o
	$(FC) -c -o obj/bbfpsn.o $(FOPT) bbfpsn.f
obj/bbharm.o : bbharm.f
	$(FC) -c -o obj/bbharm.o $(FOPT) bbharm.f
obj/bbkck.o : bbkck.f obj/bbpkcm.o
	$(FC) -c -o obj/bbkck.o $(FOPT) bbkck.f
obj/bbkck0.o : bbkck0.f
	$(FC) -c -o obj/bbkck0.o $(FOPT) bbkck0.f
obj/bbkick0.o : bbkick0.f include/cnstcm.h
	$(FC) -c -o obj/bbkick0.o $(FOPT) bbkick0.f
obj/bbkkmm.o : bbkkmm.f
	$(FC) -c -o obj/bbkkmm.o $(FOPT) bbkkmm.f
obj/bbmesh.o : bbmesh.f obj/beamcm.o obj/bbcom.o
	$(FC) -c -o obj/bbmesh.o $(FOPT) bbmesh.f
obj/bbout.o : bbout.f
	$(FC) -c -o obj/bbout.o $(FOPT) bbout.f
obj/bbphi.o : bbphi.f obj/bbpkcm.o include/ctrlcm.h
	$(FC) -c -o obj/bbphi.o $(FOPT) bbphi.f
obj/bbqmom.o : bbqmom.f obj/bbpkcm.o
	$(FC) -c -o obj/bbqmom.o $(FOPT) bbqmom.f
obj/bbxpnd.o : bbxpnd.f obj/bbpkcm.o
	$(FC) -c -o obj/bbxpnd.o $(FOPT) bbxpnd.f
obj/beamalloc.o : beamalloc.f obj/beamcm.o include/ctrlcm.h
	$(FC) -c -o obj/beamalloc.o $(FOPT) beamalloc.f
obj/bekck.o : bekck.f obj/wofz.o
	$(FC) -c -o obj/bekck.o $(FOPT) bekck.f
obj/besjdj.o : besjdj.f include/ctrlcm.h
	$(FC) -c -o obj/besjdj.o $(FOPT) besjdj.f
obj/besk.o : besk.f include/ctrlcm.h
	$(FC) -c -o obj/besk.o $(FOPT) besk.f
obj/binrng.o : binrng.f
	$(FC) -c -o obj/binrng.o $(FOPT) binrng.f
obj/bk13.o : bk13.f include/ctrlcm.h
	$(FC) -c -o obj/bk13.o $(FOPT) bk13.f
obj/bk23.o : bk23.f include/ctrlcm.h
	$(FC) -c -o obj/bk23.o $(FOPT) bk23.f
obj/bki13.o : bki13.f include/ctrlcm.h
	$(FC) -c -o obj/bki13.o $(FOPT) bki13.f
obj/bki53.o : bki53.f include/ctrlcm.h
	$(FC) -c -o obj/bki53.o $(FOPT) bki53.f
obj/blalloc.o : blalloc.f obj/beamln.o
	$(FC) -c -o obj/blalloc.o $(FOPT) blalloc.f
obj/blgeom.o : blgeom.f
	$(FC) -c -o obj/blgeom.o $(FOPT) blgeom.f
obj/blkrec.o : blkrec.f
	$(FC) -c -o obj/blkrec.o $(FOPT) blkrec.f
obj/blkrhs.o : blkrhs.f obj/flchtyp.o obj/arraymod.o include/ctrlcm.h
	$(FC) -c -o obj/blkrhs.o $(FOPT) blkrhs.f
obj/blkrhs0.o : blkrhs0.f include/ctrlcm.h include/evchcod.h
	$(FC) -c -o obj/blkrhs0.o $(FOPT) blkrhs0.f
obj/bloptics.o : bloptics.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/bloptics.o $(FOPT) bloptics.f
obj/bltrack.o : bltrack.f
	$(FC) -c -o obj/bltrack.o $(FOPT) bltrack.f
obj/bmclr.o : bmclr.f obj/beamcm.o obj/flchtyp.o include/ctrlcm.h
	$(FC) -c -o obj/bmclr.o $(FOPT) bmclr.f
obj/bmfile.o : bmfile.f include/bmfmt.h include/ctrlcm.h
	$(FC) -c -o obj/bmfile.o $(FOPT) bmfile.f
obj/bmfile2.o : bmfile2.f obj/flchtyp.o obj/arraymod.o include/ctrlcm.h \
   include/chspcm.h include/evchcod.h include/nameleng.h \
   include/evparc.h include/cnstcm.h
	$(FC) -c -o obj/bmfile2.o $(FOPT) bmfile2.f
obj/bmfile2c.o : bmfile2c.f obj/flchtyp.o obj/arraymod.o include/ctrlcm.h \
   include/chspcm.h include/evchcod.h include/nameleng.h \
   include/evparc.h include/cnstcm.h
	$(FC) -c -o obj/bmfile2c.o $(FOPT) bmfile2c.f
obj/bmfunc.o : bmfunc.f obj/flchtyp.o obj/beamcm.o obj/arraymod.o
	$(FC) -c -o obj/bmfunc.o $(FOPT) bmfunc.f
obj/bmini.o : bmini.f obj/beamcm.o include/nestcm.h include/pushcm.h \
   include/cnstcm.h include/ctrlcm.h
	$(FC) -c -o obj/bmini.o $(FOPT) bmini.f
obj/bmst.o : bmst.f obj/beamcm.o include/ctrlcm.h include/cfqedcm.h \
   include/cnstcm.h
	$(FC) -c -o obj/bmst.o $(FOPT) bmst.f
obj/bmstat.o : bmstat.f obj/beamcm.o
	$(FC) -c -o obj/bmstat.o $(FOPT) bmstat.f
obj/bmstgn.o : bmstgn.f
	$(FC) -c -o obj/bmstgn.o $(FOPT) bmstgn.f
obj/bmteq.o : bmteq.f
	$(FC) -c -o obj/bmteq.o $(FOPT) bmteq.f
obj/catcod.o : catcod.f
	$(FC) -c -o obj/catcod.o $(FOPT) catcod.f
obj/checknest.o : checknest.f include/cmdnam.h include/nestcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/checknest.o $(FOPT) checknest.f
obj/chkmagname.o : chkmagname.f
	$(FC) -c -o obj/chkmagname.o $(FOPT) chkmagname.f
obj/clear8.o : clear8.f
	$(FC) -c -o obj/clear8.o $(FOPT) clear8.f
obj/clock1.o : clock1.f
	$(FC) -c -o obj/clock1.o $(FOPT) clock1.f
obj/cmdblk.o : cmdblk.f include/ctrlcm.h
	$(FC) -c -o obj/cmdblk.o $(FOPT) cmdblk.f
obj/cohpar.o : cohpar.f obj/beamcm.o include/ctrlcm.h include/cfqedcm.h
	$(FC) -c -o obj/cohpar.o $(FOPT) cohpar.f
obj/cohpgn.o : cohpgn.f
	$(FC) -c -o obj/cohpgn.o $(FOPT) cohpgn.f
obj/copybm.o : copybm.f obj/beamcm.o
	$(FC) -c -o obj/copybm.o $(FOPT) copybm.f
obj/cprnd1.o : cprnd1.f
	$(FC) -c -o obj/cprnd1.o $(FOPT) cprnd1.f
obj/cprnd2.o : cprnd2.f
	$(FC) -c -o obj/cprnd2.o $(FOPT) cprnd2.f
obj/cprsbm.o : cprsbm.f obj/beamcm.o include/ctrlcm.h
	$(FC) -c -o obj/cprsbm.o $(FOPT) cprsbm.f
obj/cputim.o : cputim.f include/ctrlcm.h
	$(FC) -c -o obj/cputim.o $(FOPT) cputim.f
obj/dbesik.o : dbesik.f
	$(FC) -c -o obj/dbesik.o $(FOPT) dbesik.f
obj/dbesjy.o : dbesjy.f include/ctrlcm.h
	$(FC) -c -o obj/dbesjy.o $(FOPT) dbesjy.f
obj/defbl.o : defbl.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/defbl.o $(FOPT) defbl.f
obj/defmag.o : defmag.f obj/flchtyp.o obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/defmag.o $(FOPT) defmag.f
obj/deint.o : deint.f
	$(FC) -c -o obj/deint.o $(FOPT) deint.f
obj/dellos.o : dellos.f obj/beamcm.o
	$(FC) -c -o obj/dellos.o $(FOPT) dellos.f
obj/derfc.o : derfc.f
	$(FC) -c -o obj/derfc.o $(FOPT) derfc.f
obj/dgamma.o : dgamma.f include/ctrlcm.h
	$(FC) -c -o obj/dgamma.o $(FOPT) dgamma.f
obj/dgausc.o : dgausc.f
	$(FC) -c -o obj/dgausc.o $(FOPT) dgausc.f
obj/dgausn.o : dgausn.f
	$(FC) -c -o obj/dgausn.o $(FOPT) dgausn.f
obj/distdbg.o : distdbg.f
	$(FC) -c -o obj/distdbg.o $(FOPT) distdbg.f
obj/dlumcal.o : dlumcal.f
	$(FC) -c -o obj/dlumcal.o $(FOPT) dlumcal.f
obj/donut.o : donut.f obj/lasrdata.o include/lasrcm.h include/donutpar.h
	$(FC) -c -o obj/donut.o $(FOPT) donut.f
obj/drandu.o : drandu.f
	$(FC) -c -o obj/drandu.o $(FOPT) drandu.f
obj/drfext.o : drfext.f
	$(FC) -c -o obj/drfext.o $(FOPT) drfext.f
obj/drift0.o : drift0.f obj/beamcm.o
	$(FC) -c -o obj/drift0.o $(FOPT) drift0.f
obj/drift1.o : drift1.f obj/beamcm.o include/extfcm.h include/cnstcm.h
	$(FC) -c -o obj/drift1.o $(FOPT) drift1.f
obj/driftx.o : driftx.f obj/beamcm.o include/ctrlcm.h include/extfcm.h
	$(FC) -c -o obj/driftx.o $(FOPT) driftx.f
obj/endpsh.o : endpsh.f obj/beamcm.o obj/bbcom.o obj/lumcom.o \
   obj/lasrdata.o include/ctrlcm.h include/extfcm.h include/tstpcm.h \
   include/lasrcm.h include/lumcom2.h include/cfqedcm.h
	$(FC) -c -o obj/endpsh.o $(FOPT) endpsh.f
obj/enhfbw.o : enhfbw.f obj/flchtyp.o obj/arraymod.o obj/lasrdata.o \
   include/ctrlcm.h include/lasrcm.h
	$(FC) -c -o obj/enhfbw.o $(FOPT) enhfbw.f
obj/enhfcp.o : enhfcp.f obj/flchtyp.o obj/lasrdata.o include/ctrlcm.h \
   include/lasrcm.h
	$(FC) -c -o obj/enhfcp.o $(FOPT) enhfcp.f
obj/ermsg1.o : ermsg1.f
	$(FC) -c -o obj/ermsg1.o $(FOPT) ermsg1.f
obj/eval.o : eval.f obj/flchtyp.o include/nameleng.h include/evparc.h \
   include/ufncm.h include/evchkm.h include/evalcm.h
	$(FC) -c -o obj/eval.o $(FOPT) eval.f
obj/eval0.o : eval0.f obj/flchtyp.o obj/arraymod.o include/nameleng.h \
   include/evparc.h include/funlis.h include/ufncm.h include/evchkm.h \
   include/evtypcod.h include/evchcod.h
	$(FC) -c -o obj/eval0.o $(FOPT) eval0.f
obj/evarrget.o : evarrget.f obj/flchtyp.o obj/arraymod.o
	$(FC) -c -o obj/evarrget.o $(FOPT) evarrget.f
obj/evarrset.o : evarrset.f obj/arraymod.o
	$(FC) -c -o obj/evarrset.o $(FOPT) evarrset.f
obj/evchkvar.o : evchkvar.f obj/flchtyp.o obj/arraymod.o \
   include/nameleng.h include/evparc.h include/ufncm.h include/evchkm.h \
   include/evalcm.h include/evtypcod.h
	$(FC) -c -o obj/evchkvar.o $(FOPT) evchkvar.f
obj/evcmpl.o : evcmpl.f obj/flchtyp.o obj/evlmod.o obj/arraymod.o \
   include/nameleng.h include/evparc.h include/ufncm.h include/evchkm.h \
   include/evalcm.h include/ctrlcm.h include/ctrlcm3.h
	$(FC) -c -o obj/evcmpl.o $(FOPT) evcmpl.f
obj/evcmpl0.o : evcmpl0.f obj/flchtyp.o obj/arraymod.o include/nameleng.h \
   include/evparc.h include/funlis.h include/ufncm.h include/evchkm.h \
   include/evtypcod.h include/evchcod.h
	$(FC) -c -o obj/evcmpl0.o $(FOPT) evcmpl0.f
obj/evdefarr.o : evdefarr.f obj/arraymod.o include/nameleng.h \
   include/evparc.h include/ufncm.h include/funlis.h include/evtypcod.h \
   include/evchcod.h include/ctrlcm3.h
	$(FC) -c -o obj/evdefarr.o $(FOPT) evdefarr.f
obj/evdeff.o : evdeff.f obj/arraymod.o include/nameleng.h \
   include/evparc.h include/ufncm.h include/funlis.h include/evchcod.h
	$(FC) -c -o obj/evdeff.o $(FOPT) evdeff.f
obj/evdefp.o : evdefp.f obj/arraymod.o include/nameleng.h \
   include/evparc.h include/ufncm.h include/funlis.h include/evchcod.h
	$(FC) -c -o obj/evdefp.o $(FOPT) evdefp.f
obj/evendb.o : evendb.f obj/flchtyp.o include/evtypcod.h
	$(FC) -c -o obj/evendb.o $(FOPT) evendb.f
obj/evendbc.o : evendbc.f include/evtypcod.h
	$(FC) -c -o obj/evendbc.o $(FOPT) evendbc.f
obj/evfchk.o : evfchk.f include/nameleng.h include/evparc.h \
   include/ufncm.h include/evchkm.h
	$(FC) -c -o obj/evfchk.o $(FOPT) evfchk.f
obj/evfin.o : evfin.f obj/flchtyp.o obj/arraymod.o obj/evlmod.o
	$(FC) -c -o obj/evfin.o $(FOPT) evfin.f
obj/evfree.o : evfree.f obj/flchtyp.o obj/evlmod.o obj/arraymod.o
	$(FC) -c -o obj/evfree.o $(FOPT) evfree.f
obj/evfun.o : evfun.f obj/flchtyp.o
	$(FC) -c -o obj/evfun.o $(FOPT) evfun.f
obj/evinit.o : evinit.f obj/flchtyp.o obj/arraymod.o obj/evlmod.o \
   include/nameleng.h include/evparc.h include/ufncm.h include/funlis.h \
   include/chspcm.h include/evalcm.h include/evalcm2.h include/evcnst.h
	$(FC) -c -o obj/evinit.o $(FOPT) evinit.f
obj/evinq.o : evinq.f include/nameleng.h include/evparc.h
	$(FC) -c -o obj/evinq.o $(FOPT) evinq.f
obj/evlhs.o : evlhs.f obj/flchtyp.o include/evtypcod.h include/evchcod.h
	$(FC) -c -o obj/evlhs.o $(FOPT) evlhs.f
obj/evload.o : evload.f obj/flchtyp.o obj/evlmod.o obj/arraymod.o \
   include/nameleng.h include/evparc.h include/ctrlcm.h include/ufncm.h
	$(FC) -c -o obj/evload.o $(FOPT) evload.f
obj/evpchk.o : evpchk.f include/nameleng.h include/evparc.h \
   include/ufncm.h include/evchkm.h
	$(FC) -c -o obj/evpchk.o $(FOPT) evpchk.f
obj/evprarray.o : evprarray.f obj/arraymod.o
	$(FC) -c -o obj/evprarray.o $(FOPT) evprarray.f
obj/evrdnm.o : evrdnm.f include/evchcod.h
	$(FC) -c -o obj/evrdnm.o $(FOPT) evrdnm.f
obj/evredc.o : evredc.f obj/flchtyp.o obj/arraymod.o
	$(FC) -c -o obj/evredc.o $(FOPT) evredc.f
obj/evredcc.o : evredcc.f
	$(FC) -c -o obj/evredcc.o $(FOPT) evredcc.f
obj/evufn.o : evufn.f obj/flchtyp.o include/ctrlcm3.h include/ctrlcm.h
	$(FC) -c -o obj/evufn.o $(FOPT) evufn.f
obj/evufnbl.o : evufnbl.f obj/flchtyp.o obj/beamln.o obj/matchmod.o \
   include/transpcm.h
	$(FC) -c -o obj/evufnbl.o $(FOPT) evufnbl.f
obj/evufnbs.o : evufnbs.f obj/flchtyp.o
	$(FC) -c -o obj/evufnbs.o $(FOPT) evufnbs.f
obj/evufnchar.o : evufnchar.f obj/flchtyp.o include/ctrlcm3.h
	$(FC) -c -o obj/evufnchar.o $(FOPT) evufnchar.f
obj/evufnlaser.o : evufnlaser.f obj/flchtyp.o obj/lasrdata.o \
   include/lasrcm.h
	$(FC) -c -o obj/evufnlaser.o $(FOPT) evufnlaser.f
obj/evufnlum.o : evufnlum.f obj/flchtyp.o obj/lumcom.o include/lumcom2.h
	$(FC) -c -o obj/evufnlum.o $(FOPT) evufnlum.f
obj/evufnmath.o : evufnmath.f obj/flchtyp.o
	$(FC) -c -o obj/evufnmath.o $(FOPT) evufnmath.f
obj/evufntst.o : evufntst.f obj/flchtyp.o obj/beamcm.o
	$(FC) -c -o obj/evufntst.o $(FOPT) evufntst.f
obj/exflrz.o : exflrz.f include/extfcm.h
	$(FC) -c -o obj/exflrz.o $(FOPT) exflrz.f
obj/exfspn.o : exfspn.f
	$(FC) -c -o obj/exfspn.o $(FOPT) exfspn.f
obj/exlogc.o : exlogc.f obj/flchtyp.o include/ctrlcm.h
	$(FC) -c -o obj/exlogc.o $(FOPT) exlogc.f
obj/expandbl.o : expandbl.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/expandbl.o $(FOPT) expandbl.f
obj/expdis.o : expdis.f
	$(FC) -c -o obj/expdis.o $(FOPT) expdis.f
obj/extfev.o : extfev.f
	$(FC) -c -o obj/extfev.o $(FOPT) extfev.f
obj/extfld.o : extfld.f obj/beamcm.o include/extfcm.h include/cnstcm.h
	$(FC) -c -o obj/extfld.o $(FOPT) extfld.f
obj/findch.o : findch.f
	$(FC) -c -o obj/findch.o $(FOPT) findch.f
obj/flchset.o : flchset.f obj/flchtyp.o obj/arraymod.o
	$(FC) -c -o obj/flchset.o $(FOPT) flchset.f
obj/flot1.o : flot1.f
	$(FC) -c -o obj/flot1.o $(FOPT) flot1.f
obj/fmaxbn.o : fmaxbn.f
	$(FC) -c -o obj/fmaxbn.o $(FOPT) fmaxbn.f
obj/fmtcheck.o : fmtcheck.f
	$(FC) -c -o obj/fmtcheck.o $(FOPT) fmtcheck.f
obj/fndch2.o : fndch2.f
	$(FC) -c -o obj/fndch2.o $(FOPT) fndch2.f
obj/fndend.o : fndend.f include/cmdnam.h include/nestcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/fndend.o $(FOPT) fndend.f
obj/ftreb.o : ftreb.f
	$(FC) -c -o obj/ftreb.o $(FOPT) ftreb.f
obj/fvlin1.o : fvlin1.f
	$(FC) -c -o obj/fvlin1.o $(FOPT) fvlin1.f
obj/fvline.o : fvline.f
	$(FC) -c -o obj/fvline.o $(FOPT) fvline.f
obj/getmagid.o : getmagid.f obj/beamln.o
	$(FC) -c -o obj/getmagid.o $(FOPT) getmagid.f
obj/getnp.o : getnp.f obj/beamcm.o
	$(FC) -c -o obj/getnp.o $(FOPT) getnp.f
obj/harmonic.o : harmonic.f obj/nlcplptab.o
	$(FC) -c -o obj/harmonic.o $(FOPT) harmonic.f
obj/hist.o : hist.f include/topdraw.h
	$(FC) -c -o obj/hist.o $(FOPT) hist.f
obj/icbwgn.o : icbwgn.f
	$(FC) -c -o obj/icbwgn.o $(FOPT) icbwgn.f
obj/iccmpt.o : iccmpt.f include/cnstcm.h
	$(FC) -c -o obj/iccmpt.o $(FOPT) iccmpt.f
obj/idrel.o : idrel.f
	$(FC) -c -o obj/idrel.o $(FOPT) idrel.f
obj/ifarray.o : ifarray.f obj/arraymod.o
	$(FC) -c -o obj/ifarray.o $(FOPT) ifarray.f
obj/ifcmd.o : ifcmd.f
	$(FC) -c -o obj/ifcmd.o $(FOPT) ifcmd.f
obj/ifflag.o : ifflag.f include/ctrlcm.h include/ctrlcm2.h
	$(FC) -c -o obj/ifflag.o $(FOPT) ifflag.f
obj/iffun.o : iffun.f include/nameleng.h include/funlis.h
	$(FC) -c -o obj/iffun.o $(FOPT) iffun.f
obj/iflrkn.o : iflrkn.f obj/beamcm.o
	$(FC) -c -o obj/iflrkn.o $(FOPT) iflrkn.f
obj/ifname.o : ifname.f obj/arraymod.o include/nameleng.h \
   include/evparc.h include/ufncm.h include/evchkm.h include/evtypcod.h
	$(FC) -c -o obj/ifname.o $(FOPT) ifname.f
obj/ifpar.o : ifpar.f include/nameleng.h include/evparc.h
	$(FC) -c -o obj/ifpar.o $(FOPT) ifpar.f
obj/ifreli.o : ifreli.f
	$(FC) -c -o obj/ifreli.o $(FOPT) ifreli.f
obj/ifufn.o : ifufn.f include/nameleng.h include/ufncm.h
	$(FC) -c -o obj/ifufn.o $(FOPT) ifufn.f
obj/initlz.o : initlz.f obj/flchtyp.o obj/readmod.o obj/beamcm.o obj/bbcom.o \
   obj/lumcom.o obj/matchmod.o obj/lasrdata.o include/ctrlcm.h \
   include/ctrlcm2.h include/ctrlcm3.h include/cmdnam.h include/tstpcm.h \
   include/lasrcm.h include/cfqedcm.h include/extfcm.h include/headcm.h \
   include/transpcm.h
	$(FC) -c -o obj/initlz.o $(FOPT) initlz.f
obj/j0ton.o : j0ton.f include/ctrlcm.h
	$(FC) -c -o obj/j0ton.o $(FOPT) j0ton.f
obj/jobdat.o : jobdat.f
	$(FC) -c -o obj/jobdat.o $(FOPT) jobdat.f
obj/kinematics.o : kinematics.f
	$(FC) -c -o obj/kinematics.o $(FOPT) kinematics.f
obj/lboost.o : lboost.f
	$(FC) -c -o obj/lboost.o $(FOPT) lboost.f
obj/lbwevent.o : lbwevent.f obj/beamcm.o 
	$(FC) -c -o obj/lbwevent.o $(FOPT) lbwevent.f
obj/lchcod.o : lchcod.f include/chspcm.h include/evchcod.h
	$(FC) -c -o obj/lchcod.o $(FOPT) lchcod.f
obj/lcpevent.o : lcpevent.f obj/beamcm.o
	$(FC) -c -o obj/lcpevent.o $(FOPT) lcpevent.f
obj/lnbwgn.o : lnbwgn.f include/ctrlcm.h
	$(FC) -c -o obj/lnbwgn.o $(FOPT) lnbwgn.f
obj/lncpgn.o : lncpgn.f include/ctrlcm.h
	$(FC) -c -o obj/lncpgn.o $(FOPT) lncpgn.f
obj/lorntz.o : lorntz.f obj/beamcm.o include/ctrlcm.h include/cnstcm.h
	$(FC) -c -o obj/lorntz.o $(FOPT) lorntz.f
obj/lsrcoord.o : lsrcoord.f obj/lasrdata.o include/lasrcm.h
	$(FC) -c -o obj/lsrcoord.o $(FOPT) lsrcoord.f
obj/lsrfltdl.o : lsrfltdl.f obj/lasrdata.o include/ctrlcm.h
	$(FC) -c -o obj/lsrfltdl.o $(FOPT) lsrfltdl.f
obj/lsrflush.o : lsrflush.f include/ctrlcm.h include/cnstcm.h
	$(FC) -c -o obj/lsrflush.o $(FOPT) lsrflush.f
obj/lsrfocus.o : lsrfocus.f
	$(FC) -c -o obj/lsrfocus.o $(FOPT) lsrfocus.f
obj/lsrgeo.o : lsrgeo.f obj/lasrdata.o include/lasrcm.h
	$(FC) -c -o obj/lsrgeo.o $(FOPT) lsrgeo.f
obj/lsrlrz.o : lsrlrz.f obj/lasrdata.o include/lasrcm.h
	$(FC) -c -o obj/lsrlrz.o $(FOPT) lsrlrz.f
obj/lsrphoton.o : lsrphoton.f obj/lasrdata.o include/lasrcm.h
	$(FC) -c -o obj/lsrphoton.o $(FOPT) lsrphoton.f
obj/lsrprop0.o : lsrprop0.f include/ctrlcm.h
	$(FC) -c -o obj/lsrprop0.o $(FOPT) lsrprop0.f
obj/lsrprop.o : lsrprop.f obj/lasrdata.o include/ctrlcm.h include/lasrcm.h
	$(FC) -c -o obj/lsrprop.o $(FOPT) lsrprop.f
obj/lsrqedbh.o : lsrqedbh.f obj/beamcm.o obj/lasrdata.o include/lasrcm.h \
   include/ctrlcm.h include/cnstcm.h
	$(FC) -c -o obj/lsrqedbh.o $(FOPT) lsrqedbh.f
obj/lsrqedbw.o : lsrqedbw.f obj/beamcm.o obj/lasrdata.o include/lasrcm.h \
   include/ctrlcm.h include/cnstcm.h
	$(FC) -c -o obj/lsrqedbw.o $(FOPT) lsrqedbw.f
obj/lsrqedcp.o : lsrqedcp.f obj/beamcm.o obj/lasrdata.o include/lasrcm.h \
   include/ctrlcm.h include/cnstcm.h
	$(FC) -c -o obj/lsrqedcp.o $(FOPT) lsrqedcp.f
obj/lsrqed.o : lsrqed.f obj/beamcm.o obj/lasrdata.o include/lasrcm.h \
   include/ctrlcm.h include/cnstcm.h
	$(FC) -c -o obj/lsrqed.o $(FOPT) lsrqed.f
obj/lsrrdfl.o : lsrrdfl.f obj/lasrdata.o include/lasrcm.h
	$(FC) -c -o obj/lsrrdfl.o $(FOPT) lsrrdfl.f
obj/lsrtab.o : lsrtab.f obj/lasrdata.o include/lasrcm.h
	$(FC) -c -o obj/lsrtab.o $(FOPT) lsrtab.f
obj/lsrwarn.o : lsrwarn.f obj/lasrdata.o include/lasrcm.h \
   include/nllsrcm.h include/ctrlcm.h
	$(FC) -c -o obj/lsrwarn.o $(FOPT) lsrwarn.f
obj/lumalloc.o : lumalloc.f obj/lumcom.o
	$(FC) -c -o obj/lumalloc.o $(FOPT) lumalloc.f
obj/lumbfl.o : lumbfl.f obj/beamcm.o obj/lumcom.o include/lumcom2.h \
   include/ctrlcm.h
	$(FC) -c -o obj/lumbfl.o $(FOPT) lumbfl.f
obj/lumcal.o : lumcal.f obj/lumcom.o include/lumcom2.h
	$(FC) -c -o obj/lumcal.o $(FOPT) lumcal.f
obj/lumcal0.o : lumcal0.f
	$(FC) -c -o obj/lumcal0.o $(FOPT) lumcal0.f
obj/lumini.o : lumini.f obj/lumcom.o include/lumcom2.h
	$(FC) -c -o obj/lumini.o $(FOPT) lumini.f
obj/lumplt.o : lumplt.f obj/lumcom.o include/ctrlcm.h include/lumcom2.h
	$(FC) -c -o obj/lumplt.o $(FOPT) lumplt.f
obj/lumplt0.o : lumplt0.f include/topdraw.h
	$(FC) -c -o obj/lumplt0.o $(FOPT) lumplt0.f
obj/lumplt1.o : lumplt1.f obj/flchtyp.o include/ctrlcm3.h \
   include/ctrlcm.h
	$(FC) -c -o obj/lumplt1.o $(FOPT) lumplt1.f
obj/lumprt.o : lumprt.f obj/lumcom.o include/ctrlcm.h include/lumcom2.h
	$(FC) -c -o obj/lumprt.o $(FOPT) lumprt.f
obj/magset.o : magset.f obj/flchtyp.o obj/beamln.o obj/arraymod.o \
   obj/matchmod.o
	$(FC) -c -o obj/magset.o $(FOPT) magset.f
obj/matchfun.o : matchfun.f obj/flchtyp.o obj/arraymod.o obj/matchmod.o \
   include/nameleng.h include/evparc.h include/ctrlcm.h
	$(FC) -c -o obj/matchfun.o $(FOPT) matchfun.f
obj/matching.o : matching.f obj/flchtyp.o obj/arraymod.o obj/matchmod.o \
   obj/beamln.o include/nameleng.h include/evparc.h
	$(FC) -c -o obj/matching.o $(FOPT) matching.f
obj/matchset.o : matchset.f obj/flchtyp.o obj/beamln.o obj/arraymod.o \
   obj/matchmod.o include/ctrlcm.h
	$(FC) -c -o obj/matchset.o $(FOPT) matchset.f
obj/matmul.o : matmul.f include/ctrlcm.h
	$(FC) -c -o obj/matmul.o $(FOPT) matmul.f
obj/matvec.o : matvec.f include/ctrlcm.h
	$(FC) -c -o obj/matvec.o $(FOPT) matvec.f
obj/nextmag.o : nextmag.f obj/beamln.o include/ctrlcm.h \
   include/blexpand.h
	$(FC) -c -o obj/nextmag.o $(FOPT) nextmag.f
obj/nlbwfn.o : nlbwfn.f include/nllsrcm.h include/nlbwcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/nlbwfn.o $(FOPT) nlbwfn.f
obj/nlbwfn0.o : nlbwfn0.f
	$(FC) -c -o obj/nlbwfn0.o $(FOPT) nlbwfn0.f
obj/nlbwgn.o : nlbwgn.f include/nllsrcm.h include/nlbwcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/nlbwgn.o $(FOPT) nlbwgn.f
obj/nlbwgn0.o : nlbwgn0.f include/ctrlcm.h
	$(FC) -c -o obj/nlbwgn0.o $(FOPT) nlbwgn0.f
obj/nlbwpe.o : nlbwpe.f
	$(FC) -c -o obj/nlbwpe.o $(FOPT) nlbwpe.f
obj/nlbwst.o : nlbwst.f include/nllsrcm.h include/nlbwcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/nlbwst.o $(FOPT) nlbwst.f
obj/nlbwst0.o : nlbwst0.f
	$(FC) -c -o obj/nlbwst0.o $(FOPT) nlbwst0.f
obj/nlbwstp.o : nlbwstp.f include/nllsrcm.h include/nlbwcm.h \
   include/topdraw.h
	$(FC) -c -o obj/nlbwstp.o $(FOPT) nlbwstp.f
obj/nlcpfn.o : nlcpfn.f include/nllsrcm.h include/nlcpcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/nlcpfn.o $(FOPT) nlcpfn.f
obj/nlcpfn0.o : nlcpfn0.f
	$(FC) -c -o obj/nlcpfn0.o $(FOPT) nlcpfn0.f
obj/nlcpgn.o : nlcpgn.f include/nllsrcm.h include/nlcpcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/nlcpgn.o $(FOPT) nlcpgn.f
obj/nlcpgn00.o : nlcpgn00.f include/ctrlcm.h
	$(FC) -c -o obj/nlcpgn00.o $(FOPT) nlcpgn00.f
obj/nlcpgn0.o : nlcpgn0.f include/ctrlcm.h
	$(FC) -c -o obj/nlcpgn0.o $(FOPT) nlcpgn0.f
obj/nlcplfun.o : nlcplfun.f
	$(FC) -c -o obj/nlcplfun.o $(FOPT) nlcplfun.f
obj/nlcplgn.o : nlcplgn.f obj/nlcplptab.o include/nlcpcm.h
	$(FC) -c -o obj/nlcplgn.o $(FOPT) nlcplgn.f
obj/nlcplpwn.o : nlcplpwn.f obj/nlcplptab.o
	$(FC) -c -o obj/nlcplpwn.o $(FOPT) nlcplpwn.f
obj/nlcplst4.o : nlcplst4.f obj/nlcplptab.o include/nlcpcm.h
	$(FC) -c -o obj/nlcplst4.o $(FOPT) nlcplst4.f
obj/nlcpst.o : nlcpst.f include/nllsrcm.h include/nlcpcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/nlcpst.o $(FOPT) nlcpst.f
obj/nlcpst0.o : nlcpst0.f
	$(FC) -c -o obj/nlcpst0.o $(FOPT) nlcpst0.f
obj/nllsr0.o : nllsr0.f include/nllsrcm.h
	$(FC) -c -o obj/nllsr0.o $(FOPT) nllsr0.f
obj/npcount.o : npcount.f obj/flchtyp.o obj/beamcm.o
	$(FC) -c -o obj/npcount.o $(FOPT) npcount.f
obj/nxtblk.o : nxtblk.f include/ctrlcm.h
	$(FC) -c -o obj/nxtblk.o $(FOPT) nxtblk.f
obj/nxtpos.o : nxtpos.f
	$(FC) -c -o obj/nxtpos.o $(FOPT) nxtpos.f
obj/openfl.o : openfl.f include/ctrlcm.h
	$(FC) -c -o obj/openfl.o $(FOPT) openfl.f
obj/openlogo.o : openlogo.f include/headcm.h include/ctrlcm.h
	$(FC) -c -o obj/openlogo.o $(FOPT) openlogo.f
obj/opticsint.o : opticsint.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/opticsint.o $(FOPT) opticsint.f
obj/orthvc.o : orthvc.f
	$(FC) -c -o obj/orthvc.o $(FOPT) orthvc.f
obj/paroff.o : paroff.f
	$(FC) -c -o obj/paroff.o $(FOPT) paroff.f
obj/plbbfl.o : plbbfl.f obj/beamcm.o obj/bbcom.o include/ctrlcm.h \
   include/cnstcm.h include/topdraw.h
	$(FC) -c -o obj/plbbfl.o $(FOPT) plbbfl.f
obj/plblgeom.o : plblgeom.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/plblgeom.o $(FOPT) plblgeom.f
obj/plblopt.o : plblopt.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/plblopt.o $(FOPT) plblopt.f
obj/plfunc.o : plfunc.f obj/flchtyp.o
	$(FC) -c -o obj/plfunc.o $(FOPT) plfunc.f
obj/plhist.o : plhist.f obj/flchtyp.o
	$(FC) -c -o obj/plhist.o $(FOPT) plhist.f
obj/plscat.o : plscat.f obj/flchtyp.o obj/beamcm.o
	$(FC) -c -o obj/plscat.o $(FOPT) plscat.f
obj/pltlaser.o : pltlaser.f obj/lasrdata.o include/ctrlcm.h \
   include/lasrcm.h include/topdraw.h
	$(FC) -c -o obj/pltlaser.o $(FOPT) pltlaser.f
obj/pltstp.o : pltstp.f obj/flchtyp.o obj/beamcm.o include/tstpcm.h \
   include/topdraw.h
	$(FC) -c -o obj/pltstp.o $(FOPT) pltstp.f
obj/polarization.o : polarization.f obj/nlcplptab.o
	$(FC) -c -o obj/polarization.o $(FOPT) polarization.f
obj/ppint.o : ppint.f obj/beamcm.o obj/lumcom.o include/ctrlcm.h
	$(FC) -c -o obj/ppint.o $(FOPT) ppint.f
obj/ppint2.o : ppint2.f
	$(FC) -c -o obj/ppint2.o $(FOPT) ppint2.f
obj/prblgeom.o : prblgeom.f obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/prblgeom.o $(FOPT) prblgeom.f
obj/prblmag.o : prblmag.f obj/beamln.o
	$(FC) -c -o obj/prblmag.o $(FOPT) prblmag.f
obj/prcmd.o : prcmd.f include/ctrlcm.h
	$(FC) -c -o obj/prcmd.o $(FOPT) prcmd.f
obj/precho.o : precho.f include/ctrlcm.h
	$(FC) -c -o obj/precho.o $(FOPT) precho.f
obj/prmaglist.o : prmaglist.f obj/beamln.o
	$(FC) -c -o obj/prmaglist.o $(FOPT) prmaglist.f
obj/prvpos.o : prvpos.f
	$(FC) -c -o obj/prvpos.o $(FOPT) prvpos.f
obj/push.o : push.f obj/flchtyp.o obj/beamcm.o obj/bbcom.o obj/lumcom.o \
   obj/lasrdata.o include/ctrlcm.h include/pushcm.h include/lumcom2.h \
   include/cfqedcm.h include/lasrcm.h
	$(FC) -c -o obj/push.o $(FOPT) push.f
obj/pweight.o : pweight.f obj/beamcm.o
	$(FC) -c -o obj/pweight.o $(FOPT) pweight.f
obj/randcain.o : randcain.f
	$(FC) -c -o obj/randcain.o $(FOPT) randcain.f
obj/ranini.o : ranini.f
	$(FC) -c -o obj/ranini.o $(FOPT) ranini.f
obj/rdall.o : rdall.f include/ctrlcm.h include/cmdnam.h
	$(FC) -c -o obj/rdall.o $(FOPT) rdall.f
obj/rdalloc.o : rdalloc.f obj/flchtyp.o obj/readmod.o include/alloccm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/rdalloc.o $(FOPT) rdalloc.f
obj/rdarray.o : rdarray.f obj/flchtyp.o obj/readmod.o include/ctrlcm.h \
   include/nameleng.h
	$(FC) -c -o obj/rdarray.o $(FOPT) rdarray.f
obj/rdbbfl.o : rdbbfl.f obj/flchtyp.o obj/readmod.o obj/bbcom.o include/ctrlcm.h
	$(FC) -c -o obj/rdbbfl.o $(FOPT) rdbbfl.f
obj/rdbeam.o : rdbeam.f obj/flchtyp.o obj/readmod.o obj/beamcm.o obj/arraymod.o \
   include/ctrlcm.h include/nestcm.h include/pushcm.h \
   include/tstpcm.h include/cnstcm.h
	$(FC) -c -o obj/rdbeam.o $(FOPT) rdbeam.f
obj/rdblopt.o : rdblopt.f obj/flchtyp.o obj/readmod.o obj/beamcm.o include/ctrlcm.h \
   include/nestcm.h
	$(FC) -c -o obj/rdblopt.o $(FOPT) rdblopt.f
obj/rdbmln.o : rdbmln.f obj/beamln.o obj/readmod.o include/ctrlcm.h
	$(FC) -c -o obj/rdbmln.o $(FOPT) rdbmln.f
obj/rdcfqd.o : rdcfqd.f obj/flchtyp.o obj/readmod.o include/ctrlcm.h \
   include/cfqedcm.h
	$(FC) -c -o obj/rdcfqd.o $(FOPT) rdcfqd.f
obj/rdclr.o : rdclr.f obj/flchtyp.o obj/readmod.o obj/bbcom.o obj/lumcom.o \
   obj/lasrdata.o include/ctrlcm.h include/cfqedcm.h \
   include/lasrcm.h include/extfcm.h include/lumcom2.h
	$(FC) -c -o obj/rdclr.o $(FOPT) rdclr.f
obj/rdcycext.o : rdcycext.f include/cmdnam.h include/nestcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/rdcycext.o $(FOPT) rdcycext.f
obj/rddbgflg.o : rddbgflg.f obj/flchtyp.o obj/readmod.o \
   include/alloccm.h include/ctrlcm.h 
	$(FC) -c -o obj/rddbgflg.o $(FOPT) rddbgflg.f
obj/rddo.o : rddo.f obj/flchtyp.o obj/readmod.o obj/arraymod.o include/cmdnam.h \
   include/ctrlcm.h include/nestcm.h include/pushcm.h \
   include/nameleng.h include/evparc.h
	$(FC) -c -o obj/rddo.o $(FOPT) rddo.f
obj/rddrft.o : rddrft.f obj/flchtyp.o obj/readmod.o include/ctrlcm.h
	$(FC) -c -o obj/rddrft.o $(FOPT) rddrft.f
obj/rdeddo.o : rdeddo.f include/nestcm.h include/ctrlcm.h
	$(FC) -c -o obj/rdeddo.o $(FOPT) rdeddo.f
obj/rdedif.o : rdedif.f include/nestcm.h include/ctrlcm.h
	$(FC) -c -o obj/rdedif.o $(FOPT) rdedif.f
obj/rdelse.o : rdelse.f include/cmdnam.h include/nestcm.h \
   include/ctrlcm.h
	$(FC) -c -o obj/rdelse.o $(FOPT) rdelse.f
obj/rdelseif.o : rdelseif.f obj/flchtyp.o obj/readmod.o include/cmdnam.h \
   include/ctrlcm.h include/nestcm.h include/pushcm.h
	$(FC) -c -o obj/rdelseif.o $(FOPT) rdelseif.f
obj/rdendp.o : rdendp.f include/ctrlcm.h include/nestcm.h \
   include/pushcm.h
	$(FC) -c -o obj/rdendp.o $(FOPT) rdendp.f
obj/rdendtr.o : rdendtr.f obj/beamcm.o include/ctrlcm.h include/nestcm.h \
   include/pushcm.h
	$(FC) -c -o obj/rdendtr.o $(FOPT) rdendtr.f
obj/rdextf.o : rdextf.f obj/flchtyp.o obj/readmod.o include/ctrlcm.h \
   include/extfcm.h
	$(FC) -c -o obj/rdextf.o $(FOPT) rdextf.f
obj/rdfile.o : rdfile.f obj/flchtyp.o obj/readmod.o obj/arraymod.o include/ctrlcm.h
	$(FC) -c -o obj/rdfile.o $(FOPT) rdfile.f
obj/rdflag.o : rdflag.f obj/flchtyp.o obj/readmod.o include/ctrlcm.h include/ctrlcm2.h
	$(FC) -c -o obj/rdflag.o $(FOPT) rdflag.f
obj/rdhead.o : rdhead.f obj/flchtyp.o obj/readmod.o obj/arraymod.o include/ctrlcm.h \
   include/headcm.h
	$(FC) -c -o obj/rdhead.o $(FOPT) rdhead.f
obj/rdif.o : rdif.f obj/flchtyp.o obj/readmod.o include/cmdnam.h include/ctrlcm.h \
   include/nestcm.h include/pushcm.h
	$(FC) -c -o obj/rdif.o $(FOPT) rdif.f
obj/rdlasr.o : rdlasr.f obj/flchtyp.o obj/readmod.o obj/arraymod.o obj/lasrdata.o \
   include/ctrlcm.h include/cnstcm.h include/lasrcm.h
	$(FC) -c -o obj/rdlasr.o $(FOPT) rdlasr.f
obj/rdlqed.o : rdlqed.f obj/flchtyp.o obj/readmod.o obj/lasrdata.o include/ctrlcm.h \
   include/lasrcm.h
	$(FC) -c -o obj/rdlqed.o $(FOPT) rdlqed.f
obj/rdlrnz.o : rdlrnz.f obj/flchtyp.o obj/readmod.o obj/lasrdata.o include/ctrlcm.h \
   include/extfcm.h include/lasrcm.h
	$(FC) -c -o obj/rdlrnz.o $(FOPT) rdlrnz.f
obj/rdlum.o : rdlum.f obj/flchtyp.o obj/readmod.o obj/lumcom.o include/ctrlcm.h \
   include/lumcom2.h
	$(FC) -c -o obj/rdlum.o $(FOPT) rdlum.f
obj/rdmagnet.o : rdmagnet.f obj/flchtyp.o obj/readmod.o obj/beamln.o include/ctrlcm.h
	$(FC) -c -o obj/rdmagnet.o $(FOPT) rdmagnet.f
obj/rdmatch.o : rdmatch.f obj/flchtyp.o obj/readmod.o obj/beamln.o obj/arraymod.o \
   obj/matchmod.o include/ctrlcm.h include/nameleng.h \
   include/evparc.h
	$(FC) -c -o obj/rdmatch.o $(FOPT) rdmatch.f
obj/rdplot.o : rdplot.f obj/flchtyp.o obj/readmod.o obj/arraymod.o obj/bbcom.o \
   obj/lasrdata.o include/ctrlcm.h include/topdraw.h \
   include/lasrcm.h
	$(FC) -c -o obj/rdplot.o $(FOPT) rdplot.f
obj/rdppi.o : rdppi.f obj/flchtyp.o obj/readmod.o obj/lumcom.o include/ctrlcm.h \
   include/cnstcm.h
	$(FC) -c -o obj/rdppi.o $(FOPT) rdppi.f
obj/rdpush.o : rdpush.f obj/flchtyp.o obj/readmod.o include/ctrlcm.h \
   include/nestcm.h
	$(FC) -c -o obj/rdpush.o $(FOPT) rdpush.f
obj/rdrest.o : rdrest.f obj/flchtyp.o obj/readmod.o obj/arraymod.o obj/lumcom.o \
   include/ctrlcm.h include/lumcom2.h include/stdstor.h
	$(FC) -c -o obj/rdrest.o $(FOPT) rdrest.f
obj/rdset.o : rdset.f obj/flchtyp.o obj/readmod.o obj/arraymod.o include/ctrlcm.h
	$(FC) -c -o obj/rdset.o $(FOPT) rdset.f
obj/rdstor.o : rdstor.f obj/flchtyp.o obj/readmod.o obj/arraymod.o obj/lumcom.o \
   include/ctrlcm.h include/lumcom2.h include/stdstor.h
	$(FC) -c -o obj/rdstor.o $(FOPT) rdstor.f
obj/rdtrans.o : rdtrans.f obj/flchtyp.o obj/readmod.o obj/beamcm.o include/ctrlcm.h \
   include/cnstcm.h include/nestcm.h
	$(FC) -c -o obj/rdtrans.o $(FOPT) rdtrans.f
obj/rdttl.o : rdttl.f
	$(FC) -c -o obj/rdttl.o $(FOPT) rdttl.f
obj/rdwrdo.o : rdwrdo.f obj/flchtyp.o obj/arraymod.o include/nestcm.h
	$(FC) -c -o obj/rdwrdo.o $(FOPT) rdwrdo.f
obj/rdwrit.o : rdwrit.f obj/flchtyp.o obj/readmod.o obj/arraymod.o include/ctrlcm.h
	$(FC) -c -o obj/rdwrit.o $(FOPT) rdwrit.f
obj/restolum.o : restolum.f obj/lumcom.o include/lumcom2.h
	$(FC) -c -o obj/restolum.o $(FOPT) restolum.f
obj/rndfr2.o : rndfr2.f
	$(FC) -c -o obj/rndfr2.o $(FOPT) rndfr2.f
obj/rngktt.o : rngktt.f
	$(FC) -c -o obj/rngktt.o $(FOPT) rngktt.f
obj/rotmat.o : rotmat.f
	$(FC) -c -o obj/rotmat.o $(FOPT) rotmat.f
obj/scalarp4.o : scalarp4.f
	$(FC) -c -o obj/scalarp4.o $(FOPT) scalarp4.f
obj/scat.o : scat.f include/topdraw.h
	$(FC) -c -o obj/scat.o $(FOPT) scat.f
obj/set00.o : set00.f include/ctrlcm.h include/ctrlcm2.h \
   include/ctrlcm3.h
	$(FC) -c -o obj/set00.o $(FOPT) set00.f
obj/setcnst.o : setcnst.f obj/lumcom.o include/cnstcm.h include/ctrlcm3.h
	$(FC) -c -o obj/setcnst.o $(FOPT) setcnst.f
obj/setvar.o : setvar.f obj/beamcm.o include/ctrlcm3.h
	$(FC) -c -o obj/setvar.o $(FOPT) setvar.f
obj/setvar2.o : setvar2.f include/ctrlcm3.h
	$(FC) -c -o obj/setvar2.o $(FOPT) setvar2.f
obj/simeq.o : simeq.f include/ctrlcm.h
	$(FC) -c -o obj/simeq.o $(FOPT) simeq.f
obj/slveqm.o : slveqm.f obj/beamcm.o obj/bbcom.o include/ctrlcm.h \
   include/cfqedcm.h include/cnstcm.h
	$(FC) -c -o obj/slveqm.o $(FOPT) slveqm.f
obj/smesh1.o : smesh1.f obj/beamcm.o
	$(FC) -c -o obj/smesh1.o $(FOPT) smesh1.f
obj/spcoff.o : spcoff.f
	$(FC) -c -o obj/spcoff.o $(FOPT) spcoff.f
obj/sqp.o : sqp.f
	$(FC) -c -o obj/sqp.o $(FOPT) sqp.f
obj/startbl.o : startbl.f obj/beamln.o include/ctrlcm.h \
   include/blexpand.h
	$(FC) -c -o obj/startbl.o $(FOPT) startbl.f
obj/stopcain.o : stopcain.f
	$(FC) -c -o obj/stopcain.o $(FOPT) stopcain.f
obj/storelum.o : storelum.f obj/lumcom.o include/headcm.h \
   include/lumcom2.h
	$(FC) -c -o obj/storelum.o $(FOPT) storelum.f
obj/tdcnt1.o : tdcnt1.f include/topdraw.h
	$(FC) -c -o obj/tdcnt1.o $(FOPT) tdcnt1.f
obj/tdhead.o : tdhead.f include/headcm.h
	$(FC) -c -o obj/tdhead.o $(FOPT) tdhead.f
obj/tmatrix.o : tmatrix.f include/ctrlcm.h
	$(FC) -c -o obj/tmatrix.o $(FOPT) tmatrix.f
obj/tolower.o : tolower.f
	$(FC) -c -o obj/tolower.o $(FOPT) tolower.f
obj/toupper.o : toupper.f
	$(FC) -c -o obj/toupper.o $(FOPT) toupper.f
obj/trace.o : trace.f include/ctrlcm.h
	$(FC) -c -o obj/trace.o $(FOPT) trace.f
obj/transport.o : transport.f obj/beamln.o include/ctrlcm.h \
   include/transpcm.h
	$(FC) -c -o obj/transport.o $(FOPT) transport.f
obj/trapz2.o : trapz2.f
	$(FC) -c -o obj/trapz2.o $(FOPT) trapz2.f
obj/tstpst.o : tstpst.f obj/beamcm.o include/tstpcm.h
	$(FC) -c -o obj/tstpst.o $(FOPT) tstpst.f
obj/typttl.o : typttl.f
	$(FC) -c -o obj/typttl.o $(FOPT) typttl.f
obj/unit8.o : unit8.f
	$(FC) -c -o obj/unit8.o $(FOPT) unit8.f
obj/vphbfl.o : vphbfl.f obj/beamcm.o obj/lumcom.o include/cnstcm.h \
   include/topdraw.h include/ctrlcm.h
	$(FC) -c -o obj/vphbfl.o $(FOPT) vphbfl.f
obj/vphgen.o : vphgen.f include/ctrlcm.h
	$(FC) -c -o obj/vphgen.o $(FOPT) vphgen.f
obj/vphoton.o : vphoton.f include/cnstcm.h
	$(FC) -c -o obj/vphoton.o $(FOPT) vphoton.f
obj/wcohp.o : wcohp.f include/ctrlcm.h
	$(FC) -c -o obj/wcohp.o $(FOPT) wcohp.f
obj/wofz.o : wofz.f
	$(FC) -c -o obj/wofz.o $(FOPT) wofz.f
obj/wrbeam.o : wrbeam.f obj/flchtyp.o obj/beamcm.o include/ctrlcm.h \
   include/bmfmt.h
	$(FC) -c -o obj/wrbeam.o $(FOPT) wrbeam.f
obj/wrfmtfc.o : wrfmtfc.f obj/flchtyp.o include/evchcod.h
	$(FC) -c -o obj/wrfmtfc.o $(FOPT) wrfmtfc.f
obj/zfft.o : zfft.f include/ctrlcm.h
	$(FC) -c -o obj/zfft.o $(FOPT) zfft.f
obj/zfft2.o : zfft2.f
	$(FC) -c -o obj/zfft2.o $(FOPT) zfft2.f
obj/zffta.o : zffta.f include/ctrlcm.h
	$(FC) -c -o obj/zffta.o $(FOPT) zffta.f
obj/zfftb.o : zfftb.f include/ctrlcm.h
	$(FC) -c -o obj/zfftb.o $(FOPT) zfftb.f
obj/closefiles.o : unix/closefiles.f
	$(FC) -c -o obj/closefiles.o $(FOPT) unix/closefiles.f
obj/deffiles.o : unix/deffiles.f
	$(FC) -c -o obj/deffiles.o $(FOPT) unix/deffiles.f
obj/dogleg.o : minpack/dogleg.f
	$(FC) -c -o obj/dogleg.o $(FOPT) minpack/dogleg.f
obj/dpmpar.o : minpack/dpmpar.f
	$(FC) -c -o obj/dpmpar.o $(FOPT) minpack/dpmpar.f
obj/enorm.o : minpack/enorm.f
	$(FC) -c -o obj/enorm.o $(FOPT) minpack/enorm.f
obj/fdjac1.o : minpack/fdjac1.f
	$(FC) -c -o obj/fdjac1.o $(FOPT) minpack/fdjac1.f
obj/hybrd.o : minpack/hybrd.f
	$(FC) -c -o obj/hybrd.o $(FOPT) minpack/hybrd.f
obj/qform.o : minpack/qform.f
	$(FC) -c -o obj/qform.o $(FOPT) minpack/qform.f
obj/qrfac.o : minpack/qrfac.f
	$(FC) -c -o obj/qrfac.o $(FOPT) minpack/qrfac.f
obj/r1mpyq.o : minpack/r1mpyq.f
	$(FC) -c -o obj/r1mpyq.o $(FOPT) minpack/r1mpyq.f
obj/r1updt.o : minpack/r1updt.f
	$(FC) -c -o obj/r1updt.o $(FOPT) minpack/r1updt.f
obj/fcnemxy.o : fcnemxy.f
	$(FC) -c -o obj/fcnemxy.o $(FOPT) fcnemxy.f
obj/fcnllmg.o : fcnllmg.f
	$(FC) -c -o obj/fcnllmg.o $(FOPT) fcnllmg.f
obj/zemxy.o : zemxy.f
	$(FC) -c -o obj/zemxy.o $(FOPT) zemxy.f
obj/calemxy.o : calemxy.f
	$(FC) -c -o obj/calemxy.o $(FOPT) calemxy.f
obj/multiga.o : multiga.f
	$(FC) -c -o obj/multiga.o $(FOPT) multiga.f
obj/initsig.o : initsig.f
	$(FC) -c -o obj/initsig.o $(FOPT) initsig.f
obj/chk0wgt.o : chk0wgt.f
	$(FC) -c -o obj/chk0wgt.o $(FOPT) chk0wgt.f

cleanobj :
	rm -f obj/*.o
	rm -f module/*.mod
