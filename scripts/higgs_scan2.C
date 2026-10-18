// ============================================================================
// higgs_scan2.C — CAIN dump scanner, v2
//  * charge ledger over ALL species (sum of Weight, macro counts, E min/max)
//  * for selected species & E-range: weighted moments of X,Y at the log plane
//    AND projected to S=0 (straight-line drift), S<0 / S>0 stats as before
//  * GEN tally: N, sum(W), <E> per generation  (e+ GEN=2 -> BH off primary;
//    GEN=3 -> BW off primary photon; GEN>=4 mixed)
// Usage:  root -l -q 'higgs_scan2.C("higgs.dat", 3, 0., 1e15)'
// ============================================================================
#include <fstream>
#include <string>
#include <vector>
#include <cmath>
#include <cstdio>
#include <cstdlib>

void higgs_scan2(const char* fname = "higgs.dat", int species = 3,
                 double Emin = 0.0, double Emax = 1.0e99)
{
   std::ifstream fin(fname);
   if (!fin.is_open()) { printf("ERROR: cannot open %s\n", fname); return; }

   long double ledW[4] = {0,0,0,0};  long long ledN[4] = {0,0,0,0};
   double ledElo[4] = {1e99,1e99,1e99,1e99}, ledEhi[4] = {0,0,0,0};

   long double sw=0, swx=0,swx2=0, swy=0,swy2=0;
   long double swxp=0,swxp2=0, swyp=0,swyp2=0;              // projected to S=0
   long double swn=0,swsn=0,swsn2=0, swp=0,swsp=0,swsp2=0;  // S<0 / S>0
   long long   nSel=0, nLines=0;

   const int NG = 64;
   long double genW[NG] = {0}, genWE[NG] = {0};  long long genN[NG] = {0};

   std::string line;  std::vector<double> tok;  tok.reserve(16);
   while (std::getline(fin, line)) {
      ++nLines;
      if (nLines % 5000000 == 0) printf("  ... %lld lines\n", nLines);
      size_t p0 = line.find_first_not_of(" \t\r");
      if (p0 == std::string::npos || line[p0] == '!') continue;
      for (size_t i = 0; i < line.size(); ++i)
         if (line[i] == 'D' || line[i] == 'd') line[i] = 'E';
      tok.clear();
      const char* p = line.c_str();  char* end = nullptr;
      while (*p) {
         while (*p == ' ' || *p == '\t') ++p;
         if (!*p) break;
         double v = strtod(p, &end);
         if (end == p) { while (*p && *p != ' ' && *p != '\t') ++p; }
         else          { tok.push_back(v);  p = end; }
      }
      if (tok.size() < 11) continue;
      const int K = (int)(tok[0] + 0.5);
      if (K < 1 || K > 3) continue;
      const double W = tok[2], X = tok[4], Y = tok[5], S = tok[6], E = tok[7];
      const double Px = tok[8], Py = tok[9], Ps = tok[10];

      ledW[K] += W;  ++ledN[K];
      if (E < ledElo[K]) ledElo[K] = E;
      if (E > ledEhi[K]) ledEhi[K] = E;

      if (K != species || E < Emin || E > Emax) continue;
      ++nSel;  sw += W;
      swx += W*X;  swx2 += W*X*X;  swy += W*Y;  swy2 += W*Y*Y;
      if (Ps != 0.0) {
         const double Xp = X - S*Px/Ps, Yp = Y - S*Py/Ps;
         swxp += W*Xp;  swxp2 += W*Xp*Xp;  swyp += W*Yp;  swyp2 += W*Yp*Yp;
      }
      if      (S < 0) { swn += W;  swsn += W*S;  swsn2 += W*S*S; }
      else if (S > 0) { swp += W;  swsp += W*S;  swsp2 += W*S*S; }
      int g = (int)(tok[1] + 0.5);
      if (g < 0) g = 0;  if (g >= NG) g = NG - 1;
      genW[g] += W;  genWE[g] += W*E;  ++genN[g];
   }
   fin.close();

   auto rms0 = [](long double s2, long double w)->double
      { return w > 0 ? (double)sqrtl(s2/w) : 0.0; };
   auto sig  = [](long double s1, long double s2, long double w)->double {
      if (w <= 0) return 0.0;
      long double m = s1/w, v = s2/w - m*m;
      return v > 0 ? (double)sqrtl(v) : 0.0; };

   printf("\n=================  LEDGER (all species)  =================\n");
   const char* nm[4] = {"", "photon  ", "electron", "positron"};
   for (int k = 1; k <= 3; ++k)
      printf(" K=%d %s : sumW = %.6Le   macros = %lld   E=[%.3e, %.3e] eV\n",
             k, nm[k], ledW[k], ledN[k], ledElo[k], ledEhi[k]);
   printf(" net charge (e- minus e+)  : %.6Le   [compare to an = 6.3e9]\n",
          ledW[2] - ledW[3]);
   printf(" pair-consistency (e-)-(e+)-an should be ~0 if both members logged\n");

   printf("\n=========  SELECTED: K=%d, E=[%.4g,%.4g] eV  =========\n",
          species, Emin, Emax);
   printf(" entries %lld   sumW = %.6Le\n", nSel, sw);
   printf(" log plane : sigX = %.4e  sigY = %.4e  (rms0: %.4e, %.4e) m\n",
          sig(swx,swx2,sw), sig(swy,swy2,sw), rms0(swx2,sw), rms0(swy2,sw));
   printf(" at S=0    : sigX = %.4e  sigY = %.4e  (rms0: %.4e, %.4e) m\n",
          sig(swxp,swxp2,sw), sig(swyp,swyp2,sw), rms0(swxp2,sw), rms0(swyp2,sw));
   printf(" S<0: sumW = %.4Le  mean = %.4e  sig = %.4e  rms0 = %.4e m\n",
          swn, swn>0?(double)(swsn/swn):0., sig(swsn,swsn2,swn), rms0(swsn2,swn));
   printf(" S>0: sumW = %.4Le  mean = %.4e  sig = %.4e  rms0 = %.4e m\n",
          swp, swp>0?(double)(swsp/swp):0., sig(swsp,swsp2,swp), rms0(swsp2,swp));

   printf("\n GEN      N_macro          sumW        <E> [GeV]\n");
   for (int g = 0; g < NG; ++g) if (genN[g] > 0)
      printf(" %3d  %10lld  %14.6Le  %12.4e\n",
             g, genN[g], genW[g], (double)(genWE[g]/genW[g])/1e9);
   printf("===========================================================\n\n");
}
