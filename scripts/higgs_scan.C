// ============================================================================
// higgs_scan.C
//
// Scan a CAIN particle dump (higgs.dat) and report, for a chosen species
// (K = 1 photon, 2 electron, 3 positron) and energy window [Emin, Emax] (eV):
//
//   (1) weighted number of entries (sum of Weight) + raw entry count
//   (2) weighted RMS of X   (both about 0 and about the weighted mean)
//   (3) weighted RMS of Y   (both about 0 and about the weighted mean)
//   (4) weighted RMS of S for S < 0 (about 0 and about mean; mean also printed)
//   (5) weighted RMS of S for S > 0 (about 0 and about mean; mean also printed)
//
// File format notes:
//   * Header line begins with '!': skipped (any line starting with '!' is).
//   * Numbers use Fortran D-exponents (1.23D+04) -> converted to E on the fly.
//   * Header lists 15 columns (K GEN NAME Weight T X Y S E Px Py Ps Sx Sy Ss)
//     but data rows may have 14 tokens (NAME blank). Both cases handled.
//
// Usage examples:
//   root -l -q 'higgs_scan.C("higgs.dat", 1)'                       // all photons
//   root -l -q 'higgs_scan.C("higgs.dat", 1, 4.9e12, 5.1e12)'       // ~5 TeV photons
//   root -l -q 'higgs_scan.C("higgs.dat", 2, 0.,   1e15)'           // all electrons
//   root -l -q 'higgs_scan.C("higgs.dat", 3)'                       // all positrons
//
// Optional 5th argument makeHists=true writes auto-ranged diagnostic
// histograms (X, Y, S<0, S>0, E) to higgs_scan_K<species>.root.
// ============================================================================

#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include "TFile.h"
#include "TH1D.h"

void higgs_scan(const char* fname = "higgs.dat",
                int    species  = 1,
                double Emin     = 0.0,      // eV
                double Emax     = 1.0e99,   // eV
                bool   makeHists = true)
{
   std::ifstream fin(fname);
   if (!fin.is_open()) {
      printf("ERROR: cannot open %s\n", fname);
      return;
   }

   // --- weighted accumulators (long double for 1e6+ line sums) --------------
   long double sw = 0.0L;                       // sum of weights (selected)
   long double swx = 0.0L,  swx2 = 0.0L;        // X moments
   long double swy = 0.0L,  swy2 = 0.0L;        // Y moments
   long double swn = 0.0L,  swsn = 0.0L, swsn2 = 0.0L;  // S < 0
   long double swp = 0.0L,  swsp = 0.0L, swsp2 = 0.0L;  // S > 0
   long long   nSel = 0, nLines = 0, nBad = 0;
   long long   nSneg = 0, nSpos = 0, nSzero = 0;

   // --- optional buffers for auto-ranged histograms --------------------------
   std::vector<float> vx, vy, vsn, vsp, ve;

   std::string line;
   std::vector<double> tok;
   tok.reserve(16);

   while (std::getline(fin, line)) {
      ++nLines;
      if (nLines % 5000000 == 0)
         printf("  ... %lld lines read, %lld selected\n", nLines, nSel);

      if (line.empty()) continue;
      // find first non-blank character
      size_t p0 = line.find_first_not_of(" \t\r");
      if (p0 == std::string::npos) continue;
      if (line[p0] == '!') continue;                    // header/comment

      // Fortran D-exponent -> C E-exponent
      for (size_t i = 0; i < line.size(); ++i)
         if (line[i] == 'D' || line[i] == 'd') line[i] = 'E';

      // tokenize into doubles (non-numeric tokens, e.g. a NAME string, skipped)
      tok.clear();
      {
         const char* p = line.c_str();
         char* end = nullptr;
         while (*p) {
            while (*p == ' ' || *p == '\t') ++p;
            if (!*p) break;
            double v = strtod(p, &end);
            if (end == p) {                 // non-numeric token: skip word
               while (*p && *p != ' ' && *p != '\t') ++p;
            } else {
               tok.push_back(v);
               p = end;
            }
         }
      }

      // Expect 14 numeric tokens: K GEN Weight T X Y S E Px Py Ps Sx Sy Ss
      // (NAME, if present and numeric-free, was skipped above)
      if (tok.size() < 8) { ++nBad; continue; }

      const int    K = (int)(tok[0] + 0.5);
      if (K != species) continue;

      const double W = tok[2];
      const double X = tok[4];
      const double Y = tok[5];
      const double S = tok[6];
      const double E = tok[7];

      if (E < Emin || E > Emax) continue;

      // --- accumulate -------------------------------------------------------
      ++nSel;
      sw   += W;
      swx  += W * X;   swx2 += W * X * X;
      swy  += W * Y;   swy2 += W * Y * Y;

      if (S < 0.0) { ++nSneg; swn += W; swsn += W * S; swsn2 += W * S * S; }
      else if (S > 0.0) { ++nSpos; swp += W; swsp += W * S; swsp2 += W * S * S; }
      else ++nSzero;

      if (makeHists) {
         vx.push_back((float)X);
         vy.push_back((float)Y);
         ve.push_back((float)E);
         if (S < 0.0) vsn.push_back((float)S);
         else if (S > 0.0) vsp.push_back((float)S);
      }
   }
   fin.close();

   // --- statistics helpers ---------------------------------------------------
   auto rms0  = [](long double s2, long double w) -> double {
      return (w > 0.0L) ? (double)sqrtl(s2 / w) : 0.0;
   };
   auto sigma = [](long double s1, long double s2, long double w) -> double {
      if (w <= 0.0L) return 0.0;
      long double m = s1 / w;
      long double v = s2 / w - m * m;
      return (v > 0.0L) ? (double)sqrtl(v) : 0.0;
   };
   auto mean  = [](long double s1, long double w) -> double {
      return (w > 0.0L) ? (double)(s1 / w) : 0.0;
   };

   // --- report ---------------------------------------------------------------
   const char* sname = (species == 1) ? "photon" :
                       (species == 2) ? "electron" :
                       (species == 3) ? "positron" : "unknown";

   printf("\n================================================================\n");
   printf(" higgs_scan  |  file: %s\n", fname);
   printf(" species K = %d (%s),  E in [%.4g, %.4g] eV\n",
          species, sname, Emin, Emax);
   printf("----------------------------------------------------------------\n");
   printf(" lines read                 : %lld  (bad/short: %lld)\n", nLines, nBad);
   printf(" entries selected           : %lld\n", nSel);
   printf(" (1) weighted N  (sum W)    : %.8Le\n", sw);
   printf("----------------------------------------------------------------\n");
   printf(" (2) X: rms about 0         : %.6e m\n", rms0(swx2, sw));
   printf("        mean                : %.6e m\n", mean(swx, sw));
   printf("        rms about mean      : %.6e m\n", sigma(swx, swx2, sw));
   printf(" (3) Y: rms about 0         : %.6e m\n", rms0(swy2, sw));
   printf("        mean                : %.6e m\n", mean(swy, sw));
   printf("        rms about mean      : %.6e m\n", sigma(swy, swy2, sw));
   printf("----------------------------------------------------------------\n");
   printf(" (4) S < 0  entries         : %lld   (weighted: %.6Le)\n", nSneg, swn);
   printf("        rms about 0         : %.6e m\n", rms0(swsn2, swn));
   printf("        mean                : %.6e m\n", mean(swsn, swn));
   printf("        rms about mean      : %.6e m\n", sigma(swsn, swsn2, swn));
   printf(" (5) S > 0  entries         : %lld   (weighted: %.6Le)\n", nSpos, swp);
   printf("        rms about 0         : %.6e m\n", rms0(swsp2, swp));
   printf("        mean                : %.6e m\n", mean(swsp, swp));
   printf("        rms about mean      : %.6e m\n", sigma(swsp, swsp2, swp));
   if (nSzero) printf(" (S == 0 exactly: %lld entries, in neither RMS)\n", nSzero);
   printf("================================================================\n\n");

   // --- optional diagnostic histograms ---------------------------------------
   if (makeHists && nSel > 0) {
      auto book = [](const char* n, const char* t, std::vector<float>& v,
                     double lo, double hi) -> TH1D* {
         TH1D* h = new TH1D(n, t, 200, lo, hi);
         for (float x : v) h->Fill(x);
         return h;
      };
      auto range = [&](std::vector<float>& v, double& lo, double& hi) {
         lo = 1e99; hi = -1e99;
         for (float x : v) { if (x < lo) lo = x; if (x > hi) hi = x; }
         if (lo >= hi) { lo -= 1.0; hi += 1.0; }
      };
      TString ofn = TString::Format("higgs_scan_K%d.root", species);
      TFile fout(ofn, "RECREATE");
      double lo, hi;
      range(vx, lo, hi); book("hX", "X (m);X (m);weighted-less entries", vx, lo, hi)->Write();
      range(vy, lo, hi); book("hY", "Y (m);Y (m);entries",  vy, lo, hi)->Write();
      if (!vsn.empty()) { range(vsn, lo, hi); book("hSneg", "S<0 (m);S (m);entries", vsn, lo, hi)->Write(); }
      if (!vsp.empty()) { range(vsp, lo, hi); book("hSpos", "S>0 (m);S (m);entries", vsp, lo, hi)->Write(); }
      range(ve, lo, hi); book("hE", "E (eV);E (eV);entries", ve, lo, hi)->Write();
      fout.Close();
      printf(" diagnostic histograms written to %s\n\n", ofn.Data());
   }
}
