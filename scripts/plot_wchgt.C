// plot_wchgt.C
// Parse CAIN HiggsIP logs for "bbkick0 ... maxdet= T" record lines and plot
//   (1) record-cell average macro weight |wchgt|/mpnn vs time step it
//   (2) record fldabs vs it
// per (l2,k) channel, e- and e+ separately.
//
// Usage:
//   root -l -b -q 'plot_wchgt.C'                         // all three runs
//   root -l -b -q 'plot_wchgt.C("path/to/log.txt","v08770")'   // one log
//
// Output: <tag>_wchgt.png, <tag>_maxdet.csv, summary table on stdout.

#include "TCanvas.h"
#include "TGraph.h"
#include "TMultiGraph.h"
#include "TLegend.h"
#include "TLine.h"
#include "TLatex.h"
#include "TAxis.h"
#include "TString.h"
#include <fstream>
#include <string>
#include <vector>
#include <cstdio>
#include <cctype>
#include <cmath>

namespace {

const double WGT0 = 1.20987654e4;   // primary macro weight (higgs.txt)

// Fortran D-exponent -> E, in a short numeric substring only
void dtoe(std::string& s) {
  for (size_t i = 1; i + 1 < s.size(); ++i)
    if ((s[i]=='D' || s[i]=='d') &&
        (isdigit((unsigned char)s[i-1]) || s[i-1]=='.') &&
        (s[i+1]=='+' || s[i+1]=='-' || isdigit((unsigned char)s[i+1])))
      s[i] = 'E';
}

// scan n (1 or 2) doubles following 'tag'; handles "=6737  1.5E9",
// "= 72799 -2.100E+11", packed negatives, D exponents
bool scanAfter(const std::string& line, const char* tag, int n,
               double* out, size_t from = 0) {
  size_t p = line.find(tag, from);
  if (p == std::string::npos) return false;
  std::string s = line.substr(p + strlen(tag), 64);
  dtoe(s);
  if (n == 1) return sscanf(s.c_str(), "%lf", &out[0]) == 1;
  return sscanf(s.c_str(), "%lf %lf", &out[0], &out[1]) == 2;
}

struct Rec {
  long it, is, l2, k;
  double mpnn_e, wchgt_e, mpnn_p, wchgt_p, fldabs;
};

bool parseLine(const std::string& line, Rec& r) {
  if (line.find("bbkick0") == std::string::npos) return false;
  // keep only new-record lines: maxdet= T (tolerate spacing)
  size_t pm = line.find("maxdet=");
  if (pm == std::string::npos) return false;
  size_t q = pm + 7;
  while (q < line.size() && isspace((unsigned char)line[q])) ++q;
  if (q >= line.size() || line[q] != 'T') return false;

  double v[2];
  if (!scanAfter(line, "it,is=", 2, v)) return false;
  r.it = (long)v[0];  r.is = (long)v[1];
  if (!scanAfter(line, "l2=", 1, v)) return false;
  r.l2 = (long)v[0];
  if (!scanAfter(line, " k=", 1, v)) return false;
  r.k  = (long)v[0];
  if (!scanAfter(line, "e- mpnn,wchgt=", 2, v)) return false;
  r.mpnn_e = v[0];  r.wchgt_e = v[1];
  if (!scanAfter(line, "e+ mpnn,wchgt=", 2, v)) return false;
  r.mpnn_p = v[0];  r.wchgt_p = v[1];
  if (!scanAfter(line, "fldabs=", 1, v)) return false;
  r.fldabs = v[0];
  return (r.l2 >= 1 && r.l2 <= 2 && r.k >= 1 && r.k <= 2);
}

void hline(double y, int color, int style, double x1, double x2,
           const char* lab) {
  TLine* L = new TLine(x1, y, x2, y);
  L->SetLineColor(color); L->SetLineStyle(style); L->Draw();
  TLatex* t = new TLatex(x2, y, lab);
  t->SetTextSize(0.028); t->SetTextColor(color);
  t->SetTextAlign(12); t->Draw();
}

} // namespace

void plot_wchgt(const char* logfile, const char* tag = "run") {
  std::ifstream fin(logfile);
  if (!fin) { printf("ERROR: cannot open %s\n", logfile); return; }

  std::vector<Rec> recs;
  std::string line;
  while (std::getline(fin, line)) {
    Rec r;
    if (parseLine(line, r)) recs.push_back(r);
  }
  printf("\n===== %s : %zu maxdet=T records =====\n", tag, recs.size());
  if (recs.empty()) return;

  // CSV dump
  FILE* csv = fopen(Form("%s_maxdet.csv", tag), "w");
  fprintf(csv, "it,is,l2,k,mpnn_e,wchgt_e,avgw_e,mpnn_p,wchgt_p,avgw_p,fldabs\n");

  // graphs: channel idx = (l2-1)*2 + (k-1); e- and e+ separately, + fldabs
  TGraph *ge[4], *gp[4], *gf[4];
  int col[4] = {kBlue+1, kRed+1, kGreen+2, kMagenta+1};
  for (int i = 0; i < 4; ++i) {
    ge[i] = new TGraph(); ge[i]->SetMarkerStyle(20);
    ge[i]->SetMarkerColor(col[i]); ge[i]->SetMarkerSize(0.6);
    gp[i] = new TGraph(); gp[i]->SetMarkerStyle(24);
    gp[i]->SetMarkerColor(col[i]); gp[i]->SetMarkerSize(0.6);
    gf[i] = new TGraph(); gf[i]->SetMarkerStyle(20);
    gf[i]->SetMarkerColor(col[i]); gf[i]->SetMarkerSize(0.6);
  }

  long itmin = 1e9, itmax = -1;
  long onset2x[4]; double wmax[4];
  for (int i = 0; i < 4; ++i) { onset2x[i] = -1; wmax[i] = 0; }

  for (const auto& r : recs) {
    int i = (int)((r.l2 - 1) * 2 + (r.k - 1));
    double awe = (r.mpnn_e > 0) ? fabs(r.wchgt_e) / r.mpnn_e : 0;
    double awp = (r.mpnn_p > 0) ? fabs(r.wchgt_p) / r.mpnn_p : 0;
    if (awe > 0) ge[i]->SetPoint(ge[i]->GetN(), r.it, awe);
    if (awp > 0) gp[i]->SetPoint(gp[i]->GetN(), r.it, awp);
    gf[i]->SetPoint(gf[i]->GetN(), r.it, r.fldabs);
    double w = TMath::Max(awe, awp);
    if (w > wmax[i]) wmax[i] = w;
    if (onset2x[i] < 0 && w > 2.0 * WGT0) onset2x[i] = r.it;
    if (r.it < itmin) itmin = r.it;
    if (r.it > itmax) itmax = r.it;
    fprintf(csv, "%ld,%ld,%ld,%ld,%.0f,%.4e,%.4e,%.0f,%.4e,%.4e,%.4e\n",
            r.it, r.is, r.l2, r.k, r.mpnn_e, r.wchgt_e, awe,
            r.mpnn_p, r.wchgt_p, awp, r.fldabs);
  }
  fclose(csv);

  const char* chn[4] = {"l2=1,k=1", "l2=1,k=2", "l2=2,k=1", "l2=2,k=2"};
  printf("%-10s %12s %16s\n", "channel", "max<w>", "first it >2*Wgt0");
  for (int i = 0; i < 4; ++i)
    printf("%-10s %12.3e %16ld\n", chn[i], wmax[i], onset2x[i]);

  TCanvas* c = new TCanvas(Form("c_%s", tag), tag, 1100, 900);
  c->Divide(1, 2);

  // ---- pad 1: average record-cell weight ----
  c->cd(1); gPad->SetLogy(); gPad->SetGridx();
  TMultiGraph* mgw = new TMultiGraph();
  for (int i = 0; i < 4; ++i) {
    if (ge[i]->GetN()) mgw->Add(ge[i], "P");
    if (gp[i]->GetN()) mgw->Add(gp[i], "P");
  }
  mgw->SetTitle(Form("%s: record-cell <weight> = |wchgt|/mpnn;"
                     "time step it;<weight> per macro", tag));
  mgw->Draw("A");
  mgw->GetXaxis()->SetLimits(itmin - 100, itmax + 400);
  mgw->SetMinimum(0.3 * WGT0);
  double x1 = itmin - 100, x2 = itmax + 400;
  hline(WGT0,       kGray+2, 2, x1, x2, " Wgt0");
  hline(1e5,        kRed,    2, x1, x2, " tripwire 1e5");
  hline(10 * WGT0,  kGray+1, 3, x1, x2, " gen1 (x10)");
  hline(100 * WGT0, kGray+1, 3, x1, x2, " gen2 (x100)");
  hline(1000 * WGT0,kGray+1, 3, x1, x2, " gen3 (x1000)");
  TLegend* lg = new TLegend(0.13, 0.55, 0.38, 0.88);
  lg->SetBorderSize(0); lg->SetFillStyle(0);
  for (int i = 0; i < 4; ++i) lg->AddEntry(ge[i], Form("e-  %s", chn[i]), "p");
  for (int i = 0; i < 4; ++i) lg->AddEntry(gp[i], Form("e+  %s", chn[i]), "p");
  lg->Draw();

  // ---- pad 2: fldabs ratchet ----
  c->cd(2); gPad->SetLogy(); gPad->SetGridx();
  TMultiGraph* mgf = new TMultiGraph();
  for (int i = 0; i < 4; ++i) if (gf[i]->GetN()) mgf->Add(gf[i], "P");
  mgf->SetTitle(Form("%s: record fldabs;time step it;fldabs (V/m)", tag));
  mgf->Draw("A");
  mgf->GetXaxis()->SetLimits(x1, x2);
  hline(1.0e16,   kGray+2, 2, x1, x2, " ~physical pinch");
  hline(1.323e18, kRed,    2, x1, x2, " Schwinger");

  c->SaveAs(Form("%s_wchgt.png", tag));
}

// no-argument entry: process all three runs with the standard layout
void plot_wchgt() {
  const char* runs[3] = {"v08770", "v08771", "v08772"};
  for (int i = 0; i < 3; ++i) {
    TString f = Form("%s_BW_BH_NPH3_x1000_po0p90_lpc+/"
                     "log_HiggsIP_%s_BW_BH_NPH3_x1000_po0p90_lpc+.txt",
                     runs[i], runs[i]);
    plot_wchgt(f.Data(), runs[i]);
  }
}
