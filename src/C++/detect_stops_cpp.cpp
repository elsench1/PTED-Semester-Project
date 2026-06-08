#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;


// Median mit na.rm = TRUE
double median_na(std::vector<double> v) {
  std::vector<double> clean;
  clean.reserve(v.size());
  
  for (double val : v) {
    if (!R_IsNA(val) && !std::isnan(val)) {
      clean.push_back(val);
    }
  }
  
  int n = clean.size();
  
  if (n == 0) {
    return NA_REAL;
  }
  
  std::sort(clean.begin(), clean.end());
  
  if (n % 2 == 1) {
    return clean[n / 2];
  } else {
    return 0.5 * (clean[n / 2 - 1] + clean[n / 2]);
  }
}


// Quantile wie R: quantile(..., type = 7), mit na.rm = TRUE
double quantile_type7_na(std::vector<double> v, double p) {
  std::vector<double> clean;
  clean.reserve(v.size());
  
  for (double val : v) {
    if (!R_IsNA(val) && !std::isnan(val)) {
      clean.push_back(val);
    }
  }
  
  int n = clean.size();
  
  if (n == 0) {
    return NA_REAL;
  }
  
  if (n == 1) {
    return clean[0];
  }
  
  std::sort(clean.begin(), clean.end());
  
  double h = 1.0 + (n - 1) * p;
  int hf = std::floor(h);
  double frac = h - hf;
  
  int i = hf - 1; // C++ ist 0-basiert
  
  if (i < 0) {
    return clean[0];
  }
  
  if (i >= n - 1) {
    return clean[n - 1];
  }
  
  return clean[i] + frac * (clean[i + 1] - clean[i]);
}


// Hauptfunktion
// Erwartet:
// time_sec: Zeit als numerischer Vektor in Sekunden
// x:        x-Koordinaten, z. B. Meter-Koordinaten
// y:        y-Koordinaten, z. B. Meter-Koordinaten
//
// Wichtig:
// Die Daten müssen nach time_sec sortiert sein.
//
// [[Rcpp::export]]
LogicalVector detect_stops_cpp(NumericVector time_sec,
                               NumericVector x,
                               NumericVector y,
                               double min_stop_time = 300.0,
                               double stop_radius_m = 50.0) {
  
  int n = time_sec.size();
  
  if (x.size() != n || y.size() != n) {
    stop("time_sec, x und y muessen gleich lang sein.");
  }
  
  LogicalVector is_stop(n, false);
  
  int end_idx = 0;
  
  for (int i = 0; i < n; i++) {
    
    if (R_IsNA(time_sec[i]) || std::isnan(time_sec[i])) {
      continue;
    }
    
    double t0 = time_sec[i];
    double t_end = t0 + min_stop_time;
    
    if (end_idx < i) {
      end_idx = i;
    }
    
    while (end_idx + 1 < n &&
           !R_IsNA(time_sec[end_idx + 1]) &&
           !std::isnan(time_sec[end_idx + 1]) &&
           time_sec[end_idx + 1] <= t_end) {
      end_idx++;
    }
    
    int len = end_idx - i + 1;
    
    if (len >= 2) {
      
      std::vector<double> xs;
      std::vector<double> ys;
      
      xs.reserve(len);
      ys.reserve(len);
      
      for (int k = i; k <= end_idx; k++) {
        xs.push_back(x[k]);
        ys.push_back(y[k]);
      }
      
      double cx = median_na(xs);
      double cy = median_na(ys);
      
      if (!R_IsNA(cx) && !R_IsNA(cy) &&
          !std::isnan(cx) && !std::isnan(cy)) {
        
        std::vector<double> d;
        d.reserve(len);
        
        for (int k = i; k <= end_idx; k++) {
          
          if (!R_IsNA(x[k]) && !R_IsNA(y[k]) &&
              !std::isnan(x[k]) && !std::isnan(y[k])) {
            
            double dx = x[k] - cx;
            double dy = y[k] - cy;
            double dist = std::sqrt(dx * dx + dy * dy);
            
            d.push_back(dist);
          }
        }
        
        double q90 = quantile_type7_na(d, 0.9);
        
        if (!R_IsNA(q90) && !std::isnan(q90) && q90 <= stop_radius_m) {
          for (int k = i; k <= end_idx; k++) {
            is_stop[k] = true;
          }
        }
      }
    }
  }
  
  return is_stop;
}