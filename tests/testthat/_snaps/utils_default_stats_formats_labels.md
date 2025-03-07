# get_stats works as expected for defaults

    Code
      res
    Output
      [1] "count"                   "count_fraction"         
      [3] "count_fraction_fixed_dp" "fraction"               

---

    Code
      res
    Output
      [1] "unique"       "nonunique"    "unique_count"

---

    Code
      res
    Output
      [1] "n"                       "count"                  
      [3] "count_fraction"          "count_fraction_fixed_dp"
      [5] "fraction"                "n_blq"                  

---

    Code
      res
    Output
       [1] "n"               "sum"             "mean"            "sd"             
       [5] "se"              "mean_sd"         "mean_se"         "mean_ci"        
       [9] "mean_sei"        "mean_sdi"        "mean_pval"       "median"         
      [13] "mad"             "median_ci"       "quantiles"       "iqr"            
      [17] "range"           "min"             "max"             "median_range"   
      [21] "cv"              "geom_mean"       "geom_mean_ci"    "geom_cv"        
      [25] "median_ci_3d"    "mean_ci_3d"      "geom_mean_ci_3d"

# get_labels_from_stats works as expected

    Code
      res
    Output
                          count            count_fraction   count_fraction_fixed_dp 
                        "count"          "count_fraction" "count_fraction_fixed_dp" 
                       fraction 
                     "fraction" 

# get_indents_from_stats works as expected

    Code
      res
    Output
                        count          count_fraction count_fraction_fixed_dp 
                            0                       0                       0 
                     fraction 
                            0 

# labels_use_control works as expected

    Code
      res
    Output
                               mean_ci                        mean_pval 
                         "Mean 34% CI" "Mean p-value (H0: mean = 0.47)" 
                             median_ci                        quantiles 
                       "Median 34% CI"                "24% and 86%-ile" 
                          geom_mean_ci 
               "Geometric Mean 34% CI" 

---

    Code
      res
    Output
                               mean_ci                        mean_pval 
                             "mean ci" "Mean p-value (H0: mean = 0.47)" 
                             median_ci                        quantiles 
                       "Median 34% CI"                   "my quantiles" 
                          geom_mean_ci 
               "Geometric Mean 34% CI" 

# summary_formats works as expected

    Code
      res
    Output
                            n                     sum                    mean 
                        "xx."                  "xx.x"                  "xx.x" 
                           sd                      se                 mean_sd 
                       "xx.x"                  "xx.x"           "xx.x (xx.x)" 
                      mean_se                 mean_ci                mean_sei 
                "xx.x (xx.x)"        "(xx.xx, xx.xx)"        "(xx.xx, xx.xx)" 
                     mean_sdi               mean_pval                  median 
             "(xx.xx, xx.xx)"    "x.xxxx | (<0.0001)"                  "xx.x" 
                          mad               median_ci               quantiles 
                       "xx.x"        "(xx.xx, xx.xx)"           "xx.x - xx.x" 
                          iqr                   range                     min 
                       "xx.x"           "xx.x - xx.x"                  "xx.x" 
                          max            median_range                      cv 
                       "xx.x"    "xx.x (xx.x - xx.x)"                  "xx.x" 
                    geom_mean            geom_mean_ci                 geom_cv 
                       "xx.x"        "(xx.xx, xx.xx)"                  "xx.x" 
                 median_ci_3d              mean_ci_3d         geom_mean_ci_3d 
      "xx.xx (xx.xx - xx.xx)" "xx.xx (xx.xx - xx.xx)" "xx.xx (xx.xx - xx.xx)" 

# summary_labels works as expected

    Code
      res
    Output
                                  n                           sum 
                                "n"                         "Sum" 
                               mean                            sd 
                             "Mean"                          "SD" 
                                 se                       mean_sd 
                               "SE"                   "Mean (SD)" 
                            mean_se                       mean_ci 
                        "Mean (SE)"                 "Mean 95% CI" 
                           mean_sei                      mean_sdi 
                    "Mean -/+ 1xSE"               "Mean -/+ 1xSD" 
                          mean_pval                        median 
      "Mean p-value (H0: mean = 0)"                      "Median" 
                                mad                     median_ci 
        "Median Absolute Deviation"               "Median 95% CI" 
                          quantiles                           iqr 
                  "25% and 75%-ile"                         "IQR" 
                              range                           min 
                        "Min - Max"                     "Minimum" 
                                max                  median_range 
                          "Maximum"          "Median (Min - Max)" 
                                 cv                     geom_mean 
                           "CV (%)"              "Geometric Mean" 
                       geom_mean_ci                       geom_cv 
            "Geometric Mean 95% CI"         "CV % Geometric Mean" 
                       median_ci_3d                    mean_ci_3d 
                  "Median (95% CI)"               "Mean (95% CI)" 
                    geom_mean_ci_3d 
          "Geometric Mean (95% CI)" 

---

    Code
      res
    Output
                                 n                        count 
                               "n"                      "count" 
                    count_fraction      count_fraction_fixed_dp 
                  "count_fraction"    "count_fraction_fixed_dp" 
                          fraction                        n_blq 
                        "fraction"                      "n_blq" 
                       pval_counts 
      "p-value (chi-squared test)" 

