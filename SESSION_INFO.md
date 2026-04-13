# R session information

To ensure long-term reproducibility, paste the output of `sessionInfo()` here after a successful run of the replication script.

```
# Paste sessionInfo() output here, e.g.:
#
# R version 4.3.2 (2023-10-31)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 22.04.4 LTS
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] scales_1.3.0    ggridges_0.5.6  grf_2.3.2       modelsummary_1.4.5
# [5] stargazer_5.2.3 fastDummies_1.7.3 fBasics_4032.96 reshape2_1.4.4
# ...
```

For stricter reproducibility, consider using `renv`:

```r
install.packages("renv")
renv::init()
# run the script once to capture dependencies
renv::snapshot()
```

This creates a `renv.lock` file that pins exact package versions.
