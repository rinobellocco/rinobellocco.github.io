---
layout: default
permalink: /software/dosresmeta
---

The dosresmeta R package: Performing multivariate dose-response meta-analysis
========

The `dosresmeta` package is a free and open-source add-on for conducting (multivariate) dose-response meta-analysis with the statistical software environment [R](https://www.r-project.org/).
You can install the package (or upgrade to the newest version) from [CRAN](https://cran.r-project.org/web/packages/dosresmeta/index.html) directly within R by typing:

`install.packages("dosresmeta")`

Extensions to meta-regression models and alternative estimation procedures are included in an update version still under developement, which is avaiable on [GitHub](https://github.com/alecri/dosresmeta) by typing:

`install.packages("devtools")`

`devtools::install_github("alecri/dosresmeta")`

&nbsp;

 Reproducible code


* [*GitHub repository* for Discacciati, A., **Crippa, A.**, & Orsini N.  “Goodness-of-fit tools for dose-response meta-analysis of binary outcomes" 2015. JRSM](https://github.com/anddis/goodness-of-fit-meta-analysis)


 Analysis Examples

The links below demonstrate how the models, methods, and techniques described in the respective articles/chapters can be applied via the `dosresmeta` package. The examples are run using dosresmeta version 2.0.0 avaiable on GitHub.

### **Dose-response model for a single study according to different study designs**

* [Orsini, N., Bellocco, R., & Greenland, S. (2006). Generalized least squares for trend estimation of summarized dose-response data. Stata Journal, 6(1), 40.](http://rpubs.com/alecri/glst)

### **Modeling non-linear dose-response relations**

* [Liu, Qin, et al. "A two-stage hierarchical regression model for meta-analysis of epidemiologic nonlinear dose–response data." Computational Statistics & Data Analysis 53.12 (2009): 4157-4167.](http://rpubs.com/alecri/qinliu)

&nbsp;


**Additional (useful) code**

* [Missing number of cases/n. Assuming independence](/downloads/codes/missing cases n.txt)


**Older R script in txt format based on dosresmeta version 1.3.1 available on CRAN.**

* [Coffee consumption and all causes mortality](/downloads/codes/coffee_mort.txt)
* [Alcohol intake and colorectal cancer](/downloads/codes/alcohol_crc.txt)
* [Alcohol intake and lung cancer](/downloads/codes/alcohol_lc.txt)
* [Body mass index and renal cancer](/downloads/codes/ts_glst_bmi_ex.txt)
* [Alcohol intake and cardiovascular risk](/downloads/codes/ts_glst_alcohol_ex.txt)