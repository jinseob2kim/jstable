# jstable 1.3.19
* Fix: `cox2.display` now properly handles multi-state models automatically without the `msm` parameter.
* Fix: When `event_msm` is applied,`cox2.display` only returns the output with the selected variables.

# jstable 1.3.18
* Fix: `TableSubgroupMultiCox` now correctly extracts p-values from survey Cox models (svycoxph) by using the last column index instead of hardcoded column 5
* Fix: `cox2.display` now correctly handles `strata()` terms in `coxph` models

# jstable 1.3.17
* Fix: `cox2.display` now properly handles variables with no variation in complete cases during univariate analysis
* Fix: `cox2.display` correctly filters out NA p-values when using `pcut.univariate` option
* Fix: `cox2.display` now properly handles data.table objects with `data_for_univariate` parameter
* Fix: `cox2.display` shows adjusted HR properly and also handles clustered coxph model properly. 
* Fix: When `pcut.univariate` is applied, `cox2.display` now correctly shows metrics (N, AIC, C-index, Events) from the selected model with significant variables
* Fix: Added missing `@importFrom survival Surv` for proper NAMESPACE generation
* Fix: `glmshow.display` now properly handles interaction terms with `pcut.univariate` option
* Fix: `glmshow.display` correctly maintains variable order when selecting significant variables with interaction terms
* Fix: `glmshow.display` now displays proper reference levels for interaction terms with multi-level factors (e.g., "wt:cyl: ref.=4")
* Fix: `geeglm.display` now correctly uses `data_for_univariate` with `pcut.univariate` to refit model with selected variables only, updating N accordingly
* Fix: `lmer.display` now correctly uses `data_for_univariate` with `pcut.univariate` to refit model with selected variables only, updating N and other metrics accordingly
* Update: `LabeljsTable` now supports interaction terms, applying labels to both main effects and interaction coefficients

# jstable 1.3.16
* Update: Now in `CreateTableOneJS` and `svyCreateTableOneJS`, column names are more descriptive when using `psub = T` with `strata`, `strata2`.

# jstable 1.3.15
* Update: Add `data_for_univariate` in `cox2.display`, `geeglm.display`, `lmer.display`, crude p-values in univariate tables are now computed directly from the raw data passed via `data_for_univariate`.

# jstable 1.3.14
* Fix: error in `cox2.display` when all status 0
* Update: Add testcode of `cox2.display` when all status 0 (isList, column&row name diff check)

# jstable 1.3.13
* Fix: error in handling `id` and `weight` columns from input data in the Cox module.

# jstable 1.3.12
* Update: Add count_by and event variable to TableSubgroupGLM, TableSubgroupMultiGLM 
* Update: Collapse columns when there are more than 3 levels in TalbeSubgroupCox, TableSubgroupMultiCox

# jstable 1.3.11
* Update: Add pcut.univariate to geeglm.display, coxme.display, cox2.display, glmshow.display, lmer.display, svyglm.display, and svycox.display to allow multivariable analysis only with statistically significant variables.
* Update: Add n_original to svyCreateTableOneJS to display the original sample size (unweighted n) in addition to the weighted sample size.
* Update: Add Anderson-Darling normality test for sample sizes ≥ 5000.
* Fix: error in TableSubgroupMultiGLM when family = "gaussian" and data = data.design with multi-level categorical subgroup variables (3 or more levels)

# jstable 1.3.10
* Enable TableSubgroupCox to handle with mixed effect model, automatically detects mixed model with formula

# jstable 1.3.9
* Update: Add C-Index(se) in cox2.display

# jstable 1.3.8
* Update: Detect mixed model and show corresponding values in  'TableSubgroupGLM', 'TableSubgroupMultiGLM'

# jstable 1.3.7
* fix: pairwise options in CreateTableOne2, CreateTableOneJS, svyCreateTableOne2, svyCreateTableOneJS

# jstable 1.3.6
* Update: Add event, count_by option in TableSubgroupCox, TableSubgroupMultiCox
* Update: Add pairwise option in CreateTableOne2, CreateTableOneJS, svyCreateTableOne2, svyCreateTableOneJS
* Update: Add `labeldata` option in `TableSubgroupGLM`, `TableSubgroupMultiGLM`, `TableSubgroupCox`, `TableSubgroupMultiCox`
* Fix: `cox2.display` HR 

# jstable 1.3.5
* Fix: error in `TableSubgroupMultiGLM` when covariates

# jstable 1.3.4
* Fix: error in `forestcox` when categorical binary outcome
* Fix: error in `forestglm` when categorical covariates

# jstable 1.3.3
* Update: Add cox2.display available in fine-and-gray(competing risk), Multi-State Model (MSM)
* Update: Add TableSubgroupMultiCox available in fine-and-gray(competing risk)
* Fix: error in `forestcox` and `forestglm` with datatype of P value in table

# jstable 1.3.2

* Fix: error in subgroup option due to non-existent item in `forestcox`
* Fix: Factor order error in `forestcox`

# jstable 1.3.1

* Fix: `addOverall` options to  `svyCreateTableOneJS` 
* Fix: `TableSubgroupCox`

# jstable 1.3.0

* Update: Add `weights` option to `TableSubgroupCox` and `TableSubgroupMultiCox` for marginal cox model. ex: `weights = "weights"`
* Update: Add `strata` option to `TableSubgroupCox` and `TableSubgroupMultiCox` for marginal cox model. ex: `strata = "sex"`
* Fix: error in `TableSubgroupMultiCox` with cluster

# jstable 1.2.7

* Fix: Interaction P when 3 or more categorical subgroups in `TableSubgroupCox` and `TableSubgroupMultiCox`

# jstable 1.2.6

* Add AIC metric to `cox2.display`.

# jstable 1.2.5

* Update: Add `cluster` option to `TableSubgroupCox` and `TableSubgroupMultiCox` for marginal cox model. ex: `cluster = "inst"`

# jstable 1.2.4

* Fix: error for multi-category independent variable in `forestcox` and `forestglm`.

# jstable 1.2.3

* Fix: error and ref. for interaction terms in `svycox` and `svyglm`.

# jstable 1.2.2

* Fix: error and ref. for interaction terms in `lmer`.

# jstable 1.2.1

* Fix: error about interaction terms in `coxme`.

# jstable 1.2.0

* Fix: ref. for interaction terms in `cox2`.

# jstable 1.1.9

* Fix: ref. for interaction terms in `glmshow`.

# jstable 1.1.8

* Add `normalityTest` option to `CreateTableOneJS` to perform the Shapiro test for all variables.

# jstable 1.1.7

* Add family 'poisson', 'quasipoisson' in `glmshow.display` and `TableSubgroupMultiGLM`
* Add data `mort`

# jstable 1.1.6

* Bugfix `TableSubgroupGLM`: thanks for `weisx2022`

# jstable 1.1.5

* Bugfix `TableSubgroupCox`: thanks for `ciciing`

# jstable 1.1.4

* Fix: confidence interval calculation in `svyglm` ( thanks for `cyk0315`)

# jstable 1.1.3

* Add `addOverall` options to `CreateTableOneJS` and `svyCreateTableOneJS` to add overall column.

# jstable 1.1.2

* Bugfix `mk.lev`

# jstable 1.1.1

* Bugfix `LabelepiDisplay`: thanks for `thisis05`

# jstable 1.1.0

* Bugfix `TableSubgroupCox`: thanks for `Ding-yuan Wan`

# jstable 1.0.9

* Bugfix `coxme.display`: thanks for `Cristina Ganuza Vallejo`

# jstable 1.0.8

* Update: `TableSubgroupGLM` & `TableSubgroupCox` allow subgroup variable with continuous.

# jstable 1.0.7

* Update `*.display`: univariate analysis with `stats::update`

# jstable 1.0.6

* Bugfix `svyCreateTableOneJS` in example/test.

# jstable 1.0.5

* Bugfix `svycox.display`

# jstable 1.0.4

* Remove dependency with `group_split` function (**dplyr** package)

* Use `survey::regTermTest` for interaction p calculation with `survey::svycoxph`.   

# jstable 1.0.3

* Remove dependency with **car** packages


# jstable 1.0.2

* Bugfix `CreateTableOneJS`, `svyCreateTableOneJS`: when with labeldata, variables other than numeric or factor types are excluded.  


# jstable 1.0.1

* Bugfix `TableSubgroupGLM`, `TableSubgroupMultiGLM`: p value


# jstable 1.0.0

* Add `TableSubgroupGLM`, `TableSubgroupMultiGLM`: subgroup analysis for GLM(gaussian, logistic)

# jstable 0.9.8

## Bugfix

* `showpm` option with `showAllLevels = F` when no strata.

# jstable 0.9.7

## Bugfix 

* `TableSubgroupCox`: apply `extend = T` option to `summary.survfit` 

* `LabelepiDisplay`, `LabeljsTable` with only 1 independent variable.

## Update 

* `CreateTableOneJS`, `svyCreateTableOneJS`: Add `showpm` option to show normal distributed continuous variables as Mean ± SD.


# jstable 0.9.6

* Bugfix `LabelepiDisplay`, `LabeljsTable`: label error.     

# jstable 0.9.5

* Bugfix `TableSubgroupCox`: error with too large **time_eventrate**     

# jstable 0.9.4

* Bugfix `TableSubgroupCox`: error with factor variable including `NA`.

* Update `TableSubgroupCox`: compatible with upcoming survival pacakge update.

# jstable 0.9.3

* Update `svycox.display`: compatible with upcoming survival pacakge update.

# jstable 0.9.2

## Bug fix

* Column name fix: Run `CreateTableOneJS` with 2 level strata & `psub = F`.


# jstable 0.9.1

## Bug fix

* Additional bug fix: match with **survival3.1-x**.

* `class` issue: https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html

# jstable 0.9.0

* Updates & Bug fix: match with **survival3.1-x**.

# jstable 0.8.6

* Updates: Non-normal variables can be summarized with [min,max]. 

# jstable 0.8.5

* Bug fixes: Additional incorrect **P for interaction** in `TableSubgroupCox`.

# jstable 0.8.4

* Bug fixes: Incorrect **P for interaction** in `TableSubgroupCox`.

# jstable 0.8.3

* Bug fixes: Univariate analysis in `geeglm.display`.

# jstable 0.8.2

* Bug fixes: Apply label information to table 1 with strata.

# jstable 0.8.1

## Update

* `CreateTableOneJS` and `svyCreateTableOneJS` can get simplified table with **showAllLevels = F** option. 

# jstable 0.8.0

## New function

* `TableSubgroupMultiCox`: Get **sub-group analysis table** for forestplot with **Cox/svycox model**.


# jstable 0.7.10

* Update `CreateTableOneJS` and `svyCreateTableOneJS` according to [tableone](https://github.com/kaz-yos/tableone) package(0.10.0).

# jstable 0.7.9

* Add namespace **survival::cluster, survival::frailty** to `cox2.display`

# jstable 0.7.8

* Remove 2 packages to Import: **DT**, **epiDisplay**. 

# jstable 0.7.7

* Fix typo in DESCRIPTION.

# jstable 0.7.6

* Fix description text and some examples for cran release.

# jstable 0.7.5

* Change package **Title** for cran release.

# jstable 0.7.4

## Bug fixes

* Fix some spell for cran release

## Update

* Update **travis-ci**

* Add appveyor CI to test window environment

* Add vignettes

# jstable 0.7.3

## Update

* Add **R-squared** to `glmshow.display`

# jstable 0.7.2

## Bug fixes 

* `svyCreateTableOne2`,  `svyCreateTableOneJS`, `LabelJsTable`, `LabelepiDisplay` and `svyregress.display`

## Update 

* `coefNA` can be used in `svyregress.display`

# jstable 0.7.1

## Bug fixes

* `svyglm` function.

* Apply **testhat**.

# jstable 0.7.0

## Update 

* Auto-selection between **Chi-square test** and **Fisher's exact test** in `CreateTableOneJS`, `CreateTableOne2`.

* Table 1 for survey data: `svyCreateTableOne2` and `svyCreateTableOneJS` are modified functions of `svyCreateTableOne`(**tableone** package).

# jstable 0.6.9

* New function: `coefNA`

* Bug fixes: Coefficients in `glmshow.display`, `cox2.display`

# jstable 0.6.8

* Bug fixes: data.frame & cluster model issue in `cox2.display`

# jstable 0.6.7 

*  Bug fixes : duplicate variable name - `glmshow.display`, `cox2.display`, `geeglm.display`, `coxme.display` 

# jstable 0.6.5

## New function

* `glmshow.display`: table from `glm.object`.

## Bug fixes

* `LabelepiDisplay`: column name issue.

# jstable 0.6.3

## New function

* `svycox.display`: table from `svycoxph.object` in **survey** package 

# jstable 0.6.2

## New function

* `svyregress.display`: table from `svyglm.object` in **survey** package 

# jstable 0.6.1

* Update: `cox2.display` function allows `data` argument.

* Remove `jsBasicGadget` : Move to **jsmodule** package.


# jstable 0.6.0

* Shiny gadget for descriptive statistics: `jsBasicGadget`

* Rstudio Addin of `jsBasicGadget`: `jsBasicAddin`

# jstable 0.5.2

* Bug fixes: `geeExp`, `lmerExp` function


# jstable 0.5.1

* Bug fixes: `coxExp`, `cox2.display` function


# jstable 0.5.0

## New function

* Table from `coxph.object` (**survival** package) - allow `cluster` & `frailty` options: `cox2.display` function

* Apply label information to `cox2.display`: `LabeljsCox` function

* Apply label information to `geeglm.display`: `LabeljsGeeglm` function

## Bug fixes

* Bug fixes: `geeglm.display` function



# jstable 0.4.5 

* Apply label information to `epiDisplay.object`: `LabelepiDisplay` function

* Apply label information to `lmer.display`, `coxme.display`: `LabeljsMixed` function




# jstable 0.4.0

## New function

* Table from `coxme.object` (**coxme** package): `coxme.display` function

## Bug fixes

* Bug fixes: 1 variable case.




# jstable 0.3.5

* Change default page length option of `opt.tb1` from 10 to 25.
