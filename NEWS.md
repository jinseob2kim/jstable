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
