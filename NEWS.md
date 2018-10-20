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