* ssc install ghk2, replace
* ssc install cmp, replace
* ssc install estout, replace
* ssc install texsave, replace

version 18
clear all
set type double

gl path "P:\svo\20250501 labor"
cd "$path"
gl reg_data "reg_data.csv"

import delimited "$reg_data", clear

* data prep
{
qui ds, has(type string)
foreach i of var `r(varlist)' {
	replace `i' = "" if inlist(`i', "NA", "-Inf", "Inf")
}

destring *, replace

xtset id year

* Labels
{
label var education             "Years of Education"
label var education_dad         "Dad's Education"
label var education_mom         "Mom's Education"
label var drug_use_score        "Childhood Drug Use Score"
label var flag_female          "Female"
label var flag_black           "Black"
label var flag_hispanic        "Hispanic"
label var flag_divorce         "Parents Divorced"
label var school_quality       "School Quality Score"
label var cv_census_region     "Census Region"
label var log_wage "Log(Wage)"
label var fte_experience "Work Experience"
label var flag_working "Currently Working"
label var flag_married "Married"
label var num_children "Number of Children"
label var fte_experience_sq "Work Experience Squared"
label var age     "Age"
}
}

* OLS
{
reg log_wage education fte_experience fte_experience_sq if flag_working & flag_female, vce(cluster id)

outreg2 using "results", replace bd(3) sd(3) label dta adjr2 ctitle("OLS 1") addtext("Region FE","No")


reg log_wage education fte_experience fte_experience_sq ///
 flag_black flag_hispanic num_children i.cv_census_region ///
if flag_working & flag_female, vce(cluster id)

outreg2 using "results", append bd(3) sd(3) label dta adjr2 ctitle("OLS 2") addtext("Region FE","Yes")
}

* Fixed effects
{
areg log_wage education fte_experience fte_experience_sq num_children i.cv_census_region ///
if flag_working & flag_female, vce(cluster id) a(id)

outreg2 using "sensitivities", replace bd(3) sd(3) label dta adjr2 ctitle("OLS w/ FE") addtext("Region FE","Yes")
}

* IV
{
ivregress 2sls log_wage fte_experience fte_experience_sq ///
 flag_black flag_hispanic num_children i.cv_census_region ///
(education = education_dad education_mom) ///
if flag_working & flag_female, vce(cluster id)

outreg2 using "results", append bd(3) sd(3) label dta adjr2 ctitle("IV 1") addtext("Region FE","Yes")

ivregress 2sls log_wage fte_experience fte_experience_sq ///
flag_black flag_hispanic num_children i.cv_census_region ///
(education = education_dad education_mom drug_use_score school_quality) ///
if flag_working & flag_female, vce(cluster id)

outreg2 using "results", append bd(3) sd(3) label dta adjr2 ctitle("IV 2") addtext("Region FE","Yes")


xtivreg log_wage fte_experience fte_experience_sq num_children i.cv_census_region ///
(education = education_dad education_mom) ///
if flag_working & flag_female, vce(cluster id)

outreg2 using "sensitivities", append bd(3) sd(3) label dta ctitle("IV 1 RE") addtext("Region FE","Yes")

xtivreg log_wage fte_experience fte_experience_sq num_children i.cv_census_region ///
(education = education_dad education_mom drug_use_score school_quality) ///
if flag_working & flag_female, vce(cluster id)

outreg2 using "sensitivities", append bd(3) sd(3) label dta ctitle("IV 2 RE") addtext("Region FE","Yes")
}

* table with just IV (both stages)
{
reg education education_dad education_mom ///
if flag_working & flag_female, vce(cluster id)
outreg2 using "iv results", replace bd(3) sd(3) label dta adjr2 ctitle("IV 1", "Education") addtext("Region FE","No")

ivregress 2sls log_wage fte_experience fte_experience_sq ///
 flag_black flag_hispanic num_children i.cv_census_region ///
(education = education_dad education_mom) ///
if flag_working & flag_female, vce(cluster id)
outreg2 using "iv results", append bd(3) sd(3) label dta adjr2 ctitle("IV 1", "Log(Wage)") addtext("Region FE","Yes")

reg education education_dad education_mom drug_use_score school_quality ///
if flag_working & flag_female, vce(cluster id)
outreg2 using "iv results", append bd(3) sd(3) label dta adjr2 ctitle("IV 2", "Education") addtext("Region FE","No")

ivregress 2sls log_wage fte_experience fte_experience_sq ///
flag_black flag_hispanic num_children i.cv_census_region ///
(education = education_dad education_mom drug_use_score school_quality) ///
if flag_working & flag_female, vce(cluster id)
outreg2 using "iv results", append bd(3) sd(3) label dta adjr2 ctitle("IV 2", "Log(Wage)") addtext("Region FE","Yes")
}

* Heckman
{
heckman log_wage education fte_experience fte_experience_sq  ///
 flag_black flag_hispanic num_children i.cv_census_region ///
if flag_female, select(flag_working = num_children flag_black flag_hispanic age flag_married) vce(cluster id)

outreg2 using "results", append bd(3) sd(3) label dta ctitle("Heckit") addtext("Region FE","Yes")

outreg2 using "heckman", replace bd(3) sd(3) label dta ctitle("Heckit") addtext("Region FE","Yes") adds("\rho", e(rho))

*xtheckman log_wage education fte_experience fte_experience_sq  ///
 flag_black flag_hispanic num_children ///
if flag_female, select(flag_working = num_children flag_black flag_hispanic age flag_married) technique(nr)
*    this is too slow

cmp ///
(log_wage = education fte_experience fte_experience_sq flag_black flag_hispanic num_children i.cv_census_region || id:) ///
(flag_working = num_children flag_black flag_hispanic age flag_married) ///
if flag_female, ind(flag_working ${cmp_probit}) nolr vce(cluster id)

outreg2 using "sensitivities", append bd(3) sd(3) label dta ctitle("Heckit RE") addtext("Region FE","Yes")
}

* CMP
{
cmp setup	

/* IV
cmp ///
(log_wage = education fte_experience fte_experience_sq ///
 flag_black flag_hispanic num_children) ///
(education = education_dad education_mom drug_use_score school_quality flag_black flag_hispanic flag_divorce) ///
if flag_working & flag_female, ind(${cmp_cont} ${cmp_cont}) qui


* heckman replication
cmp ///
(log_wage = education fte_experience fte_experience_sq  flag_black flag_hispanic num_children i.cv_census_region) ///
(flag_working = num_children flag_black flag_hispanic age flag_married) ///
if flag_female, ind(flag_working ${cmp_probit}) qui
*/

* Combination
cmp ///
(log_wage = education fte_experience fte_experience_sq flag_black flag_hispanic num_children i.cv_census_region) ///
(education = education_dad education_mom drug_use_score school_quality flag_black flag_hispanic) ///
(flag_working = num_children flag_black flag_hispanic age flag_married) ///
if flag_female, ind(flag_working ${cmp_cont} ${cmp_probit}) vce(cluster id) qui

outreg2 using "results", append bd(3) sd(3) label dta ctitle("IV+Heckit") addtext("Region FE","Yes")

* Combination with labor status affected by education
cmp ///
(log_wage = education fte_experience fte_experience_sq flag_black flag_hispanic num_children i.cv_census_region) ///
(education = education_dad education_mom drug_use_score school_quality flag_black flag_hispanic) ///
(flag_working = num_children flag_black flag_hispanic age flag_married education) ///
if flag_female, ind(flag_working ${cmp_cont} ${cmp_probit}) vce(cluster id) qui

outreg2 using "sensitivities", append bd(3) sd(3) label dta ctitle("IV+Heckit Endogenous") addtext("Region FE","Yes")

* Combination random effects
cmp ///
(log_wage = education fte_experience fte_experience_sq flag_black flag_hispanic num_children i.cv_census_region || id:) ///
(education = education_dad education_mom drug_use_score school_quality flag_black flag_hispanic) ///
(flag_working = num_children flag_black flag_hispanic age flag_married) ///
if flag_female, ind(flag_working ${cmp_cont} ${cmp_probit}) vce(cluster id) qui

outreg2 using "sensitivities", append bd(3) sd(3) label dta ctitle("IV+Heckit RE") addtext("Region FE","Yes")
}

* Reg table
{
u "results_dta", clear

drop if inrange(_n, 22, 37) | inlist(_n, 1, 3, 40, 44, 45)
drop v7 v8 v10-v16

drop if regexm(v1[_n+1], "Census Region")
drop if regexm(v1, "Census Region")

replace v1 = "" in 1
texsave using "results.tex", title("Regression Results") hlines(1 15) replace nonames noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") size("footnotesize")

* summary table
keep in 1/3
drop v1

g i=_n
reshape long v, i(i) j(j) s
reshape wide v, i(j) j(i)
drop j

ren (v1 v2 v3) (Model Coefficient SE)
texsave using "results summary.tex", title("Summary of Estimated Wage Effects") replace noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") width("0.5\linewidth")
}

* OLS table
{
u "results_dta", clear

drop if inrange(_n, 22, 37) | inlist(_n, 1, 3, 40, 44, 45)
keep v1 v2 v3

drop if regexm(v1[_n+1], "Census Region")
drop if regexm(v1, "Census Region")
drop if regexm(v3, "\(")

replace v1 = "" in 1
texsave using "ols.tex", title("OLS Regression Results") hlines(1 8) replace nonames noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") size("footnotesize") width("0.5\linewidth")
}

* IV table
{
* stage 1
u "iv results_dta", clear

drop v3 v5
drop if inrange(_n, 4, 22) | inlist(_n, 1, 33, 37, 38)
replace v1 = "" in 2
drop if regexm(v4, "\(")

texsave using "iv stage 1.tex", title("IV Regression Stage 1 Results") hlines(2 7) replace nonames noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") size("footnotesize") width("0.5\linewidth")

* stage 2
u "iv results_dta", clear

drop v2 v4
drop if inrange(_n, 23, 30) | inlist(_n, 1, 4, 33, 37, 38)
drop if regexm(v1[_n+1], "Census Region")
drop if regexm(v1, "Census Region")
replace v1 = "" in 2
drop if regexm(v5, "\(") & _n!=2

texsave using "iv stage 2.tex", title("IV Regression Stage 2 Results") hlines(2 8) replace nonames noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") size("footnotesize") width("0.5\linewidth")
}

* Heckman table
{
* stage 1
u "heckman_dta", clear	

drop v2 v4
keep if !mi(v3)
drop in 1
replace v1 = "" in 1
replace v3 = "Work Status" in 1
replace v3 = "No" if v1 == "Region FE"

drop if regexm(v3, "\(")

texsave using "heckman stage 1.tex", title("Heckman Selection Stage 1 Results") hlines(1 7) replace nonames noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") size("footnotesize") width("0.5\linewidth")

* stage 2
u "heckman_dta", clear	

drop v3 v4
keep if !mi(v2)
drop in 1
drop if regexm(v1[_n+1], "Census Region")
drop if regexm(v1, "Census Region")
replace v1 = "" in 1
drop if regexm(v2, "\(")
replace v2 = "Log(Wage)" in 1

g sort = _n
replace sort = 16.5 if v1 == "\rho"
sort sort
drop sort

texsave using "heckman stage 2.tex", title("Heckman Selection Stage 2 Results") hlines(1 8) replace nonames noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") size("footnotesize") width("0.5\linewidth")
}


* Sensitivities table
{
u "sensitivities_dta", clear

drop v6-v9 v11-v17 v19-v26

drop in 3
keep in 1/4
drop v1

g i=_n
reshape long v, i(i) j(j) s
reshape wide v, i(j) j(i)
drop j

replace v1 = regexs(1) if regexm(v1, "\(([0-9]+)\)")
destring v1, replace
sort v1
drop v1

ren (v2 v3 v4) (Model Coefficient SE)
texsave using "sensitivities summary.tex", title("Summary of Sensitivities") hlines(6) replace noendash frag location("h") footnote("Standard errors clustered by participant. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.") width("0.5\linewidth")
}
