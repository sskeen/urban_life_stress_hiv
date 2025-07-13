
		*--------------------------------------------------------------------------------------*
		*																		               *
		*	Law enforcement–related stress and gentrification as determinants of CD4 decline   *
		*		urban_life_stress_hiv.do										               *
		*		Simone J. Skeen (07-12-2025)									               *
		*																		               *
		*--------------------------------------------------------------------------------------*

clear all
set more off

* wd

cd "C:\Users\sskee\OneDrive\Documents\02_tulane\01_research\noah\urban_life_stress_hiv\inputs\data"
clear

* set scheme, font

set scheme white_tableau
graph set window fontface "Arial"

		///////////////// *----------------------------------------* /////////////////
		///////////////// * Import complete lvl-1 dataset: N = 395 * /////////////////
		///////////////// *----------------------------------------* /////////////////

import excel Data, firstrow case(lower)
d

* log

capture log close
log using urban_life_stress_hiv_n395_log.txt, replace

* housekeeping

* drop D* carc_study_id

drop if _n > 395

* gen sum_stigma

		*** SJS 5/30: confirmed: the lvl-2 stigma_sum _is_ a simple sum
	
egen stigma_sum = rowtotal(stigma1-stigma40)		

* harmonize ulss varnames

foreach i of varlist ulss1-ulss21 {
	rename `i' `i'_n		
	}

* save .dta

save urban_life_stress_hiv_n395, replace

		///////////////// *-------------------------------------------------------------* /////////////////
		///////////////// * Import geolinking-restricted lvl-1 / lvl-2 dataset: n = 274 * /////////////////
		///////////////// *-------------------------------------------------------------* /////////////////

clear

*import delimited gent_pat_cd4  
*import delimited gent_pat_cd4_051525
import delimited gent_pat_cd4_geo_restr_051525
d

* drop implicit idx / artifact

drop v1

* drop addresses: anonymize

*drop street_* full_address
drop full_address

* log

capture log close
log using urban_life_stress_hiv_n274.txt, replace

* encode ulss

foreach i of varlist ulss1-ulss21 {
	encode `i', gen(`i'_n)
	}
	
* save .dta

save urban_life_stress_hiv_n274, replace		


		///////////////// Table 1. NOAH cohort individual (N = 395) socio-demographics at baseline /////////////////

		use urban_life_stress_hiv_n395, clear

		summ age_current
		tab gender
		tab race
		tab ethnicity
		tab income
		tab housing
		tab homeless
		tab incarcerated
		tab cd4cat
		tab vlcat

		////////////////////////////////////////////////////////////////////////////////////////////////////////////


		///// Table 2. NOAH cohort residential socio-economics, crime and law enforcement exposures (n = 274) at baseline /////

				*** SJS 6/24: tabulated in R by EAS
		
		//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

		
		///////////////// *-----------------------------* /////////////////
		///////////////// * Composite indices: generate * /////////////////
		///////////////// *-----------------------------* /////////////////		

		*** SJS 6/5: EAS lvl-1 loadings, entered manually
		
		*** SJS 5/8: EAS lvl-2 loadings, entered manually

* F1: weighted + unweighted: compute n395 indices regardless of restriction

local myfiles : dir "C:\Users\sskee\OneDrive\Documents\02_tulane\01_research\noah\urban_life_stress_hiv\inputs\data" files "*.dta"
foreach file in `myfiles' {
    use "`file'", clear

		*** SJS 6/26: clean this gross wd string up...
	
	gen F1_w_n395 = ((ulss5_n * 0.75) + (ulss6_n * 0.73) + (ulss7_n * 0.62) + (ulss8_n * 0.78)) / 4
	label var F1_w_n395 "hyperlocal everday stressors (weighted, n395)"

	gen F1_u_n395 = (ulss5_n + ulss6_n + ulss7_n + ulss8_n) / 4
	label var F1_u_n395 "hyperlocal everday stressors (unweighted, n395)"
	
	* Cronbach's

	alpha ulss5_n ulss6_n ulss7_n ulss8_n
	
	save "`file'", replace
	}

/* F1: weighted + unweighted: compute n395 indices for N = 395 set, n274 indices for n = 274 set

*use urban_life_stress_hiv_n395, clear			

*gen F1_w_n395 = ((ulss5_n * 0.75) + (ulss6_n * 0.73) + (ulss7_n * 0.62) + (ulss8_n * 0.78)) / 4
*label var F1_w_n395 "hyperlocal everday stressors (weighted lvl-1)"

*gen F1_u_n395 = (ulss5_n + ulss6_n + ulss7_n + ulss8_n) / 4
*label var F1_u_n395 "hyperlocal everday stressors (unweighted lvl-1)"			
*/			

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear		

* _alt_ specification: geolinking-restricted F1 factor loadings / scores
		
gen F1_w_n274 = ((ulss8_n * 0.81) + (ulss12_n * 0.73) + (ulss6_n * 0.73) + (ulss5_n * 0.71) + (ulss7_n * 0.63)) / 5
label var F1_w_n274 "hyperlocal everday stressors (weighted, n274)"

gen F1_u_n274 = (ulss8_n + ulss12_n + ulss6_n + ulss5_n + ulss7_n) / 5
label var F1_u_n274 "hyperlocal everday stressors (unweighted, n274)"

* save
	
save urban_life_stress_hiv_n274, replace
		
* F2: weighted + unweighted: compute n395 indices regardless of restriction	

local myfiles : dir "C:\Users\sskee\OneDrive\Documents\02_tulane\01_research\noah\urban_life_stress_hiv\inputs\data" files "*.dta"
foreach file in `myfiles' {
    use "`file'", clear
	
	gen F2_w_n395 = ((ulss14_n * 0.91) + (ulss15_n * 0.84) + (ulss13_n * 0.84) + (ulss20_n * 0.65) + (ulss19_n * 0.61)) / 5
	label var F2_w_n395 "strife and disorder stressors (weighted, n395)"

	gen F2_u_n395 = (ulss14_n + ulss15_n + ulss13_n + ulss20_n + ulss19_n) / 5
	label var F2_u_n395 "strife and disorder stressors (unweighted, n395)"	
	
	* Cronbach's

	alpha ulss14_n ulss15_n ulss13_n ulss20_n ulss19_n
	
	save "`file'", replace
	}

	
/* F1: weighted + unweighted: compute n395 indices for N = 395 set, n274 indices for n = 274 set

*use urban_life_stress_hiv_n395, clear	

gen F2_w_n395 = ((ulss14_n * 0.91) + (ulss15_n * 0.84) + (ulss13_n * 0.84) + (ulss20_n * 0.65) + (ulss19_n * 0.61)) / 5
label var F2_w_n395 "strife and disorder stressors (weighted lvl-1)"

gen F2_u_n395 = (ulss14_n + ulss15_n + ulss13_n + ulss20_n + ulss19_n) / 5
label var F2_u_n395 "strife and disorder stressors (unweighted lvl-1)"	

		*** SJS 6/5: so, here, different from lvl-2, "family violence" is included (at 0.61)
		
		*** SJS 6/5: so at a rounded 0.6 cutoff, that would still differ...who cares
*/

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear				

* _alt_ specification: geolinking-restricted F2 factor loadings / scores
		
gen F2_w_n274 = ((ulss14_n * 0.89) + (ulss15_n * 0.88) + (ulss13_n * 0.83) + (ulss20_n * 0.68) + (ulss21_n * 0.61)) / 5
label var F2_w_n274 "strife and disorder stressors (weighted, n274)"

gen F2_u_n274 = (ulss14_n + ulss15_n + ulss13_n + ulss20_n + ulss21_n) / 5
label var F2_u_n274 "strife and disorder stressors (unweighted, n274)"	
	
* save
	
save urban_life_stress_hiv_n274, replace
		
		///////////////// *---------------------------------* /////////////////
		///////////////// * Model specifications: replicate * /////////////////
		///////////////// *---------------------------------* /////////////////

		*** SJS 5/29: independent replication of EAS R code...filename harmonization for repo TKTK

* lvl-2 id / indexing = geoid

*tab geoid		
		
* lvl-1 id / indexing = carc_study_id		
		
*list carc_study_id geoid zipcode city, sep(0)		
		
		
		///////////////// *-----------------------------------------* /////////////////
		///////////////// * Model A: y_i = borderline depression Sx * /////////////////
		///////////////// *-----------------------------------------* /////////////////		

* transform covars for model entry		

* use complete lvl-1 dataset: N = 395

use urban_life_stress_hiv_n395, clear

* rename LEH

rename lehtotal_sum lehtotal_sum_n

		*** SJS 6/26: check why n74 has an "NA" and n395 doesn't - eventually

* save
		
save urban_life_stress_hiv_n395, replace

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear	

* destring ICE_race + SDI

foreach i of varlist ice_race sdi {
	destring `i', gen(`i'_n) force
	replace `i'_n = . if `i' == "NA"
	list `i' `i'_n, sep(0)
	}

* destring LEH

drop if lehtotal_sum == "NA"
destring lehtotal_sum, generate(lehtotal_sum_n)	
	
		*** SJS 5/29: loop over all Model A + B destrings eventually		

* z-standardize n274 indices		

foreach i of varlist F1_u_n274 F2_u_n274 F1_w_n274 F2_w_n274 {
	egen z`i' = std(`i')
	}	
	
* save
		
save urban_life_stress_hiv_n274, replace		

local myfiles : dir "C:\Users\sskee\OneDrive\Documents\02_tulane\01_research\noah\urban_life_stress_hiv\inputs\data" files "*.dta"
foreach file in `myfiles' {
    use "`file'", clear
	
	* gen age60

	gen age60 = 0
	replace age60 = 1 if (age_current >= 60)
	list age_current age60, sep(0)		
		
	* gen race_bin

	gen race_bin = 0
	replace race_bin = 1 if (race == 4)
	list race race_bin, sep(0)		
		
	* gen assigned_sex /// 0 = female; 1 = male

	gen assigned_sex = 0
	replace assigned_sex = 1 if (gender == 2)
	list gender assigned_sex, sep(0)

	* gen homeless_bin /// 0 = no; 1 = homeless/shelter

	gen homeless_bin = 0
	replace homeless_bin = 1 if (housing == 4)
	list housing homeless_bin, sep(0)		

	* z-standardize: cont varlist

	foreach i of varlist F1_u_n395 F2_u_n395 F1_w_n395 F2_w_n395 ///
	lehtotal_sum_n stigma_sum {
	egen z`i' = std(`i')
	}

	*cap drop zF1_u_n395 zF2_u_n395 zF1_w_n395 zF2_w_n395

	* gen dichot outcome: hads_depressbin
	
	tab hads_depressscore
	tab hads_depresscat /// 0 = 0-8 "normal"; 1 = 8-11 "borderline"; 2 = >11 "depressed"

	gen hads_depressbin = 0
	replace hads_depressbin = 1 if (hads_depresscat >= 1)

	tab hads_depresscat
	tab hads_depressbin
	
	save "`file'", replace
	}

		
		///////// Model A0: varying-intercept ("empty" model) > ICC /////////
		*	  																*
		*	  complete lvl-1 dataset: N = 395								*
		*     x_i = none;    												*
		*     x_j = none	    											*
		*																	*
		///////////////////////////////////////////////////////////////////// 

* use complete lvl-1 dataset: N = 395

use urban_life_stress_hiv_n395, clear		
		
		*** SJS 6/24: I need to do this for both, eventually...ICC is almost certainly negligible regardless
				
		*mixed hads_depressbin || geoid:, mle
		melogit hads_depressbin || geoid:

		estat icc 

* intraclass correlation / variance partition /// estat icc ok for logit / glm+ etc: https://www.stata.com/features/overview/intraclass-correlations-for-multilevel-models/

		*** SJS 5/29: ICC is infinitesmal; proceeding w/ logit

		////////////////////// Model A1: bivar / crude //////////////////////
		*																	*
		*	  complete lvl-1 dataset: N = 395								*
		*     x_i = all lvl-1 predictors (SJS theoretically selected);    	*
		*     x_j = none	    											*
		*																	*
		///////////////////////////////////////////////////////////////////// 
	
* factor1: refined factor scores [ml2] vs "unrefined" un/weighted composite indices [F1_u, F1_w]
* factor2: refined factor scores [ml1] vs "unrefined" un/weighted composite indices [F2_u, F2_w] 	

		* factor varlist

		foreach i of varlist medhx___36 incarcerated /// 
			homeless_bin race_bin assigned_sex age60 {
			logit hads_depressbin i.`i', or nolog
			}

		* cont z-standardized varlist
			
		foreach i of varlist zF1_w_n395 zF2_w_n395 zstigma_sum zlehtotal_sum {
			logit hads_depressbin `i', or nolog
			}	
		
* unweighted "unrefined" factor scores
		
*logit hads_depressbin zF1_u zF2_u i.housing zlehtotal_sum i.incarcerated zstigma_sum ///
	i.assigned_sex i.age60 i.race_bin		
	
		/////////////////// Model A2: multivar / adjusted ///////////////////
		*																	*
		*	  complete lvl-1 dataset: N = 395								*
		*     x_i = all lvl-1 predictors (SJS theoretically selected);    	*
		*     x_j = none												    *
		*																	*
		///////////////////////////////////////////////////////////////////// 
	
		* weighted "unrefined" factor scores

		logit hads_depressbin ///
			zF1_w_n395 zF2_w_n395 zstigma_sum i.medhx___36 zlehtotal_sum i.incarcerated i.homeless_bin ///
			i.race_bin i.assigned_sex i.age60, or nolog

				*** SJS 5/27: leaning toward this as "preferred" given its precision (thanks to _w) and interpretability balance
				
		* "refined" factor scores: EAS / psych R package 	
				
		*logit hads_depressbin zml2 zml1 i.housing zlehtotal_sum_n i.incarcerated zstigma_sum ///
			i.assigned_sex i.age60 i.race_bin

			*** SJS 5/29: robust against that respecification, fuck yeah

		////////////////////// Model A3: bivar / crude //////////////////////
		*																	*
		*	  geolinking-restricted lvl-1 / lvl-2 dataset: n = 274			*
		*     x_i = none;    												*
		*     x_j = all lvl-2 predictors (EAS theoretically selected)	    *
		*																	*
		///////////////////////////////////////////////////////////////////// 

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear		
		
		* loop over buffer sizes		

		foreach i of varlist pd_call_010m pd_call_025m pd_call_050m ice_race_n sdi_n {		
			logit hads_depressbin `i', or vce(cluster geoid) nolog
			}
		
		*** SJS 5/29: null af

		/////////////////// Model A4: multivar / adjusted ///////////////////
		*																	*
		*	  geolinking-restricted lvl-1 / lvl-2 dataset: n = 274			*
		*     x_i = none;											        *
		*     x_j = all lvl-2 predictors (EAS theoretically selected)	    *
		*																	*
		///////////////////////////////////////////////////////////////////// 		
		
		* loop over buffer sizes w/ covars	

		foreach i of varlist pd_call_010m pd_call_025m pd_call_050m {		
			logit hads_depressbin ice_race_n sdi_n `i', or vce(cluster geoid) nolog
			}

		/////////////////// Model A5: multivar / adjusted ///////////////////
		*																	*
		*	  geolinking-restricted lvl-1 / lvl-2 dataset: n = 274			*
		*     x_i = all lvl-1 predictors (SJS theoretically selected);      *
		*     x_j = all lvl-2 predictors (EAS theoretically selected)	    *
		*																	*
		///////////////////////////////////////////////////////////////////// 	

		*** SJS 5/29: pd buffers are all null, using 10m for now

		* n395 loadings / indices		
				
		logit hads_depressbin ///
			pd_call_010m ice_race_n sdi_n ///
			zF1_w_n395 zF2_w_n395 zstigma_sum i.medhx___36 zlehtotal_sum_n i.incarcerated i.homeless_bin ///
			i.race_bin i.assigned_sex i.age60, or vce(cluster geoid) nolog	

		* _alt_ specification - n274 loadings / indices		

		logit hads_depressbin ///
			pd_call_010m ice_race_n sdi_n ///
			zF1_w_n274 zF2_w_n274 zstigma_sum i.medhx___36 zlehtotal_sum_n i.incarcerated i.homeless_bin ///
			i.race_bin i.assigned_sex i.age60, or vce(cluster geoid) nolog	
				
		* multicollinearity dx - uncentered

		collin F1_w_n274 F2_w_n274 homeless_bin medhx___36 lehtotal_sum_n incarcerated stigma_sum ///
			assigned_sex age60 race_bin pd_call_010m ice_race_n sdi_n if !missing(hads_depressbin), corr

		* margins
			
		*** SJS 7/12: using Model A5 specification / post-estimation w/ n395 loadings
					
		* LEH - median-split	

		summ zlehtotal_sum_n, detail	
		xtile zlehtotal_sum_n_mdn = zlehtotal_sum_n, nq(2)
		list zlehtotal_sum_n zlehtotal_sum_n_mdn, sep(0)
		recode zlehtotal_sum_n_mdn (1=0) (2=1)	
		tab zlehtotal_sum_n_mdn

		* re-run: LEH Mdn as factor var

		logit hads_depressbin ///
			pd_call_010m ice_race_n sdi_n ///
			zF1_w_n395 zF2_w_n395 zstigma_sum i.medhx___36 i.zlehtotal_sum_n_mdn i.incarcerated i.homeless_bin ///
			i.race_bin i.assigned_sex i.age60, or vce(cluster geoid) nolog

		* F1 - display quartiles

		summ zF1_w_n395, detail

		* marginal effects at representative values

		margins, dydx(zlehtotal_sum_n_mdn) at(zF1_w_n395 = (-1.3206 -.8746 -.0885 .6091 1.9727)) vsquish	
			
		*marginsplot, recast(line) recastci(rarea) title(" ") ///
		*plot1opts(lcolor("204 0 204")) ci1opts(color("204 0 204%15")) ///
		*xtitle("quartiles of _hyperlocal, everyday stress_ endorsement", size(*.95)) ///
		*ytitle("Pr(borderline depression = 1)")

		
		///////////////// *--------------------------------------------* /////////////////
		///////////////// * Model B: y_i = suboptimal CD4 count (<500) * /////////////////
		///////////////// *--------------------------------------------* /////////////////		

* transform covars for model entry		

* use complete lvl-1 dataset: N = 395

use urban_life_stress_hiv_n395, clear

* inspect cd4cat /// 1 = <200 cells/mm^3; 2 = 200-499 cells/mm^3; 3 = 500+ cells/mm^3

tab cd4cat

* rename cd4cat

rename cd4cat cd4cat_n

* save

save urban_life_stress_hiv_n395, replace

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear

* destring

destring cd4cat, gen(cd4cat_n) force
replace cd4cat_n = . if cd4cat == "NA"

* save

save urban_life_stress_hiv_n274, replace

local myfiles : dir "C:\Users\sskee\OneDrive\Documents\02_tulane\01_research\noah\urban_life_stress_hiv\inputs\data" files "*.dta"
foreach file in `myfiles' {
    use "`file'", clear

	* gen audit_bin

	gen audit_bin = 0
	replace audit_bin = 1 if (audit_total > 7)
	list audit_bin audit_total, sep(0)

	* gen dichot outcome: cd4_bin

	gen cd4_bin = .
	replace cd4_bin = 0 if (cd4cat_n < 3)
	replace cd4_bin = 1 if (cd4cat_n == 3)
	list cd4cat_n cd4_bin, sep(0)	

	save "`file'", replace
	}

		///////// Model B0: varying-intercept ("empty" model) > ICC /////////
		*	  																*
		*	  complete lvl-1 dataset: N = 395								*
		*     x_i = none;    												*
		*     x_j = none	    											*
		*																	*
		///////////////////////////////////////////////////////////////////// 

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear

* ICC check
				
		*mixed cd4_bin || geoid:, mle
		melogit cd4_bin || geoid:
		estat icc 

		*** SJS 5/29: ICC is infinitesmal; proceeding w/ logit

		////////////////////// Model B1: bivar / crude //////////////////////
		*																	*
		*	  complete lvl-1 dataset: N = 395								*
		*     x_i = all lvl-1 predictors (SJS theoretically selected);    	*
		*     x_j = none	    											*
		*																	*
		///////////////////////////////////////////////////////////////////// 

* use complete lvl-1 dataset: N = 395

use urban_life_stress_hiv_n395, clear		
		
		* factor varlist

		foreach i of varlist art audit_bin dailypolyuse ///
			assigned_sex age60 race_bin {
			logit cd4_bin i.`i', or nolog
			}

		* cont z-standardized varlist
			
		foreach i of varlist zF1_w zF2_w {
			logit cd4_bin `i', or nolog
			}		
			
		/////////////////// Model B2: multivar / adjusted ///////////////////
		*																	*
		*	  complete lvl-1 dataset: N = 395								*
		*     x_i = all lvl-1 predictors (SJS theoretically selected);    	*
		*     x_j = none												    *
		*																	*
		///////////////////////////////////////////////////////////////////// 

		logit cd4_bin /// 
			zF1_w zF2_w i.art i.audit_bin i.dailypolyuse i.assigned_sex i.age60 i.race_bin, or nolog	

		* average marginal effect: x_1
				
		margins, dydx(race_bin)			
				
		* marginal effects at representative values: x_k

		*margins, dydx(race_bin) at(zF2_w_n395 =(0 1 2)) vsquish	
		margins, dydx(race_bin) at(audit_bin =(0 1)) vsquish	
	
		////////////////////// Model A3: bivar / crude //////////////////////
		*																	*
		*	  geolinking-restricted lvl-1 / lvl-2 dataset: n = 274			*
		*     x_i = none;    												*
		*     x_j = all lvl-2 predictors (EAS theoretically selected)	    *
		*																	*
		///////////////////////////////////////////////////////////////////// 

* use geolinking-restricted lvl-1 / lvl-2 dataset: n = 274

use urban_life_stress_hiv_n274, clear	

		* loop over buffer sizes	

		foreach i of varlist pd_call_010m pd_call_025m pd_call_050m ice_race_n sdi_n {		
			logit cd4_bin `i', or vce(cluster geoid) nolog
			}	

		/////////////////// Model B4: multivar / adjusted ///////////////////
		*																	*
		*	  geolinking-restricted lvl-1 / lvl-2 dataset: n = 274			*
		*     x_i = none;											        *
		*     x_j = all lvl-2 predictors (EAS theoretically selected)	    *
		*																	*
		///////////////////////////////////////////////////////////////////// 		
		
		* loop over buffer sizes w/ covars	

		foreach i of varlist pd_call_010m pd_call_025m pd_call_050m {		
			logit cd4_bin ice_race_n sdi_n `i', or vce(cluster geoid) nolog
			}	
	
		/////////////////// Model B5: multivar / adjusted ///////////////////
		*																	*
		*	  geolinking-restricted lvl-1 / lvl-2 dataset: n = 274			*
		*     x_i = all lvl-1 predictors (SJS theoretically selected);      *
		*     x_j = all lvl-2 predictors (EAS theoretically selected)	    *
		*																	*
		///////////////////////////////////////////////////////////////////// 	

		* n395 loadings / indices		
				
		logit cd4_bin ///
			pd_call_010m ice_race_n sdi_n ///
			zF1_w_n395 zF2_w_n395 i.art i.audit_bin i.dailypolyuse ///
			i.assigned_sex i.age60 i.race_bin, or vce(cluster geoid) nolog	

		* _alt_ specification - n274 loadings / indices	
			
		logit cd4_bin ///
			pd_call_010m ice_race_n sdi_n ///
			zF1_w_n274 zF2_w_n274 i.art i.audit_bin i.dailypolyuse ///
			i.assigned_sex i.age60 i.race_bin, or vce(cluster geoid) nolog	
				
		* multicollinearity dx - uncentered

		collin F1_w_n274 F2_w_n274 art audit_bin dailypolyuse ///
			assigned_sex age60 race_bin pd_call_010m ice_race_n sdi_n if !missing(cd4_bin), corr

		//////////////// Model B: exploratory re-specification //////////////
		*																	*
		*	  lvl-1 dataset restricted on race = Black: N = 329				*
		*     x_i = F1, F2, all ulss item-wise;    							*
		*     x_j = none												    *
		*																	*
		///////////////////////////////////////////////////////////////////// 

* use complete lvl-1 dataset: N = 395

use urban_life_stress_hiv_n395, clear			
		
		keep if race_bin == 1		
		*keep if assigned_sex == 1 & race_bin == 1
		*keep if assigned_sex == 0 & race_bin == 1

		* cont z-standardized ULSS

		foreach i of varlist ulss1_n-ulss21_n {
			egen z`i' = std(`i')
			}

		* factor varlist

		foreach i of varlist art audit_bin dailypolyuse ///
			assigned_sex age60 {
			logit cd4_bin i.`i', or nolog
			}

		* z-standardized ULSS cont varlist	
			
		foreach i of varlist zF1_w_n395 zF2_w_n395 zulss1_n-zulss21_n {
			logit cd4_bin `i', or nolog
			}

		* crude bivar - confirmatory	
			
		*logit cd4_bin zulss21_n, or nolog
				
		* n395
				
		logit cd4_bin zulss21_n i.art i.audit_bin i.dailypolyuse ///
			i.assigned_sex i.age60, or nolog		

				*** SJS 6/26: ulss21 sig _if_ F1 is excluded...interesting
				
		collin zulss21_n art audit_bin dailypolyuse ///
			assigned_sex age60 if !missing(cd4_bin), corr		

		* marginal effects at representative values

		summ zulss21_n, detail

		margins, dydx(assigned_sex) at(zulss21_n = (-.6200 .6671 1.9542)) vsquish


 *---------------------------------*
 * End of urban_life_stress_hiv.do *			
 *---------------------------------*		
		
		
		