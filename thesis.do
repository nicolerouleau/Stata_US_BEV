cd "/Users/Nicole/Thesis"

clear all

set more off, permanently
log using thesis_log, smcl append

import excel Thesis_Data.xlsx, sheet("full") firstrow


//summary stat
sutex year BEV market_share_by_state quant_sold avg_msrp fed_sub avg_state_sub avg_holdback state_pop med_state_income, key(mid_descstat) replace ///
file(thesis_descstat.tex) title("Summary Statistics") minmax


//generate dummy*variables
gen BEVXstate_sub=BEV*avg_state_sub
gen BEVXfed_sub=BEV*fed_sub
gen BEVXshare = BEV*market_share_by_state
gen BEVXmsrp = BEV*avg_msrp

//Here I generate state dummy variables. 
tabulate state, gen(s_)

//sort data
sort state year
////gen regression prep
egen tot_veh_sold=sum(quant_sold), by(state year)
gen outside_share= 1 - tot_veh_sold/state_pop
gen inside_share=quant_sold/state_pop
gen logshare_sub_logout = log(inside_share) - log(outside_share)

//specification on subsidy data between bev and non-bev 
////sort data again (need to do again bc sorting again)
sort state year
egen state_sub2=max(avg_state_sub), by(state year)
sort year
egen fed_sub2=max(fed_sub), by(year)

gen BEVXstate_sub2=BEV*state_sub2
gen BEVXfed_sub2=BEV*fed_sub2

gen y2019 = 0
replace y2019=1 if year==2019

eststo: regress logshare_sub_logout BEV BEVXstate_sub2 BEVXfed_sub2 state_sub2 fed_sub2 med_state_income avg_msrp y2019  s_1 - s_49

esttab using log_thesis_spec, replace keep(BEV BEVXstate_sub2 BEVXfed_sub2 state_sub2 fed_sub2 med_state_income avg_msrp) ar2 title("Logit Specific") addnotes("F(57,242) = 159.79" "Prob > F = 0.0000") tex 

esttab using log_thesis_appendix, replace ar2 title("Logit Specific Full") longtable addnotes("F(57,242) = 159.79" "Prob > F = 0.0000") tex 

// probability 
gen test=_b[_cons]

gen inclusive_val=_b[_cons]

foreach var of varlist BEV BEVXstate_sub2 BEVXfed_sub2 state_sub2 fed_sub2 med_state_income avg_msrp y2019 s_1 - s_49 {
	replace inclusive_val=inclusive_val+_b[`var']*`var'
}

sort state year
egen min_inc_val=min(inclusive_val), by(state year)
egen max_inc_val=max(inclusive_val), by(state year)

gen prob_purchase=exp(inclusive_val)/(1+exp(min_inc_val)+exp(max_inc_val))
gen prob_out_purchase=1/(1+exp(min_inc_val)+exp(max_inc_val))

///rerun with replace  of 0 to compare with new state subs at 0 to see if counterfactual causes a consumer behavior of 0 

replace state_sub2 = 0
replace BEVXstate_sub2 = 0

gen rep_inclusive_val=_b[_cons]

foreach var of varlist BEV BEVXstate_sub2 BEVXfed_sub2 state_sub2 fed_sub2 med_state_income avg_msrp y2019 s_1 - s_49 {
	replace rep_inclusive_val=rep_inclusive_val+_b[`var']*`var'	
}

sort state year
egen rep_min_inc_val_rep=min(rep_inclusive_val), by(state year)
egen rep_max_inc_val_rep=max(rep_inclusive_val), by(state year)

gen rep_prob_purchase=exp(rep_inclusive_val)/(1+exp(rep_min_inc_val)+exp(rep_max_inc_val))
gen rep_prob_out_purchase=1/(1+exp(rep_min_inc_val)+exp(rep_max_inc_val))

//Elasticity.
sort state year
gen no_EV_prob = prob_purchase if BEV==0
gen EV_prob = prob_purchase if BEV==1

replace no_EV_prob=no_EV_prob[_n-1] if state[_n-1]==state&year[_n-1]==year&no_EV_prob==.
replace no_EV_prob=no_EV_prob[_n+1] if state[_n+1]==state&year[_n+1]==year&no_EV_prob==.

replace EV_prob=EV_prob[_n-1] if state[_n-1]==state&year[_n-1]==year&EV_prob==.
replace EV_prob=EV_prob[_n+1] if state[_n+1]==state&year[_n+1]==year&EV_prob==.

gen elasticity = state_sub2*((_b[BEVXstate_sub2] + _b[state_sub2])*(1 - EV_prob) - _b[state_sub2]*no_EV_prob)
summarize elasticity if BEV==1


replace state_sub2 = 0
replace BEVXstate_sub2 = 0



foreach var of varlist BEV BEVXstate_sub2 BEVXfed_sub2 state_sub2 fed_sub2 med_state_income avg_msrp y2019 s_1 - s_49 {
	replace rep_inclusive_val=rep_inclusive_val+_b[`var']*`var'	
}

sort state year
egen rep_min_inc_val_rep=min(rep_inclusive_val), by(state year)
egen rep_max_inc_val_rep=max(rep_inclusive_val), by(state year)

gen rep_prob_purchase=exp(rep_inclusive_val)/(1+exp(rep_min_inc_val)+exp(rep_max_inc_val))
gen rep_prob_out_purchase=1/(1+exp(rep_min_inc_val)+exp(rep_max_inc_val))


//results of probabilities
mkmat year BEV prob_purchase prob_out_purchase rep_prob_purchase rep_prob_out_purchase, matrix(Probability) rownames(state) 
matrix list Probability

sum prob_purchase if BEV==1
sutex prob_purchase if BEV==1, replace ///
file(thesis_sum_bev_prob_sub.tex) title("BEV Probability Purchase (Subsidies)") minmax digits(6)

sum rep_prob_purchase if BEV==1
sutex rep_prob_purchase if BEV==1, replace ///
file(thesis_sum_bev_prob.tex) title("BEV Probability Purchase (No Subsidies)") minmax digits(6)

sum prob_purchase if BEV==0
sutex prob_purchase if BEV==0, replace ///
file(thesis_sum_prob_sub.tex) title("Non-BEV Probability Purchase (Subsidies)") minmax digits(6)

sum rep_prob_purchase if BEV==0
sutex rep_prob_purchase if BEV==0, replace ///
file(thesis_sum_prob.tex) title("Non-BEV Probability Purchase (No Subsidies)") minmax digits(6)

// test heterskedasticity complete this after regression
estat hettest

log close

