clear
clear matrix
set more off
capture log close
global result "C:\D\Archivio2026\pubblicazioni\2018 european sociological review (penalosa-visser)\text\ESR\final version"
cd "$result"
log using "$result\hoursinequality.log", replace

*********************************************
****** PREPARING DATASET ********************
*********************************************
use "$result\individual data", clear

*** revising weights - there is a jump in DE 1999 and FR 2003 - rescaling weights by country/year
table year country, stat(mean weight) 
egen tmp=sum(weight), by(country year)
ren weight weightold
gen weight=weightold/tmp
table year country, stat(sum weight) 
drop tmp

*** selection rule: age 25-54 (included) with non missing information on sex/edu/age/weight
tab age country
drop if age <25|age>54
gen missing=(sex==""|edu==""|age==.|weight==.)
table year country, stat(percent missing)
drop if missing==1
drop missing

*** self-employed and people out of labour market do not have incomes (thus we cannot exploit non labour income owned by the partner if they are self-employed)
tab emp country, col
table emp country , stat(mean W H Y ) nformat(%9.3f)
gen employed=emp=="employed"
gen self_employed=emp=="self-employed"

* variable creation
encode country, g(cnt)
g female=sex=="female"
gen age2=age^2
g w=ln(W)
g h=ln(H)
g y=ln(Y)
encode top, g(job)
encode ind, g(sector)
gen public=private_emp=="public"
gen old=age>40&age!=.
egen jobtype=group(job sector), label

* foreign born is missing for 9.5% - attributed to native
tab foreign_born country, m
gen foreign=foreign_born=="foreign_born"

* race available only for US - recoding missing to others
tab race if country=="US", m
replace race="other" if race==""&country=="US"

* define minority if you are foreignborn in Europe and non white or foreign born in US
gen minority=foreign_born=="foreign_born"
replace minority=1 if country=="US"&race!="white"
table foreign minority country

* education variable are not comparable across countries - compare Low skill in DE-US vs FR-UK
tab edu country, col
encode edu, g(education)
recode education 1=3 2=1 3=2
label define education 1 "Low" 2 "Medium" 3 "high", modify
drop edu

* household composition - assign to "other arrangement" if married is "other" or missing
ren married tmp
gen married=tmp=="married"
drop tmp
g single=hhmember==1
g familysize=hhmember-1

* defining the household income - the computed number of people in the family is lower than hhmember (which include members younger than 25 or older than 55)
sort cnt year hid
egen tmp1=group(hid cnt year)
egen tmp2=count(tmp1), by(tmp1)
sum tmp2 hhmember
corr tmp2 hhmember
egen double tmp3=sum(Y), by(tmp1)
egen double tmp4=sum(self), by(tmp1)
bysort country: sum tmp3 tmp4

gen double hhINCOME=tmp3 if Y==.
replace hhINCOME=tmp3-Y if Y>=0&Y!=.
replace hhINCOME=0 if hhINCOME<0&hhINCOME!=.
replace hhINCOME=0.1 if hhINCOME==0
g hhincome=log(hhINCOME)
g hhself=tmp4>0
drop tmp*
sum Y hhINCOME hhincome
corr Y hhINCOME

tab education, g(edu)
tab jobtype, g(job)
g wxf=w*female

label variable female "Female"
label var age "Age"
label var age2 "AgeSq"
label var foreign "Foreign born"
label var edu2 "Medium Educ."
label var edu3 "High Educ."
label var hhincome "(log) household income (excluding respondent)"
label var hhself "Presence of self-employed in the household"
label var single "Single"
label var nchild "Number of children"
label var w "Hourly wage"
label var wxf "H. wage x Female"
label var job2 "Low skill - Capital int. "
label var job3  "Low skill - Labour int. "
label var job4  "Medium skill - Agr. manufacturing"
label var job5 "Medium skill - Capital int. services"
label var job6  "Medium skill - Labour "
label var job7 "High skill - Agr. manufacturing "
label var job8 "High skill - Capital int. services"
label var job9 "High skill - Labour int. services"
label var public "Public sector"

order hid country cnt year Y W H y w h female age age2 old education edu1-edu3 foreign race minority hhINCOME hhincome hhself married single familysize nchild employed job* sector jobtype public weight emp relationship wxf
keep hid country cnt year Y W H y w h female age age2 old education edu1-edu3 foreign race minority hhINCOME hhincome hhself married single familysize nchild employed job* sector jobtype public weight emp relationship wxf
compress
sort country year
des
save "$result\workfile.dta", replace

***********************************************************
********************* ANALYSIS ****************************
***********************************************************
use "$result\workfile.dta", clear

**********************************************************
*** table A1 in the appendix - descriptive statistics
**********************************************************
table cnt if year==1995 [aw=weight], stat(mean Y W H) stat(sd Y W H) stat(n y) nototal nformat(%9.2f) 
table cnt if year==2002 [aw=weight], stat(mean Y W H) stat(sd Y W H) stat(n y) nototal nformat(%9.2f)
table cnt if year==2009 [aw=weight], stat(mean Y W H) stat(sd Y W H) stat(n y) nototal nformat(%9.2f)
table cnt if year==2016 [aw=weight], stat(mean Y W H) stat(sd Y W H) stat(n y) nototal nformat(%9.2f)

**********************************************************
*** table A2 in the appendix - descriptive statistics
**********************************************************
bysort cnt: sum Y W H employed female age foreign edu2 edu3 hhincome hhself single nchild

**********************************************************
*** table A5 in the appendix - descriptive statistics
**********************************************************
bysort cnt: tab sector job, cell nofreq

***********************************************************
*** inequality decomposition for the entire population
***********************************************************
ssc install ainequal

gen mld_Y=.
gen mld_W=.
gen mld_H=.
gen corr=.

* Germany
forvalues y = 1991/2017 {
ainequal Y W H [aw=weight] if cnt==1&year==`y', mld
replace mld_Y=`r(mld_1)' if cnt==1&year==`y'
replace mld_W=`r(mld_2)' if cnt==1&year==`y'
replace mld_H=`r(mld_3)' if cnt==1&year==`y'
replace corr=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
}
* France 
forvalues y = 1990/2017 {
ainequal Y W H [aw=weight] if cnt==2&year==`y', mld
replace mld_Y=`r(mld_1)' if cnt==2&year==`y'
replace mld_W=`r(mld_2)' if cnt==2&year==`y'
replace mld_H=`r(mld_3)' if cnt==2&year==`y'
replace corr=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
}
* UK
forvalues y = 1991/2016 {
ainequal Y W H [aw=weight] if cnt==3&year==`y', mld
replace mld_Y=`r(mld_1)' if cnt==3&year==`y'
replace mld_W=`r(mld_2)' if cnt==3&year==`y'
replace mld_H=`r(mld_3)' if cnt==3&year==`y'
replace corr=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
}
* US
forvalues y = 1989/2019 {
ainequal Y W H [aw=weight] if cnt==4&year==`y', mld
replace mld_Y=`r(mld_1)' if cnt==4&year==`y'
replace mld_W=`r(mld_2)' if cnt==4&year==`y'
replace mld_H=`r(mld_3)' if cnt==4&year==`y'
replace corr=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
}

**********************************************************
*** table A6 in the appendix - decomposition by country/year
**********************************************************
table year if country=="DE", stat(mean mld_Y mld_W mld_H corr) nformat(%9.3f) nototal
table year if country=="FR", stat(mean mld_Y mld_W mld_H corr) nformat(%9.3f) nototal
table year if country=="UK", stat(mean mld_Y mld_W mld_H corr) nformat(%9.3f) nototal
table year if country=="US", stat(mean mld_Y mld_W mld_H corr) nformat(%9.3f) nototal

**********************************************************
*** figure 1 - main text
**********************************************************
preserve
collapse mld* corr, by(country year)
twoway line mld_Y year if country == "US" , clcolor(black) lwidth(medium) ti("Weekly earnings") yti("mean log deviation") xti("year") legend(on col(1) order (1 "US" 2 "UK" 3 "DE" 4 "FR")) || line mld_Y year if country == "UK" , lpattern(longdash) clcolor(black) lwidth(medium) || line mld_Y year if country == "DE" , lpattern(dash_dot) clcolor(black) lwidth(medium) || line mld_Y year if country == "FR" , lpattern(shortdash) clcolor(black) lwidth(medium) name(graph_mld_Y, replace)

twoway line mld_W year if country == "US" , clcolor(black) lwidth(medium) ti("Hourly wages") yti("mean log deviation") xti("year") legend(on col(1) order (1 "US" 2 "UK" 3 "DE" 4 "FR")) || line mld_W year if country == "UK" , lpattern(longdash) clcolor(black) lwidth(medium) || line mld_W year if country == "DE" , lpattern(dash_dot) clcolor(black) lwidth(medium) || line mld_W year if country == "FR" , lpattern(shortdash) clcolor(black) lwidth(medium) name(graph_mld_W, replace)

twoway line mld_H year if country == "US" , clcolor(black) lwidth(medium) ti("Hours worked") yti("mean log deviation") xti("year") legend(on col(1) order (1 "US" 2 "UK" 3 "DE" 4 "FR")) || line mld_H year if country == "UK" , lpattern(longdash) clcolor(black) lwidth(medium) || line mld_H year if country == "DE" , lpattern(dash_dot) clcolor(black) lwidth(medium) || line mld_H year if country == "FR" , lpattern(shortdash) clcolor(black) lwidth(medium) name(graph_mld_H, replace)

twoway line corr year if country == "US" , yli(0) clcolor(black) lwidth(medium) ti("Covariance term") yti("covariance term") xti("year") legend(on col(1) order (1 "US" 2 "UK" 3 "DE" 4 "FR")) || line corr year if country == "UK" , lpattern(longdash) clcolor(black) lwidth(medium) || line corr year if country == "DE" , lpattern(dash_dot) clcolor(black) lwidth(medium) || line corr year if country == "FR" , lpattern(shortdash) clcolor(black) lwidth(medium) name(graph_corr, replace)

graph combine graph_mld_Y graph_mld_W graph_mld_H graph_corr, saving("$result\figure1", replace)
graph export "$result\figure1.eps", replace
graph export "$result\figure1.jpg", replace
restore

***********************************************************
*** inequality decomposition for population subgroups 
***********************************************************
tab cnt female, m
tab cnt old, m
tab cnt minority, m

gen mld_Y_men=.
gen mld_W_men=.
gen mld_H_men=.
gen corr_men=.
gen mld_Y_women=.
gen mld_W_women=.
gen mld_H_women=.
gen corr_women=.
gen female_sh=.

gen mld_Y_you=.
gen mld_W_you=.
gen mld_H_you=.
gen corr_you=.
gen mld_Y_old=.
gen mld_W_old=.
gen mld_H_old=.
gen corr_old=.
gen old_sh=.

gen mld_Y_nat=.
gen mld_W_nat=.
gen mld_H_nat=.
gen corr_nat=.
gen mld_Y_min=.
gen mld_W_min=.
gen mld_H_min=.
gen corr_min=.
gen min_sh=.

*** Germany
forvalues y = 1991/2017 {
ainequal Y W H [aw=weight] if cnt==1&year==`y'&female==0, mld
replace mld_Y_men=`r(mld_1)' if cnt==1&year==`y'
replace mld_W_men=`r(mld_2)' if cnt==1&year==`y'
replace mld_H_men=`r(mld_3)' if cnt==1&year==`y'
replace corr_men=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
ainequal Y W H [aw=weight] if cnt==1&year==`y'&female==1, mld
replace mld_Y_women=`r(mld_1)' if cnt==1&year==`y'
replace mld_W_women=`r(mld_2)' if cnt==1&year==`y'
replace mld_H_women=`r(mld_3)' if cnt==1&year==`y'
replace corr_women=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
sum female if Y!=.&cnt==1&year==`y'
replace female_sh=r(mean) if cnt==1&year==`y'

ainequal Y W H [aw=weight] if cnt==1&year==`y'&old==0, mld
replace mld_Y_you=`r(mld_1)' if cnt==1&year==`y'
replace mld_W_you=`r(mld_2)' if cnt==1&year==`y'
replace mld_H_you=`r(mld_3)' if cnt==1&year==`y'
replace corr_you=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
ainequal Y W H [aw=weight] if cnt==1&year==`y'&old==1, mld
replace mld_W_old=`r(mld_1)' if cnt==1&year==`y'
replace mld_W_old=`r(mld_2)' if cnt==1&year==`y'
replace mld_H_old=`r(mld_3)' if cnt==1&year==`y'
replace corr_old=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
sum old if Y!=.&cnt==1&year==`y'
replace old_sh=r(mean) if cnt==1&year==`y'

ainequal Y W H [aw=weight] if cnt==1&year==`y'&minority==0, mld
replace mld_Y_nat=`r(mld_1)' if cnt==1&year==`y'
replace mld_W_nat=`r(mld_2)' if cnt==1&year==`y'
replace mld_H_nat=`r(mld_3)' if cnt==1&year==`y'
replace corr_nat=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
ainequal Y W H [aw=weight] if cnt==1&year==`y'&minority==1, mld
replace mld_W_min=`r(mld_1)' if cnt==1&year==`y'
replace mld_W_min=`r(mld_2)' if cnt==1&year==`y'
replace mld_H_min=`r(mld_3)' if cnt==1&year==`y'
replace corr_min=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==1&year==`y'
sum minority if Y!=.&cnt==1&year==`y'
replace min_sh=r(mean) if cnt==1&year==`y'

display "year==`y'"
}

*** France
forvalues y = 1990/2017 {
ainequal Y W H [aw=weight] if cnt==2&year==`y'&female==0, mld
replace mld_Y_men=`r(mld_1)' if cnt==2&year==`y'
replace mld_W_men=`r(mld_2)' if cnt==2&year==`y'
replace mld_H_men=`r(mld_3)' if cnt==2&year==`y'
replace corr_men=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
ainequal Y W H [aw=weight] if cnt==2&year==`y'&female==1, mld
replace mld_Y_women=`r(mld_1)' if cnt==2&year==`y'
replace mld_W_women=`r(mld_2)' if cnt==2&year==`y'
replace mld_H_women=`r(mld_3)' if cnt==2&year==`y'
replace corr_women=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
sum female if Y!=.&cnt==2&year==`y'
replace female_sh=r(mean) if cnt==2&year==`y'

ainequal Y W H [aw=weight] if cnt==2&year==`y'&old==0, mld
replace mld_Y_you=`r(mld_1)' if cnt==2&year==`y'
replace mld_W_you=`r(mld_2)' if cnt==2&year==`y'
replace mld_H_you=`r(mld_3)' if cnt==2&year==`y'
replace corr_you=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
ainequal Y W H [aw=weight] if cnt==2&year==`y'&old==1, mld
replace mld_W_old=`r(mld_1)' if cnt==2&year==`y'
replace mld_W_old=`r(mld_2)' if cnt==2&year==`y'
replace mld_H_old=`r(mld_3)' if cnt==2&year==`y'
replace corr_old=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
sum old if Y!=.&cnt==2&year==`y'
replace old_sh=r(mean) if cnt==2&year==`y'

ainequal Y W H [aw=weight] if cnt==2&year==`y'&minority==0, mld
replace mld_Y_nat=`r(mld_1)' if cnt==2&year==`y'
replace mld_W_nat=`r(mld_2)' if cnt==2&year==`y'
replace mld_H_nat=`r(mld_3)' if cnt==2&year==`y'
replace corr_nat=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
ainequal Y W H [aw=weight] if cnt==2&year==`y'&minority==1, mld
replace mld_W_min=`r(mld_1)' if cnt==2&year==`y'
replace mld_W_min=`r(mld_2)' if cnt==2&year==`y'
replace mld_H_min=`r(mld_3)' if cnt==2&year==`y'
replace corr_min=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==2&year==`y'
sum minority if Y!=.&cnt==2&year==`y'
replace min_sh=r(mean) if cnt==2&year==`y'

display "year==`y'"
}

*** UK
forvalues y = 1991/2016 {
ainequal Y W H [aw=weight] if cnt==3&year==`y'&female==0, mld
replace mld_Y_men=`r(mld_1)' if cnt==3&year==`y'
replace mld_W_men=`r(mld_2)' if cnt==3&year==`y'
replace mld_H_men=`r(mld_3)' if cnt==3&year==`y'
replace corr_men=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
ainequal Y W H [aw=weight] if cnt==3&year==`y'&female==1, mld
replace mld_Y_women=`r(mld_1)' if cnt==3&year==`y'
replace mld_W_women=`r(mld_2)' if cnt==3&year==`y'
replace mld_H_women=`r(mld_3)' if cnt==3&year==`y'
replace corr_women=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
sum female if Y!=.&cnt==3&year==`y'
replace female_sh=r(mean) if cnt==3&year==`y'

ainequal Y W H [aw=weight] if cnt==3&year==`y'&old==0, mld
replace mld_Y_you=`r(mld_1)' if cnt==3&year==`y'
replace mld_W_you=`r(mld_2)' if cnt==3&year==`y'
replace mld_H_you=`r(mld_3)' if cnt==3&year==`y'
replace corr_you=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
ainequal Y W H [aw=weight] if cnt==3&year==`y'&old==1, mld
replace mld_W_old=`r(mld_1)' if cnt==3&year==`y'
replace mld_W_old=`r(mld_2)' if cnt==3&year==`y'
replace mld_H_old=`r(mld_3)' if cnt==3&year==`y'
replace corr_old=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
sum old if Y!=.&cnt==3&year==`y'
replace old_sh=r(mean) if cnt==3&year==`y'

ainequal Y W H [aw=weight] if cnt==3&year==`y'&minority==0, mld
replace mld_Y_nat=`r(mld_1)' if cnt==3&year==`y'
replace mld_W_nat=`r(mld_2)' if cnt==3&year==`y'
replace mld_H_nat=`r(mld_3)' if cnt==3&year==`y'
replace corr_nat=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
ainequal Y W H [aw=weight] if cnt==3&year==`y'&minority==1, mld
replace mld_W_min=`r(mld_1)' if cnt==3&year==`y'
replace mld_W_min=`r(mld_2)' if cnt==3&year==`y'
replace mld_H_min=`r(mld_3)' if cnt==3&year==`y'
replace corr_min=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==3&year==`y'
sum minority if Y!=.&cnt==3&year==`y'
replace min_sh=r(mean) if cnt==3&year==`y'

display "year==`y'"
}

*** US
forvalues y = 1989/2019 {
ainequal Y W H [aw=weight] if cnt==4&year==`y'&female==0, mld
replace mld_Y_men=`r(mld_1)' if cnt==4&year==`y'
replace mld_W_men=`r(mld_2)' if cnt==4&year==`y'
replace mld_H_men=`r(mld_3)' if cnt==4&year==`y'
replace corr_men=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
ainequal Y W H [aw=weight] if cnt==4&year==`y'&female==1, mld
replace mld_Y_women=`r(mld_1)' if cnt==4&year==`y'
replace mld_W_women=`r(mld_2)' if cnt==4&year==`y'
replace mld_H_women=`r(mld_3)' if cnt==4&year==`y'
replace corr_women=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
sum female if Y!=.&cnt==4&year==`y'
replace female_sh=r(mean) if cnt==4&year==`y'

ainequal Y W H [aw=weight] if cnt==4&year==`y'&old==0, mld
replace mld_Y_you=`r(mld_1)' if cnt==4&year==`y'
replace mld_W_you=`r(mld_2)' if cnt==4&year==`y'
replace mld_H_you=`r(mld_3)' if cnt==4&year==`y'
replace corr_you=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
ainequal Y W H [aw=weight] if cnt==4&year==`y'&old==1, mld
replace mld_W_old=`r(mld_1)' if cnt==4&year==`y'
replace mld_W_old=`r(mld_2)' if cnt==4&year==`y'
replace mld_H_old=`r(mld_3)' if cnt==4&year==`y'
replace corr_old=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
sum old if Y!=.&cnt==4&year==`y'
replace old_sh=r(mean) if cnt==4&year==`y'

ainequal Y W H [aw=weight] if cnt==4&year==`y'&minority==0, mld
replace mld_Y_nat=`r(mld_1)' if cnt==4&year==`y'
replace mld_W_nat=`r(mld_2)' if cnt==4&year==`y'
replace mld_H_nat=`r(mld_3)' if cnt==4&year==`y'
replace corr_nat=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
ainequal Y W H [aw=weight] if cnt==4&year==`y'&minority==1, mld
replace mld_W_min=`r(mld_1)' if cnt==4&year==`y'
replace mld_W_min=`r(mld_2)' if cnt==4&year==`y'
replace mld_H_min=`r(mld_3)' if cnt==4&year==`y'
replace corr_min=`r(mld_1)'-`r(mld_2)'-`r(mld_3)' if cnt==4&year==`y'
sum minority if Y!=.&cnt==4&year==`y'
replace min_sh=r(mean) if cnt==4&year==`y'

display "year==`y'"
}

*** intermediate saving to avoid the repetition of the loops
preserve
bysort cnt year: g n=_n
keep if n==1
keep country cnt year mld_Y mld_W mld_H corr mld_Y_men mld_W_men mld_H_men corr_men mld_Y_women mld_W_women mld_H_women corr_women female_sh mld_Y_you mld_W_you mld_H_you corr_you mld_Y_old mld_W_old mld_H_old corr_old old_sh mld_Y_nat mld_W_nat mld_H_nat corr_nat mld_Y_min mld_W_min mld_H_min corr_min min_sh
save "$result\hours_inequality_indices.dta", replace
restore

**********************************************************
*** figures 2a-2b-3a-3b - main text
**********************************************************
use "$result\hours_inequality_indices.dta", clear
twoway line mld_H_men mld_H_women year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("Germany") name(sex_H_DE, replace) scale(0.8)
twoway line mld_H_nat mld_H_min year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("Germany") name(race_H_DE, replace) scale(0.8) 
twoway line corr_men corr_women year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("Germany") name(sex_corr_DE, replace) yli(0) scale(0.8)
twoway line corr_nat corr_min year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("Germany") name(race_corr_DE, replace) yli(0) scale(0.8)

twoway line mld_H_men mld_H_women year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("United States") name(sex_H_US, replace) scale(0.8)
twoway line mld_H_nat mld_H_min year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "white") label(2 "minority") ) xti("year") ti("United States") name(race_H_US, replace) scale(0.8) 
twoway line corr_men corr_women year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("United States") name(sex_corr_US, replace) yli(0) scale(0.8)
twoway line corr_nat corr_min year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "white") label(2 "minority") ) xti("year") ti("United States") name(race_corr_US, replace) yli(0) scale(0.8)

twoway line mld_H_men mld_H_women year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("United Kingdom") name(sex_H_UK, replace) scale(0.8)
twoway line mld_H_nat mld_H_min year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("United Kingdom") name(race_H_UK, replace) scale(0.8) 
twoway line corr_men corr_women year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("United Kingdom") name(sex_corr_UK, replace) yli(0) scale(0.8)
twoway line corr_nat corr_min year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("United Kingdom") name(race_corr_UK, replace) yli(0) scale(0.8)

twoway line mld_H_men mld_H_women year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("France") name(sex_H_FR, replace) scale(0.8)
twoway line mld_H_nat mld_H_min year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("France") name(race_H_FR, replace) scale(0.8) 
twoway line corr_men corr_women year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("France") name(sex_corr_FR, replace) yli(0) scale(0.8)
twoway line corr_nat corr_min year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("France") name(race_corr_FR, replace) yli(0) scale(0.8)

graph combine sex_H_US sex_H_UK sex_H_DE sex_H_FR, ti("Inequality in hours by sex") saving("$result\figure2a", replace) ycomm
graph export "$result\figure2a.eps", replace
graph export "$result\figure2a.jpg", replace

graph combine race_H_US race_H_UK race_H_DE race_H_FR, ti("Inequality in hours by race/origin") saving("$result\figure2b", replace) ycomm
graph export "$result\figure2b.eps", replace
graph export "$result\figure2b.jpg", replace

graph combine sex_corr_US sex_corr_UK sex_corr_DE sex_corr_FR, ti("Covariance wage-hours by sex") saving("$result\figure3a", replace) ycomm
graph export "$result\figure3a.eps", replace
graph export "$result\figure3a.jpg", replace

graph combine race_corr_US race_corr_UK race_corr_DE race_corr_FR, ti("Covariance wage-hours by race/origin") saving("$result\figure3b", replace) ycomm
graph export "$result\figure3b.eps", replace
graph export "$result\figure3b.jpg", replace

**********************************************************
*** figures A1a-A1b-A1c-A1d-A1e in Appendix
**********************************************************
twoway line mld_W_men mld_W_women year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("Germany") name(sex_W_DE, replace) scale(0.8)
twoway line mld_W_nat mld_W_min year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("Germany") name(race_W_DE, replace) scale(0.8) 

twoway line mld_W_men mld_W_women year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("United States") name(sex_W_US, replace) scale(0.8)
twoway line mld_W_nat mld_W_min year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "white") label(2 "minority") ) xti("year") ti("United States") name(race_W_US, replace) scale(0.8) 

twoway line mld_W_men mld_W_women year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("United Kingdom") name(sex_W_UK, replace) scale(0.8)
twoway line mld_W_nat mld_W_min year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign") ) xti("year") ti("United Kingdom") name(race_W_UK, replace) scale(0.8) 

twoway line mld_W_men mld_W_women year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "men") label(2 "women") ) xti("year") ti("France") name(sex_W_FR, replace) scale(0.8)
twoway line mld_W_nat mld_W_min year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "native") label(2 "foreign")  ) xti("year") ti("France") name(race_W_FR, replace) scale(0.8) 

twoway line mld_W_you mld_W_old year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("Germany") name(age_W_DE, replace) scale(0.8)
twoway line mld_H_you mld_H_old year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("Germany") name(age_H_DE, replace) scale(0.8) 
twoway line corr_you corr_old year if country=="DE", clcolor(black black) lwidth(medium medium) lpattern(solid longdash)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("Germany") name(age_corr_DE, replace) yli(0) scale(0.8)

twoway line mld_W_you mld_W_old year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("United States") name(age_W_US, replace) scale(0.8)
twoway line mld_H_you mld_H_old year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("United States") name(age_H_US, replace) scale(0.8) 
twoway line corr_you corr_old year if country=="US", clcolor(black black) lwidth(medium medium) lpattern(solid longdash)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("United States") name(age_corr_US, replace) yli(0) scale(0.8)

twoway line mld_W_you mld_W_old year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("United Kingdom") name(age_W_UK, replace) scale(0.8)
twoway line mld_H_you mld_H_old year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("United Kingdom") name(age_H_UK, replace) scale(0.8) 
twoway line corr_you corr_old year if country=="UK", clcolor(black black) lwidth(medium medium) lpattern(solid longdash)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("United KIngdom") name(age_corr_UK, replace) yli(0) scale(0.8)

twoway line mld_W_you mld_W_old year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot) legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("France") name(age_W_FR, replace) scale(0.8)
twoway line mld_H_you mld_H_old year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid dash_dot)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("France") name(age_H_FR, replace) scale(0.8) 
twoway line corr_you corr_old year if country=="FR", clcolor(black black) lwidth(medium medium) lpattern(solid longdash)  legend(col(1) label(1 "young") label(2 "old")) xti("year") ti("France") name(age_corr_FR, replace) yli(0) scale(0.8)

graph combine sex_W_US sex_W_UK sex_W_DE sex_W_FR, ti("Inequality in hourly wage by sex") saving("$result\figureA1a", replace) ycomm
graph export "$result\figureA1a.eps", replace
graph export "$result\figureA1a.jpg", replace

graph combine race_W_US race_W_UK race_W_DE race_W_FR, ti("Inequality in hourly wage by race/origin") saving("$result\figureA1b", replace) ycomm
graph export "$result\figureA1b.eps", replace
graph export "$result\figureA1b.jpg", replace

graph combine age_W_US age_W_UK age_W_DE age_W_FR, ti("Inequality in hourly wage by age group") saving("$result\figureA1c", replace) ycomm
graph export "$result\figureA1c.eps", replace
graph export "$result\figureA1c.jpg", replace

graph combine age_H_US age_H_UK age_H_DE age_H_FR, ti("Inequality in hours by age group") saving("$result\figureA1d", replace) ycomm
graph export "$result\figureA1d.eps", replace
graph export "$result\figureA1d.jpg", replace

graph combine age_corr_US age_corr_UK age_corr_DE age_corr_FR, ti("Covariance wage-hours by age") saving("$result\figureA1e", replace) ycomm
graph export "$result\figureA1e.eps", replace
graph export "$result\figureA1e.jpg", replace

**********************************************************
*** selection equation into employment - table A7 in the appendix
**********************************************************
/* exclusion restriction is based on partner/household income, controlling where other members are selfemployed, number of children and being single - the employment selection equation excludes self-employed (because they selfdecleare being employed but we do not observe any earnings - using minority or foreign_born does not make any difference, so we retain foreign born - estimates controls for country x survey and erros are clustered by country x survey - sample weights - we have explored the possibility of differentiating the selection equation by gender, but the relevant variables bear the same sign and significance for men and women except the number of children (positive for men and negative for women) - the computed mills ratio estimated by gender and estimated over the entire sample are highly correlated (0.89 correlation) - we decide to use the entire sample ratio in the sequel
*/

use "$result\workfile.dta", clear
egen clu=group(country year)
global x1 = "female age age2 foreign i.education"
global x2 = "female age age2 minority i.education"
global y1 = "i.jobtype public"
global y2 = "hhincome hhself single nchild"
global z = "i.cnt#i.year"

ssc install outreg2

reg employed female age age2 foreign edu2 edu3 hhincome hhself single nchild $z if emp!="self-employed" [aw=weight], clu(clu)
outreg2 female age age2 foreign edu2 edu3 hhincome hhself single nchild using "$result\tableA7.out", cti(all no self) bdec(3) bracket aster(coef) se replace addnote(weighed - errors clustered by country x year - year x country dummies included) label
predict xb_all, xb
gen millsratio=normalden(xb_all)/normal(xb_all)
label var millsratio "Mills ratio"

reg employed age age2 foreign edu2 edu3 hhincome hhself single nchild $z if female==0&emp!="self-employed" [aw=weight], clu(clu)
outreg2 age age2 foreign edu2 edu3 hhincome hhself single nchild using "$result\tableA7.out", cti(men no self) bdec(3) bracket aster(coef) se label append 
predict xb_men, xb
gen millsratiom=normalden(xb_men)/normal(xb_men) if female==0&emp!="self-employed"

reg employed age age2 foreign edu2 edu3 hhincome hhself single nchild $z if female==1&emp!="self-employed"  [aw=weight], clu(clu)
outreg2 age age2 foreign edu2 edu3 hhincome hhself single nchild using "$result\tableA7.out", cti(women no self) bdec(3) bracket aster(coef) se label append 
predict xb_women, xb
gen millsratiow=normalden(xb_women)/normal(xb_women) if female==1&emp!="self-employed"
gen millsratiosex=millsratiom
replace millsratiosex=millsratiow if female==1
label var millsratiosex "Mills ratio by sex"
corr millsratio millsratiosex
drop millsratiom millsratiow 

**********************************************************
*** individual determinants of hours without institutions - table A8 in the appendix
**********************************************************

reg h w female wxf age age2 foreign $z [aw=weight], clu(clu)
outreg2 w female wxf age age2 foreign using "$result\tableA8.out", cti(demographics no mills) bdec(3) bracket aster(coef) se replace addnote(weighed - erros clustered by country x year - year x country dummies included) label

reg h w female wxf age age2 foreign millsratio $z [aw=weight], clu(clu)
outreg2 w female wxf age age2 foreign millsratio using "$result\tableA8.out", cti(demographics mills) bdec(3) bracket aster(coef) se append label

reg h w female wxf age age2 foreign millsratiosex $z [aw=weight], clu(clu)
outreg2 w female wxf age age2 foreign millsratiosex using "$result\tableA8.out", cti(demographics mills sex) bdec(3) bracket aster(coef) se append label

reg h w female wxf age age2 foreign edu2 edu3 millsratio $z [aw=weight], clu(clu) 
outreg2 w female wxf age age2 edu2 edu3 foreign millsratio using "$result\tableA8.out", cti(edu mills) bdec(3) bracket aster(coef) se append label

reg h w female wxf age age2 foreign edu2 edu3 job2-job9 public millsratio $z [aw=weight], clu(clu)
outreg2 w female wxf age age2 foreign edu2 edu3 job2-job9 public millsratio using "$result\tableA8.out", cti(edu+job mills) bdec(3) bracket aster(coef) se append label

reg h w female wxf age age2 minority edu2 edu3 job2-job9 public millsratio $z [aw=weight], clu(clu) 
outreg2 w female wxf age age2 minority edu2 edu3 job2-job9 public millsratio using "$result\tableA8.out", cti(edu+job+minority mills) bdec(3) bracket aster(coef) se append label

**********************************************************
*** figure 4 elasticity in main text
**********************************************************

forvalues z = 1989/2019 {
gen y`z'=year==`z'
}
forvalues s = 1/4 {
gen cnt`s'=cnt==`s'
}
forvalues s = 1/4 {
forvalues z = 1989/2019 {
gen mwcnt`s'y`z'=(1-female)*w*cnt`s'*y`z' 
gen fwcnt`s'y`z'=female*w*cnt`s'*y`z' 
} 
}

* main equation - with countryxyearxgender dummies - job controls added
reg h $x1 mwcnt* fwcnt* $y1 millsratio $z [aw=weight], robust 

keep in 1/1

forvalues s = 1/4 {
forvalues z = 1989/2019 {
gen b_mwcnt`s'y`z'=.
gen b_fwcnt`s'y`z'=.
} 
}

forvalues s = 1/4 {
forvalues z = 1989/2019 {
replace b_mwcnt`s'y`z'=_b[mwcnt`s'y`z']
replace b_fwcnt`s'y`z'=_b[fwcnt`s'y`z']
} 
}

keep b_*
ren b_* *

forvalues s = 1/4 {
foreach z in m f {
preserve 
gen cnt=`s'
reshape long `z'wcnt`s'y, i(cnt) j(year 1989-2019)
keep cnt year `z'wcnt`s'
gen sex="`z'"
ren `z'wcnt`s' welasticity
save "$result\'`z'cnt`s''.dta", replace
restore
}
}

preserve
clear
forvalues s = 1/4 {
foreach z in m f {
append using "$result\'`z'cnt`s''.dta"
}
}
replace welasticity=. if welasticity==0
label define cnt 1 "Germany" 2 "France" 3 "United Kingdom" 4 "United States"
label value cnt cnt

line welast year if sex=="m", yti(hours elasticity wrt hourly wages) clcolor(black) lwidth(medium) lpattern(solid) by(cnt,  ///
note("Note: Estimates include controls for age and age², education, foreign born, job skill requirement, " "sector and country x year dummies - self-selection into employment controlled using household" "level information (single, number of children, others' incomes)")) scale(0.8) yli(0) || line welast year if sex=="f", clcolor(black) lwidth(medium) lpattern(longdash) by(cnt) legend(label(1 "men") label(2 "women")) saving("$result\figure4.gph", replace)
graph export "$result\figure4.eps", replace
graph export "$result\figure4.jpg", replace

erase "$result\'fcnt1'.dta"
erase "$result\'fcnt2'.dta"
erase "$result\'fcnt3'.dta"
erase "$result\'fcnt4'.dta"
erase "$result\'mcnt1'.dta"
erase "$result\'mcnt2'.dta"
erase "$result\'mcnt3'.dta"
erase "$result\'mcnt4'.dta"
restore

**********************************************************
***** LABOUR MARKET INSTITUTIONS 
**********************************************************

*** import supplementary files with labour market institutions and demand shocks - plot the shock variables - figureA2 and figureA3 in the appendix
clear
import excel "$result\shocks_oecd.xlsx", sheet("export") firstrow
destring year kaitz import export output_volatility, replace
encode country, g(cnt)
saveold "$result\shocks.dta", replace

twoway (line ud_v year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line ud_v year if country=="FR",  clcolor(black) lpattern(shortdash)) (line ud_v year if country=="UK",  clcolor(black) lpattern(longdash)) (line ud_v year if country=="US", clcolor(black) lpattern(solid)  clcolor(black) lpattern(solid)), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Union density) yti(" ") saving(tmp1, replace)

twoway (line adjcov_v year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line adjcov_v year if country=="FR",  clcolor(black) lpattern(shortdash)) (line adjcov_v year if country=="UK",  clcolor(black) lpattern(longdash)) (line adjcov_v year if country=="US", clcolor(black) lpattern(solid)  clcolor(black) lpattern(solid)), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Bargaining coverage) yti(" ") saving(tmp2, replace)

twoway (line work_reg year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line work_reg year if country=="FR",  clcolor(black) lpattern(shortdash)) (line work_reg year if country=="UK",  clcolor(black) lpattern(longdash)) (line work_reg year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Work regulation - composite index) yti(" ") saving(tmp3, replace)

twoway (line epl_per year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line epl_per year if country=="FR",  clcolor(black) lpattern(shortdash)) (line epl_per year if country=="UK",  clcolor(black) lpattern(longdash)) (line epl_per year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(EPL permanent contract) yti(" ") saving(tmp4, replace)

twoway (line epl_tmp year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line epl_tmp year if country=="FR",  clcolor(black) lpattern(shortdash)) (line epl_tmp year if country=="UK",  clcolor(black) lpattern(longdash)) (line epl_tmp year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(EPL temporary contract) yti(" ") saving(tmp5, replace)

twoway (line kaitz year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line kaitz year if country=="FR",  clcolor(black) lpattern(shortdash)) (line kaitz year if country=="UK",  clcolor(black) lpattern(longdash)) (line kaitz year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Minimum/median wage) yti(" ") saving(tmp6, replace)

twoway (line ub year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line ub year if country=="FR",  clcolor(black) lpattern(shortdash)) (line ub year if country=="UK",  clcolor(black) lpattern(longdash)) (line ub year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Unemployment subsidy) yti(" ") saving(tmp7, replace)

twoway (line leave year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line leave year if country=="FR",  clcolor(black) lpattern(shortdash)) (line leave year if country=="UK",  clcolor(black) lpattern(longdash)) (line leave year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Parental leave) yti(" ") saving(tmp8, replace)

graph combine tmp1.gph tmp2.gph tmp3.gph tmp4.gph , cols(2) scale(0.8) saving("$result\figureA2.gph", replace)
graph export "$result\figureA2.eps", replace
graph export "$result\figureA2.jpg", replace

graph combine tmp5.gph tmp6.gph tmp7.gph tmp8.gph, cols(2) scale(0.8) saving("$result\figureA2b.gph", replace)
graph export "$result\figureA2b.eps", replace
graph export "$result\figureA2b.jpg", replace

twoway (line output year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line output year if country=="FR",  clcolor(black) lpattern(shortdash)) (line output year if country=="UK",  clcolor(black) lpattern(longdash)) (line output year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Output volatility) yti(" ") saving(tmp9, replace)

twoway (line open year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line open year if country=="FR",  clcolor(black) lpattern(shortdash)) (line open year if country=="UK",  clcolor(black) lpattern(longdash)) (line open year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Trade openneness) yti(" ") saving(tmp10, replace)

twoway (line invest_gdp year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line invest_gdp year if country=="FR",  clcolor(black) lpattern(shortdash)) (line invest_gdp year if country=="UK",  clcolor(black) lpattern(longdash)) (line invest_gdp year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(Investment/GDP) yti(" ") saving(tmp11, replace)

twoway (line ict year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line ict year if country=="FR",  clcolor(black) lpattern(shortdash)) (line ict year if country=="UK",  clcolor(black) lpattern(longdash)) (line ict year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(ICT investment) yti(" ") saving(tmp12, replace)

twoway (line int_property year if country=="DE",  clcolor(black) lpattern(dash_dot)) (line int_property year if country=="FR",  clcolor(black) lpattern(shortdash)) (line int_property year if country=="UK",  clcolor(black) lpattern(longdash)) (line int_property year if country=="US", clcolor(black) lpattern(solid) ), legend(label(1 Germany) label(2 France) label(3 UK) label(4 US)) ti(R&D investment) yti(" ") saving(tmp13, replace)

graph combine tmp9.gph tmp10.gph tmp11.gph tmp12.gph tmp13.gph , scale(0.8) cols(2) saving("$result\figureA3.gph", replace)
graph export "$result\figureA3.eps", replace
graph export "$result\figureA3.jpg", replace

erase tmp1.gph
erase tmp2.gph
erase tmp3.gph
erase tmp4.gph
erase tmp5.gph
erase tmp6.gph
erase tmp7.gph
erase tmp8.gph
erase tmp9.gph
erase tmp10.gph
erase tmp11.gph
erase tmp12.gph
erase tmp13.gph

**********************************************************
*** table 1 in main text - full version in table A9 in the Appendix
**********************************************************
use "$result\workfile.dta", clear
egen clu=group(country year)

global x1 = "female age age2 foreign i.education"
global x2 = "female age age2 minority i.education"
global y1 = "i.jobtype public"
global y2 = "hhincome hhself single nchild"
global z = "i.cnt#i.year"

reg employed female age age2 foreign edu2 edu3 hhincome hhself single nchild $z if emp!="self-employed" [aw=weight], clu(clu)
predict xb_all, xb
gen millsratio=normalden(xb_all)/normal(xb_all)
label var millsratio "Mills ratio"

merge m:1 cnt year using "$result\shocks.dta"
drop if _merge==2
drop _merge

global x1 = "age age2 foreign i.education"
global x2 = "age age2 minority i.education"
global y1 = "i.jobtype public"
global z1 = "i.cnt i.year"
global z2 = "i.cnt#i.year"

*** coping with missing values for minimum wage: equal to zero or equal to sample mean 
replace kaitz=0 if kaitz==.

g wud=w*ud
g wepl_per=w*epl_per
g wepl_tmp=w*epl_tmp
g wkaitz=w*kaitz
g wub=w*ub
g wleave=w*leave
g wopen=w*open
g winvest=w*invest
g wict=w*ict
g wpro=w*int_property
g woutput=w*output_volatility
g wud_v=w*ud_v
g wadjcov_v=w*adjcov_v
g wwork_reg=w*work_reg

label var ud "Union density (OECD)"
label var wud "Union den.xWage"
label var ud_v "Union density (AIAS)"
label var wud_v "Union den.xWage"
label var adjcov "Bargaining coverage"
label var wadjcov "Barg.coveragexWage"
label var work_reg "Work regulation"
label var wwork_reg "work reg.xWage"
label var epl_per "EPL Permanent"
label var wepl_per "EPL Per.xWage"
label var epl_tmp "EPL Temporary"
label var wepl_tmp "EPL Tem.xWage"
label var kaitz "Minimum wage"
label var wkaitz "Min.wagexWage"
label var ub "Unemp. Benefit"
label var wub "Un.Ben.xWage"
label var leave "Parental leave"
label var wleave "Par.Leav.xWage"
label var output "Output volatility"
label var woutput  "Out.Vol.xWage"
label var open "Trade Openness"
label var wopen "Tr.Open.xWage"
label var invest "Investment"
label var winvest "InvestmentxWage"
label var ict "Invest. ICT"
label var wict "Inv.ICTxWage" 
label var int_pro "Invest. Intel.Prop."
label var wpro "In.Int.Pr.xWage"

reg h w female wxf ud_v wud_v adjcov wadjcov work_reg wwork_reg epl_per wepl_per epl_tmp wepl_tmp kaitz wkaitz ub wub leave wleave output woutput open wopen invest winvest ict wict int_pro wpro $x1 millsratio $z1 [aw=weight], clu(clu)
outreg2 w female wxf ud_v wud_v adjcov wadjcov work_reg wwork_reg epl_per wepl_per epl_tmp wepl_tmp kaitz wkaitz ub wub leave wleave output woutput open wopen invest winvest ict wict int_pro wpro using "$result\table1.out", cti(kaitz=0 when missing - coutry&year) bdec(3) bracket aster(coef) se replace addnote("weighed - control for selfselection, age, foreign born and educational attainment" "Year and country dummies included - Errors clustered at country x year") label

reg h w female wxf ud_v wud_v adjcov wadjcov work_reg wwork_reg epl_per wepl_per epl_tmp wepl_tmp ub wub leave wleave output woutput open wopen invest winvest ict wict int_pro wpro $x1 millsratio $z1 [aw=weight], clu(clu)
outreg2 w female wxf ud_v wud_v adjcov wadjcov work_reg wwork_reg epl_per wepl_per epl_tmp wepl_tmp ub wub leave wleave output woutput open wopen invest winvest ict wict int_pro wpro using "$result\table1.out", cti(removing kaitz - coutry&year) bdec(3) bracket aster(coef) label se append 

reg h w female wxf ud_v wud_v adjcov wadjcov work_reg wwork_reg epl_per wepl_per epl_tmp wepl_tmp kaitz wkaitz ub wub leave wleave output woutput open wopen invest winvest ict wict int_pro wpro $x1 millsratio $z1 $y1 [aw=weight], clu(clu)
outreg2 w female wxf ud_v wud_v adjcov wadjcov work_reg wwork_reg epl_per wepl_per epl_tmp wepl_tmp kaitz wkaitz ub wub leave wleave output woutput open wopen invest winvest ict wict int_pro wpro using "$result\table1.out", cti(jobs included - coutry&year) bdec(3) bracket aster(coef) label se append 

reg h w female wxf wud_v wwork_reg wepl_tmp wkaitz wub wleave woutput wopen winvest $x1 millsratio $z2 [aw=weight], clu(clu)
outreg2 w female wxf wud_v wwork_reg wepl_tmp wkaitz wub wleave woutput wopen winvest using "$result\table1.out", cti(only significant - countryxyear) bdec(3) bracket aster(coef) label se append 

reg h w female wxf wud_v wwork_reg wepl_tmp wkaitz wub wleave woutput wopen winvest $x1 millsratio $z2 [aw=weight], robust beta
outreg2 w female wxf wud_v wwork_reg wepl_tmp wkaitz wub wleave woutput wopen winvest using "$result\table1.out", beta bracket cti(beta only significant - countryxyear) aster(beta) append label


**********************************************************
*** relative contribution of each shock to elasticity dynamics - table 2 in main text
**********************************************************
preserve
reg h w female wxf wud_v wwork_reg wepl_tmp wkaitz wub wleave woutput wopen winvest $x1 millsratio $z2 [aw=weight], clu(clu)
scalar bwud_v=_b[wud_v]
scalar bwwork_reg=_b[wwork_reg]
scalar bwepl_tmp=_b[wepl_tmp]
scalar bwkaitz=_b[wkaitz]
scalar bwub=_b[wub]
scalar bwleave=_b[wleave]
scalar bwoutptu=_b[woutput]
scalar bwopen=_b[wopen]
scalar bwinvest=_b[winvest]

bysort country year: gen tmp=_n
keep if tmp==1
drop tmp
keep if year==1991|year==2016
keep cnt year ud_v work_reg epl_per epl_tmp kaitz ub leave output open invest_gdp ict 
sum ud_v work_reg epl_per epl_tmp kaitz ub leave output open invest_gdp ict 
reshape wide ud_v work_reg epl_per epl_tmp kaitz ub leave output open invest_gdp ict , i(cnt) j(year)
g ud_v=bwud_v*(ud_v2016-ud_v1991)
g work_reg=bwwork_reg*(work_reg2016-work_reg1991)
g epl_tmp=bwepl_tmp*(epl_tmp2016-epl_tmp1991)
g kaitz=bwkaitz*(kaitz2016-kaitz1991)
g ub=bwub*(ub2016-ub1991)
g leave=bwleave*(leave2016-leave1991)
g volatility=bwoutptu*(output_volatility2016-output_volatility1991)
g open=bwopen*(open2016-open1991)
g invest=bwinvest*(invest_gdp2016-invest_gdp1991)

gen total=ud_v+work_reg+epl_tmp+kaitz+ub+leave+volatility+open+invest
mkmat cnt ud_v work_reg epl_tmp kaitz ub leave volatility open invest total, matrix(A)
matrix B = A'
**********************************************************
*** table 2 in main text
**********************************************************
matrix list B, format(%9.3f)
restore

log close