/* Code for the paper "Asymmetric Information, Adverse Selection and Online Disclosure: The Case of eBay Motors", by Gregory Lewis.
This version of the code is dated Jan 2010.  Takes as input the file ebaydatafinal.dta; and produces LaTeX tables as output, in the files
output1.tex, output2.tex, output3.tex, output4.tex*/

***************************************************************
******** Replication of All Tables ****************************
***************************************************************
clear all
set mem 800m

use ebaydatafinal

/* Text Variables */

gen ding_good = max(ding_barely, ding_minute, ding_negligible, ding_small, ding_limited, ding_almost, ding_minor, ding_little, ding_invisible)
gen ding_bad = max(ding_wide, ding_enormous, ding_noticeable, ding_large, ding_obvious, ding_major, ding_substantial, ding_visible, ding_huge, ding_medium, ding_big, ding_significant, ding_sizable, ding_vast)
gen ding_knowledge = max(ding_apparent, ding_known)
gen ding_negation = max(ding_no, ding_free, ding_never, ding_nothing)  //ding_zero,
gen ding_low = max(ding_seldom, ding_one, ding_rarely, ding_only, ding_hardly, ding_couple)
gen ding_high = max(ding_several, ding_much, ding_very, ding_extremely, ding_many, ding_some)
gen ding_pics = max(ding_pic, ding_photo)

gen dent_good = max(dent_barely, dent_minute, dent_negligible, ding_small, dent_limited, dent_almost, dent_minor, dent_little, dent_invisible)
gen dent_bad = max(dent_wide, dent_enormous, dent_noticeable, dent_large, dent_obvious, dent_major, dent_substantial, ding_visible, dent_huge, dent_medium, dent_big, dent_significant, dent_sizable, dent_vast)
gen dent_knowledge = max(dent_apparent, dent_known)
gen dent_negation = max(dent_no, dent_free, dent_never, dent_nothing)    // dent_zero,
gen dent_low = max(dent_seldom, dent_one, dent_rarely, dent_only, dent_hardly, dent_couple)
gen dent_high = max(dent_several, dent_much, dent_very, dent_extremely, dent_many, dent_some)
gen dent_pics = max(dent_pic, dent_photo)

gen crack_good = max(crack_barely, crack_minute, ding_negligible, crack_small, crack_limited, crack_almost, crack_minor, crack_little, crack_invisible)
gen crack_bad = max(ding_wide, crack_enormous, crack_noticeable, ding_large, crack_obvious, crack_major, crack_substantial, crack_visible, crack_huge, ding_medium, crack_big, crack_significant, crack_sizable, crack_vast)
gen crack_knowledge = max(crack_apparent, crack_known)
gen crack_negation = max(crack_no, crack_free, crack_never, crack_nothing)          //crack_zero, 
gen crack_low = max(crack_seldom, crack_one, crack_rarely, crack_only, crack_hardly, crack_couple)
gen crack_high = max(crack_several, crack_much, crack_very, crack_extremely, crack_many, crack_some)
gen crack_pics = max(crack_pic, crack_photo)

gen problem_good = max(problem_barely, problem_minute, problem_negligible, problem_small, problem_limited, problem_almost, problem_minor, problem_little, problem_invisible)
gen problem_bad = max(problem_wide, problem_enormous, problem_noticeable, problem_large, problem_obvious, problem_major, problem_substantial, problem_visible, problem_huge, problem_medium, problem_big, problem_significant, problem_sizable, problem_vast)
gen problem_knowledge = max(problem_apparent, problem_known)
gen problem_negation = max(problem_no, problem_free, problem_never, problem_nothing)   //problem_zero, 
gen problem_low = max(problem_seldom, ding_one, problem_rarely, problem_only, problem_hardly, problem_couple)
gen problem_high = max(problem_several, problem_much, problem_very, problem_extremely, problem_many, problem_some)
gen problem_pics = max(problem_pic, problem_photo)

gen rust_good = max(rust_barely, rust_minute, rust_negligible, rust_small, rust_limited, rust_almost, rust_minor, rust_little, rust_invisible)
gen rust_bad = max(rust_wide, rust_enormous, rust_noticeable, rust_large, rust_obvious, rust_major, rust_substantial, rust_visible, rust_huge, rust_medium, rust_big, rust_significant, rust_sizable, rust_vast)
gen rust_knowledge = max(rust_apparent, rust_known)
gen rust_negation = max(rust_no, rust_free,  rust_never, rust_nothing)        //rust_zero,
gen rust_low = max(rust_seldom, rust_one, rust_rarely, rust_only, rust_hardly, rust_couple)
gen rust_high = max(rust_several, rust_much, rust_very, rust_extremely, rust_many, rust_some)
gen rust_pics = max(rust_pic, rust_photo)

gen scratch_good = max(scratch_barely, scratch_minute, scratch_negligible, scratch_small, scratch_limited, scratch_almost, scratch_minor, scratch_little, scratch_invisible)
gen scratch_bad = max(scratch_wide, scratch_enormous, scratch_noticeable, scratch_large, scratch_obvious, scratch_major, scratch_substantial, scratch_visible, scratch_huge, scratch_medium, scratch_big, scratch_significant, scratch_sizable, scratch_vast)
gen scratch_knowledge = max(scratch_apparent, scratch_known)
gen scratch_negation = max(scratch_no, scratch_free, scratch_never, scratch_nothing)       //scratch_zero, 
gen scratch_low = max(scratch_seldom, scratch_one, scratch_rarely, scratch_only, scratch_hardly, scratch_couple)
gen scratch_high = max(scratch_several, scratch_much, scratch_very, scratch_extremely, scratch_many, scratch_some)
gen scratch_pics = max(scratch_pic, scratch_photo)

gen broken_good = max(broken_barely, broken_minute, broken_negligible, broken_small, broken_limited, broken_almost, broken_minor, broken_little, broken_invisible)
gen broken_bad = max(broken_wide, broken_enormous, broken_noticeable, broken_large, broken_obvious, broken_major, broken_substantial, broken_visible, broken_huge, broken_medium, broken_big, broken_significant, broken_sizable, broken_vast)
gen broken_knowledge = max(broken_apparent, broken_known)
gen broken_negation = max(broken_no, broken_free,  broken_never, broken_nothing)     //broken_zero,
gen broken_low = max(broken_seldom, broken_one, broken_rarely, broken_only, broken_hardly, broken_couple)
gen broken_high = max(broken_several, broken_much, broken_very, broken_extremely, broken_many, broken_some)
gen broken_pics = max(broken_pic, broken_photo)

gen ding_group = 0
replace ding_group = 1 if ding == 1 & ding_negation == 1 
replace ding_group = 2 if ding == 1 & ding_negation == 0 & (ding_good == 1 | ding_low == 1) & ding_high == 0 & ding_bad == 0
replace ding_group = 3 if ding == 1 & ding_negation == 0 & ding_good == 0 & ding_low == 0 & ding_bad == 0 & ding_high == 0
replace ding_group = 4 if ding == 1 & ding_negation == 0 & (ding_bad == 1 | ding_high == 1) & ding_low == 0 & ding_good == 0
replace ding_group = 5 if ding == 1 & ding_negation == 0 & ding_group == 0

gen scratch_group = 0
replace scratch_group = 1 if scratch == 1 & scratch_negation == 1 
replace scratch_group = 2 if scratch == 1 & scratch_negation == 0 & (scratch_good == 1 | scratch_low == 1) & scratch_high == 0 & scratch_bad == 0
replace scratch_group = 3 if scratch == 1 & scratch_negation == 0 & scratch_good == 0 & scratch_low == 0 & scratch_bad == 0 & scratch_high == 0
replace scratch_group = 4 if scratch == 1 & scratch_negation == 0 & (scratch_bad == 1 | scratch_high == 1) & scratch_low == 0 & scratch_good == 0
replace scratch_group = 5 if scratch == 1 & scratch_negation == 0 & scratch_group == 0

gen crack_group = 0
replace crack_group = 1 if crack == 1 & crack_negation == 1 
replace crack_group = 2 if crack == 1 & crack_negation == 0 & (crack_good == 1 | crack_low == 1) & crack_high == 0 & crack_bad == 0
replace crack_group = 3 if crack == 1 & crack_negation == 0 & crack_good == 0 & crack_low == 0 & crack_bad == 0 & crack_high == 0
replace crack_group = 4 if crack == 1 & crack_negation == 0 & (crack_bad == 1 | crack_high == 1) & crack_low == 0 & crack_good == 0
replace crack_group = 5 if crack == 1 & crack_negation == 0 & crack_group == 0

gen broken_group = 0
replace broken_group = 1 if broken == 1 & broken_negation == 1 
replace broken_group = 2 if broken == 1 & broken_negation == 0 & (broken_good == 1 | broken_low == 1) & broken_high == 0 & broken_bad == 0
replace broken_group = 3 if broken == 1 & broken_negation == 0 & broken_good == 0 & broken_low == 0 & broken_bad == 0 & broken_high == 0
replace broken_group = 4 if broken == 1 & broken_negation == 0 & (broken_bad == 1 | broken_high == 1) & broken_low == 0 & scratch_good == 0
replace broken_group = 5 if broken == 1 & broken_negation == 0 & broken_group == 0

gen dent_group = 0
replace dent_group = 1 if dent == 1 & dent_negation == 1 
replace dent_group = 2 if dent == 1 & dent_negation == 0 & (dent_good == 1 | dent_low == 1) & dent_high == 0 & dent_bad == 0
replace dent_group = 3 if dent == 1 & dent_negation == 0 & scratch_good == 0 & scratch_low == 0 & dent_bad == 0 & dent_high == 0
replace dent_group = 4 if dent == 1 & dent_negation == 0 & (dent_bad == 1 | dent_high == 1) & dent_low == 0 & dent_good == 0
replace dent_group = 5 if dent == 1 & dent_negation == 0 & dent_group == 0

gen problem_group = 0
replace problem_group = 1 if problem == 1 & problem_negation == 1 
replace problem_group = 2 if problem == 1 & problem_negation == 0 & (problem_good == 1 | problem_low == 1) & problem_high == 0 & problem_bad == 0
replace problem_group = 3 if problem == 1 & problem_negation == 0 & problem_good == 0 & problem_low == 0 & problem_bad == 0 & problem_high == 0
replace problem_group = 4 if problem == 1 & problem_negation == 0 & (problem_bad == 1 | problem_high == 1) & problem_low == 0 & problem_good == 0
replace problem_group = 5 if problem == 1 & problem_negation == 0 & problem_group == 0

gen rust_group = 0
replace rust_group = 1 if rust == 1 & rust_negation == 1 
replace rust_group = 2 if rust == 1 & rust_negation == 0 & (rust_good == 1 | rust_low == 1) & rust_high == 0 & rust_bad == 0
replace rust_group = 3 if rust == 1 & rust_negation == 0 & rust_good == 0 & rust_low == 0 & rust_bad == 0 & rust_high == 0
replace rust_group = 4 if rust == 1 & rust_negation == 0 & (rust_bad == 1 | rust_high == 1) & rust_low == 0 & rust_good == 0
replace rust_group = 5 if rust == 1 & rust_negation == 0 & rust_group == 0

/* Labels */

lab var text "Text Size (in bytes)"
lab var age "Age"
lab var miles "Miles"
lab var logmiles "Log Miles"
lab var startbid "Starting Bid"
lab var numbid "Number of Bids"
la var store "Store"
la var pwrseller "Powerseller"
la var sellfdback "Seller Feedback (total)"
la var pctfdback "Seller Feedback (percentage)"
 
lab var age2 "Age Squared"
lab var trans "Manual Transmission"
lab var buyitnow "Buy-it-now"
lab var inspection "Inspection"
lab var reserve "Reserve"
lab var logtext "Log of Text Size (in bytes)"

lab var phone "Phone Number Provided"
lab var addedinfo "Added Info during the Auction"
lab var questions "Questions Posted on Webpage?"

lab var totallisted "Total Number of Listings"
lab var totalsold "Total Sold during sample period"
lab var photos "Number of Photos"

lab var address "Address Provided"
lab var featured "Featured Listing"
lab var logfdback "Log Feedback"

replace sellerage = sellerage/365
lab var sellerage "Duration of eBay membership (years)"

/* Cleaning and Selection */

drop if trans == 1 | trans == 5
drop if miles < 0
drop if miles == 0 & condition != 22
drop if model == 1 | model == 37
drop if doors == 1 | doors == 5
drop if doors == 3 & model != 30 
drop if model == 27 & doors == 2
drop if model == 28 & doors == 4
drop if model == 32 & doors == 4
drop if model == 33 & doors == 4
drop if model == 38 & doors == 2 
drop if model == 39 & doors == 4 
drop if title != 17
keep if condition == 23

gen negpct = 100 - pctfdback

lab var negpct "\% Negative Feedback"

drop if age == .
drop if year < 1950
drop if software == ""

egen carmodel = group(model doors trans)
drop if carmodel == 14 | carmodel == 50

xi i.carmodel i.year i.week, prefix(cont)
xi i.carmodel i.year i.n i.week, prefix(con2)
gen ageXphoto = age*photos
gen warrantyXphoto = warranty*photos
gen warrantyXage = warranty*age
gen warrantyXageXphotos = warranty*age*photos

gen group = 0
replace group = 1 if model == 25 | model == 26 | model == 27 | model == 29 | model == 30 | model == 31| model == 38
replace group = 2 if model == 33| model == 34 | model == 35 | model == 36 | model == 40| model == 41 | model == 42

encode sellername, generate(sellerid) 
xtset sellerid
save temp, replace

/* Table 0: Summary Statistics */
/* Listings are 42.3% dealer, 57.7% private sellers */

gen posbid = 0
replace posbid = 1 if n != 0
gen minbid = startbid/bookvalue

gen pro = 0
replace pro = 1 if software == "carad" 
replace pro = 2 if software == "auction123"
replace pro = 3 if software == "eBizAutos"

xi i.pro i.group
summ miles age trans warranty options photos relist sellfdback negpct minbid posbid sell biddy1 _Igroup* _Ipro* rust rust_negation scratch scratch_negation dent dent_negation 
bysort dealer: summ miles age trans warranty options photos relist sellfdback negpct minbid posbid sell biddy1 _Igroup* _Ipro* rust rust_negation scratch scratch_negation dent dent_negation

/* Table 1: Information on Photos, cut many ways --- must explain that most of these get at least 1 bid */

gen photos2 = photos^2/100
label var photos2 "Photos squared / 100"

gen collectible = 0
replace collectible = 1 if group == 0 & year < 1980

reg logbid1 logmiles photos photos2 options logfdback negpct cont*, cluster(sellername)
estimates store m1, title((1))
reg logbid1 logmiles photos photos2 options ageXphoto warranty warrantyXphoto logfdback negpct cont*,  cluster(sellername)
estimates store m2, title((2))
reg logbid1 logmiles photos photos2 options logfdback negpct cont* if dealer == 0, cluster(sellername)
estimates store m3, title((3))
reg logbid1 logmiles photos photos2 options logfdback negpct cont* if dealer == 1, cluster(sellername)
estimates store m4, title((4))

reg logbid1 logmiles photos photos2 options logfdback negpct cont* if collectible == 1, cluster(sellername)
estimates store m16, title((16))
reg logbid1 logmiles photos photos2 logbook options logfdback negpct cont*, cluster(sellername)
estimates store m5, title((5))

/* Table 2: Endogeneity, many versions */

reg logbid1 logmiles photos photos2 options logfdback negpct con2*, cluster(sellername)
estimates store m6, title((6))
xtreg logbid1 logmiles photos photos2 options logfdback negpct cont* if dealer == 1, fe r 
estimates store m7, title((7))

gen depvar2 = logbid1
replace depvar2 = logstart if n == 0

intreg logbid1 depvar2 logmiles photos photos2 options logfdback negpct cont*, cluster(sellername)
estimates store m8, title((8))

/* Table 3: Text Coefficients */
xi: reg logbid1 logmiles photos photos2 options logfdback negpct i.scratch_group i.dent_group i.rust_group  cont*, cluster(sellername)

estimates store m9, title((9))

xi: reg logbid1 logmiles photos photos2 options logfdback negpct i.scratch_group i.dent_group i.rust_group  cont* if dealer == 0, cluster(sellername)

estimates store m10, title((10))

xi: reg logbid1 logmiles photos photos2 options logfdback negpct i.scratch_group i.dent_group i.rust_group  cont* if dealer == 1, cluster(sellername)

estimates store m11, title((11))

xi: reg logbid1 logmiles photos photos2 logbook options logfdback negpct i.scratch_group i.dent_group i.rust_group cont*, cluster(sellername)
estimates store m12, title((12))

/* Table 4: Costs and IV */
char software[omit] "ebayhosting"

xi i.software, prefix(_sof)

xtreg photos _sof* logmiles options logfdback negpct cont* if dealer == 1 & n > 0, fe r
estimates store m13
test _sofsoftwar_1 _sofsoftwar_2 _sofsoftwar_3
xtivreg logbid1 (photos = _sof*) logmiles options logfdback negpct cont* if dealer == 1, fe
estimates store m14

xtreg logbid1 logmiles photos options logfdback negpct cont* if dealer == 1, fe r 
estimates store m15

/* Output */

esttab m1 m2 m3 m4 m16 m5 using output1.doc, cells(b(fmt(%9.3f)) se(par fmt(%9.3f))) stats(r2 N, fmt(%9.4f %9.0g) labels(R-squared))  style(fixed) varlabels(_cons Constant) title("Reduced Form Hedonic Regression") nolegend label replace 
esttab m6 m8 m7 using output2.doc, cells(b(fmt(%9.3f)) se(par fmt(%9.3f))) stats(r2 N, fmt(%9.4f %9.0g) labels(R-squared)) style(fixed) varlabels(_cons Constant) title("Dealers versus Non-Dealers") nolegend label replace
esttab m9 m10 m11 m12 using output3.doc, cells(b(fmt(%9.3f)) se(par fmt(%9.3f))) stats(r2 N, fmt(%9.4f %9.0g) labels(R-squared)) style(fixed) varlabels(_cons Constant) title("Text Analysis") nolegend label replace
esttab m13 m14 m15 using output4.doc, cells(b(fmt(%9.3f)) se(par fmt(%9.3f)))  stats(r2 N, fmt(%9.4f %9.0g) labels(R-squared)) style(fixed) varlabels(_cons Constant) title("Costs and IV") nolegend label replace

/* Supplementary Material: Test whether observable car characteristics are the same pre-and-post upgrade */

gen double startingdate2 = clock(startdate, "MDYhms#", 2040)
sort sellername startingdate2
gen switch = 0
by sellername: replace switch = 1 if webpage[_n-1] != webpage[_n] & sellername[_n] == sellername[_n-1]
by sellername: egen totalswitch = sum(switch)



/* Cut dataset to just people who switch at some point */


keep if totalswitch > 0
xi i.group


bysort sellername: egen meanmiles = mean(miles)
bysort sellername: egen meanage = mean(age)

bysort sellername: egen meantrans = mean(trans)
bysort sellername: egen meanreliable = mean(_Igroup_1)

bysort sellername: egen meantruck = mean(_Igroup_2)


gen resmiles = miles - meanmiles

gen resage = age - meanage

gen restrans = trans - meantrans

gen resreliable = _Igroup_1 - meanreliable

gen restruck = _Igroup_2 - meantruck


gen postswitch = 0


drop temp

by sellername: gen temp = _n if switch == 1

by sellername: egen temp2 = min(temp)

by sellername: replace postswitch = 1 if _n >= temp2

bysort sellername postswitch: gen numobs = _N


save temp2, replace


bysort postswitch: summ resmiles resage restrans resreliable restruck

ttest resmiles, by(postswitch)

ttest resage, by(postswitch)

ttest restrans, by(postswitch)

ttest resreliable, by(postswitch)

ttest restruck, by(postswitch)

hotelling resmiles resage restrans resreliable restruck, by(postswitch)

collapse miles age trans _Igroup_1 _Igroup_2, by(sellername postswitch)

bysort postswitch: summ miles age trans _Igroup_1 _Igroup_2



use temp2, clear

gen upgrade = 0 

bysort sellername: replace upgrade = 1 if webpage[_n-1] ==15 & webpage[_n] != 15 & sellername[_n] == sellername[_n-1]

by sellername: egen totalupgrade = sum(upgrade)


keep if totalupgrade > 0

gen postupgrade = 0

drop numobs

bysort sellername postupgrade: gen numobs = _N


drop temp temp2


by sellername: gen temp = _n if upgrade == 1

by sellername: egen temp2 = min(temp)

by sellername: replace postupgrade = 1 if _n >= temp2


bysort postupgrade: summ resmiles resage restrans resreliable restruck

ttest resmiles, by(postupgrade)

ttest resage, by(postupgrade)

ttest restrans, by(postupgrade)

ttest resreliable, by(postupgrade)

ttest restruck, by(postupgrade)

hotelling resmiles resage restrans resreliable restruck, by(postupgrade)


collapse miles age trans _Igroup_1 _Igroup_2, by(sellername postupgrade)

bysort postupgrade: summ miles age trans _Igroup_1 _Igroup_2



use temp2, clear


gen downgrade = 0 

bysort sellername: replace downgrade = 1 if webpage[_n-1] !=15 & webpage[_n] == 15 & sellername[_n] == sellername[_n-1]

by sellername: egen totaldowngrade = sum(downgrade)


keep if totaldowngrade > 0

gen postdowngrade = 0

drop numobs

bysort sellername postdowngrade: gen numobs = _N


drop temp temp2

by sellername: gen temp = _n if downgrade == 1

by sellername: egen temp2 = min(temp)

by sellername: replace postdowngrade = 1 if _n >= temp2


bysort postdowngrade: summ resmiles resage restrans resreliable restruck

ttest resmiles, by(postdowngrade)

ttest resage, by(postdowngrade)

ttest restrans, by(postdowngrade)

ttest resreliable, by(postdowngrade)

ttest restruck, by(postdowngrade)

hotelling resmiles resage restrans resreliable restruck, by(postdowngrade)


collapse miles age trans _Igroup_1 _Igroup_2, by(sellername postdowngrade)

bysort postdowngrade: summ miles age trans _Igroup_1 _Igroup_2


/* File Cleanup */

//erase "temp.dta"
//erase "temp2.dta"