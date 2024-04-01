/*In this exercise, we use the Principal component anlaysis (PCA) to evaluate the Impact of 
development assistant on economic development though some indicators and a new index


Part 1: 

I. Principal component anlysis 

7. Run a principal component analysis on sample N=275 of the 
three variables of the policy index. 


 Initial sample size: N=275
 
 
 Policy index: the policy index (variable "policy") is the weighted average  of 
 the openess measure (variable "sacw"), the inflation rate (variable "infl") 
 and the budget surplus (variable "bb"), where the weights are  diven by the
 corresponding coefficients in the regression reported in table 4 colum 1. 
 The index is measured in terms of percentage points of GDP growth.
 
"In this work we considered the possiblity that the policy index should 
be treated as endogenous. (...) for simplicity, we treat poliy as exogenous"�.

"our model suggests that it is the distorsions that affect growth that will
 determine the effectiveness of aid. Therefore, we thought it was natural
 that our policy index should weight the policies according to their impact
 on growth, a feature that is absent from the principal component analysis.
 (...) The key feature of our policy index is that it weights the policy 
 variables according to their correlation with growth. (...) we use an 
 OLS regression of the growth equation with no aid term to fix the values 
 of the coefficients that determine the policy index"

 
 1st step: PCA of the three variables of the policy index.
 
 Principal component analysis is a statistical procedure that uses an 
orthogonal transformation to convert a set of observations of possibly 
correlated variables (entities each of which takes on various numerical
 values) into a set of value of linearly uncorrelated variables called
 principal components. If there are n observations (here, n=275) with p
 variables (here, p=3), then the number of distinct principal components 
 is min(n-1, p)=min (274,3)=3.
 The resulting vectors are each one a linear combination of the variables
 and contain n observations and an uncorrelated orthogonal basis set.
Mathematically, the transoformation is defined by a set of p dimensional 
vectors of weights or coefficient wk=(w1,...wp) that map each row vector
 x of X to a new vector of principal component scores t=(t1,...tl) given 
 by tk=x*w
 PCA can be used to reduce the number of variables  by describing a series
 of uncorrelated linear combinations of the variables that contain most of the variance
 or to learn about the underlying structure of the data. All principal components 
 combined contain the same information as the original variables, but the important
 information is partitioned over the components in a particular way: the components
 are orthohonal and earlier components contain more information than later compoenents.
 
 In stata, pca shows 2 panels. The first panel lists the eigenvalues of the 
 correlation matrix, from largest to smallest. The corresponding egeinvectors are 
 listed in the second panel. The eigenvalues are the variances of the principal components.


 */
 
cls 
clear all 
set more off 
set scrollbufsize 500000 
set maxvar 10000
graph drop _all 
capture log close 



* 2 - Data Step
* --------------
* 2a - Discover the dataset
* -------------------------

use "C:\Users\DIALLO K MOHAMED\Documents\jbd_project\burnside275.dta" 

d
count
  
 pca sacw infl bb
 estimates store PCA 
 /* Here, we can see that the first component has variance 1.32 explaining
 1.32/3 =44 % of the total variance. The second principal component has variance
 0.99 or 33% of the total variance and the third has variance 0.7 ie 23 %.
 Principal components are uncorrelated
 In the second panel, we can see that these 3 components contain all information in the data
 (and therefore all the variances in the variables are explained*/ 
  
 
 /*
 Second step: Compute three indices corresponding to each of the 
 three principal axis, as a linear combination with PCA 
weights on each of the three standardized variables.*/

****** Obtaining each component as a weighted sum of standardized variables */

egen std_sacw1 = std(sacw)
egen std_infl = std(infl)
egen std_bb = std(bb)
gen index1=0.3831*std_sacw-0.6013*std_infl+0.7012*std_bb
gen index2=0.8520*std_sacw+0.5233*std_infl-0.0167*std_bb
gen index3=-0.3569*std_sacw+0.6038*std_infl+0.7128*std_bb
 
*Lower
* it is the same that the following 1-line code: 

predict pc1 pc2 pc3, score
 estimates store Predicted 
/* we can check that the components are orthogonal (ie corr=0):*/
corr pc1 pc2
corr pc2 pc3
corr pc1 pc3

screeplot, mean 
screeplot, yline(1) ci(het)
loadingplot, comp(3) combined 
 
 * From these corelation matrix and the the plots, we can see that all variable are clustered together and are relatively far from the origin.
 *its mean that there is no correlation between them*
 

/*8. Run an OLS regression for N=275 where you include the three axis of PCA 
instead of each of the variables (inflation, budget surplus, openness).
 Comment.*/
 
 
 * regression (as in Burnside and Dollar) (based on table 6)
 
 *A. in regressions without interaction terms
 
 *- all countries
 
 reg lgdp index1 index2 index3 eda, vce (robust)
 estimates store PCAAll
 
 margins, dydx(index1 index2 index3 eda)

 * to be compared with:
 reg lgdp infl sacw bb eda
 estimates store OLSAll1
 
 margins, dydx(infl sacw bb eda)

 * and with:
 reg lgdp policy eda
 estimates store OLSAll2
 
 margins, dydx(policy eda)

 esttab PCAAll OLSAll1 OLSAll2  using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE A1: OLS Regressions without interaction terms for All") replace
 
 
 *-> we can see that the R2 in the PCA case is equal to the R2
 * in the case where we use the 3 variables: that is because we have seen
 * that the 3 components fully explain the total variance
 *(while the R2 with the policy index is weaker)
 
*-> here, without interaction term, only the variable eda is singificant (and negative)
* and the index 2 at a 5% level, whereas by using the 3 variables, all variables are 
*significant except the budget surplus
* In the regression with the policy index, this variable is not signficiant

 
  *gen eda_gdp= eda/gdp (a priori it is not necesary because the variable eda in the 
 * database is already eda/gdp


 *- lower income countries
 
reg lgdp index1 index2 index3 eda /// 
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")
 
 estimates store PCAIC
 margins, dydx(index1 index2 index3 eda)
 
 reg lgdp infl sacw bb eda ///
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")
 
 estimates store OLSIC
 margins, dydx(infl sacw bb eda)
 
 *WITH 
 reg lgdp policy eda ///
 if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")
 
 
 estimates store OLSIC2
 margins, dydx(policy eda)
  
 esttab PCAIC OLSIC OLSIC2 using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE A2: OLS Regressions without interaction terms For Income Countries") replace

  
  *------interactions term, quadratic term and aid------*
  *All countries*
  
 reg lgdp policy eda edapolicy eda2policy 
 estimates store OLSITAll
 
 margins, dydx(policy eda edapolicy eda2policy)
 
 reg lgdp policy eda edapolicy eda2policy ///
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")
 
estimates store OLSIT


esttab OLSITAll OLSIT using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE A3: OLS Regressions With Interaction and Quadratic terms for All and Income Countries") replace


 *TO DO ?
 
 *COMMENT:
 *B.----- in regressions with simple and quadratic interaction terms (ie with edapolicy 
 * an with eda2policy)
 
  *- all countries
  
 gen eda_index1=eda*index1
 gen eda_index2=eda*index2
 gen eda_index3=eda*index3
 gen eda2_index1=eda^2*index1
 gen eda2_index2=eda^2*index2
 gen eda2_index3=eda^2*index3
  
 reg lgdp index1 index2 index3 eda eda_index1 eda_index2 eda_index3 eda2_index1 eda2_index2 eda2_index3, vce(robust) 
estimates store PCAITQAll

esttab PCAITQAll using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) ///
 se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE B1: OLS Regressions With PCA for All Countries") replace


 *margins, dydx(index1 index2 index3 eda)

 * to be compared with:
 gen eda_infl=eda*infl
 gen eda_sacw=eda*sacw
 gen eda_bb=eda*bb
 gen eda2_infl=eda^2*infl
 gen eda2_sacw=eda^2*sacw
 gen eda2_bb=eda^2*bb
 
 reg lgdp infl sacw bb eda  eda_infl eda_sacw eda_bb eda2_infl eda2_sacw eda2_bb
estimates store OLSIQAll
 
 
  *margins, dydx(infl sacw bb eda)

 * and with:
 reg lgdp policy eda edapolicy eda2policy
 estimates store OLSIQ
 
 margins, dydx(policy eda edapolicy eda2policy)
 
 esttab OLSIQAll OLSIQ using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) ///
 se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE B1: OLS Regressions With Simple Interaction and Quadratic terms for All Countries") replace

 
 
 *COMMENT:
 
 *R2 with PCA is almost the same than R2 with the 3 variables (42%) and is higher 
 *than R2 with policy index
 * Again, index1 is not signficant. The interaction terms are almost never significant
 * especially the quadratic ones (with PCA as with the 3 variables)
 *The eda coefficient is still significant and negative

 
 *- lower income countries
 * 259 observations
 
 reg lgdp index1 index2 index3 eda eda_index1 eda_index2 eda_index3 eda2_index1 eda2_index2 eda2_index3  ///
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")

estimates store PCAITQIC
 
 esttab PCAITQIC using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) ///
 se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE B1: OLS Regressions With Simple PCA for Income Countries") replace

 
reg lgdp infl sacw bb eda  eda_infl eda_sacw eda_bb eda2_infl eda2_sacw eda2_bb ///
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")

estimates store OLSIQIC
  
 
 *To commpared
 
 
reg lgdp policy eda edapolicy eda2policy /// 
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")

estimates store OLSITIC2
 
margins, dydx(policy eda edapolicy eda2policy)

esttab OLSIQIC OLSITIC2 using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) ///
 se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE B2: OLS Regressions With Simple Interaction and Quadratic terms for Income Countries") replace



*compared the both
 
 
 
 
 *C. In regressions with smple interaction terms
 
  *- all countries
  
  reg lgdp index1 index2 index3 eda eda_index1 eda_index2 eda_index3
* margins, dydx(index1 index2 index3 eda)
estimates store PCASITAl1C
 esttab PCASITAl1C  using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE C1: OLS Regressions With PCA for ALL Countries") replace

 

 * to be compared with:
 reg lgdp infl sacw bb eda eda_infl eda_sacw eda_bb
 *margins, dydx(infl sacw bb eda)
estimates store PCASSITAll2
 * and with:
 reg lgdp policy eda edapolicy
 *margins, dydx(policy eda)
 estimates store OLSSITAll
 
  esttab PCASSITAll2 OLSSITAll using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE C1: OLS Regressions With PCA and simple Interaction terms for ALL Countries") replace

 
 
 *COMMENT:
 *With simple interaction term, only the variables index1 and eda_index1 (interaction term)
 *are not significant, and respectively yhr variable sacw and eda_infl.
 *The interaction terms eda_index1 and eda_index2 eda_sacw are positive, while the interaction term
 *eda_index3 eda_nfl eda_bb are negative.
 *With the policy index, we find a positive coefficient of interaction btw aid and policy 
 *as in Burnside and Dollar, but not significant.
 
 *- lower income countries
 reg lgdp index1 index2 index3 eda eda_index1 eda_index2 eda_index3 /// 
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")
 
 *margins, dydx(policy eda)
estimates store PCASITICC

 esttab PCASITICC using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE C2: OLS Regressions With PCA for Income Countries") replace


* to be compared with:
 reg lgdp infl sacw bb eda eda_infl eda_sacw eda_bb /// 
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")

 *margins, dydx(infl sacw bb eda)
 estimates store OLSSIT2IC
 
 
 * and with:
 reg lgdp policy eda edapolicy /// 
if (countryyear != "GAB2") ///
& (countryyear !="ARG2") /// 
& (countryyear != "BRA2")  /// 
& (countryyear !="CHL2") ///
& (countryyear != "COL2") /// 
& (countryyear !="CRI2") ///
& (countryyear !="GTM2")  ///
& (countryyear !="JAM3") ///
& (countryyear != "MEX2") ///
& (countryyear !="PER2")  ///
& (countryyear != "TTO2")  /// 
& (countryyear !="URY2")  ///
& (countryyear != "VEN2")  /// 
& (countryyear != "SYR2")  ///
& (countryyear != "TUR7")  ///
& (countryyear !="MYS2")

 *margins, dydx(policy eda)
 estimates store OLSSIT2IC2
 
 esttab OLSSIT2IC OLSSIT2IC2 using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE C2: OLS Regressions With PCA and Simple Interaction terms for Income Countries") replace


 
 
 *we can see that in all cases, index1 (or pc1) is not statistically significant at 5%
 
*COMMENTER LES RESULTATS ET L INTERET DE CETTE METHODE
/*Burnside and Dollar: "we consider a number of alternative methods. 
 The first method we considered was a simple principal components approach
 that is, using the first principal component in our analysis rather than
 all three policy variables. Unfortunately, in our sample the first two
 principal components are almost perfectly correlated with openness and 
 inflation, respectively. Thus, the principal components approach did not
 lead us to a natural single index measure of policy. Instead, it effectively
 suggested that we drop the budget surplus variable and include both openness
 and inflation in our regression."*/
 
 
 corr pc1 infl
 corr pc2 infl
 corr pc1 sacw
 corr pc2 sacw
 corr pc1 bb
 corr pc2 bb
 
 *-> strong but not full correlation
 **************************************************************************************
 /* Part 2: GMM
 
*************************************************************************************
 
 1. Arrellano and Bond GMM (panel data regression with policy, aid and policy*aid_GDP
 with only lag(1) as instruments
 
 
 Arellano-Bond estimator:  generalized method of moments esitmator used to estimate
 dynamic panel data models. (dynamic= which include lagged levels of the dependent
 vriable as regresor => no strict exogeneity because the lagged dependent variable
 is necessarily correlated with the idiosyncratic error.). In the Arellano-Bond method,
 first difference of the regression equation are taken to eliminate the fixed effects.
 Then, deeper lags of the dependent variable are used as instruments for differenced
 lags of the dependent variable (which are endogenous). In traditional panel data
 techniques, Adding deeper lags of the dependent variable reduces the number of
 observations available. For example, if observations are available at T time 
 periods, then, after first differencing, only T-1 lags are usuable. If K lags
 of the dependent variable are used as instruents, only T-K-1 obs are usable
 in the regression → trade off btw more lags ie more IV but smaller sample size 
→ the arellano-bond method circumvents this problem = general estimators designed 
for situations with small T, large N panels with independent variables that are not
 strictly exogenous meaning they are correlated with past , fixed effect, 
 heteroskedasticity and autocorr.
 */
 
 *-Step 1: declaring that we use panel data
 *first, converting the string variable country in numeric variable
encode country, gen (country2)
 xtset country2 year
 *- Step2: Arellano-Bond estimator
xtabond lgdp eda policy edapolicy, lags(1)
 xtabond lgdp eda policy edapolicy, lags(1)twostep robust
* to have unbiased standard error, option robust.
estimates store BOND1
  
 *2. Number of observations remaining using this GMM estimation
 
 * Results (table): 162 observations remaining (indeed, here, observations available
 *at T=6, then, after differencing only 5 lags usuable. 1 lag is used => 4 obs for
 *each country. As we have 56 countries, it would remains 4*56=224.
 *But the panel is umbalanced (for some countries, there are less than 6 period
 *That is why we have 162 observations.
 
 *3. Sargan test and interpretation
 *The distribution of the Sargan test is know only when the errors are iid.
 *(ie for a homoskedastic error term)
 *-> we need to work with the biased standard errors because the robust variance
 *prevent from doing a sargan test
  xtabond lgdp eda policy edapolicy, lags(1)twostep
 estat sargan
 
 *result: prob>chi2 = 0.0133 < 5% : we reject H0 at a 5% level of significancy
 *In other words, the overidentifying conditions are not valid -> we need to reconsider
 *our model or our instrument, unless we attribute the rejection to heteroskedasticity

  *In 1 line code: we can have the sargan test at the same time than the estimation 
 *with the xtabond2 command: 
 
 ssc install xtabond2
xtabond2 lgdp eda policy edapolicy, gmm(eda policy edapolicy, lag(1 1))
estimates store BONB2
 
 * Correlation matrix of first differences and lagged levels (checking the 
 * strenght of instruments IN THE SAMPLE USED IN EQUATION 13
 esttab BOND1 BONB2 using "C:\Users\DIALLO K MOHAMED\Documents\dossier_JBC.rtf", b(%10.3f) se r2 scalars(N N_clust) star(* 0.10 ** 0.05 *** 0.01) ///
label nodepvars nomtitles title(" TABLE 1: Arrellano and Bond GMM System") replace

 
 *THERE IS NO EQUATION 13
 
 *we can also test for the autocorrelation : 
 estat abond
 *We can see that there is first order autocorrelation (H0 rejected= but not 
 *second-order autocorrelaton in the first differenced errors.
 *However, as there is not serial correlation in the first-differenced errors 
 *at an order higher than 1, the moment conditions used by xtabond remains valid.
 
 
 
 
 *global comment: 
 *GMM useful because variables assumed to be endogenous (causality may run in
 *both directions => regressors may be correlated with the error term)+
*+ time invariant country characteristics (fixed effects) such as geography and 
*demographics may be correlated with the explanatory variables 
*+ the presence of the lagged dependent variable gives rise to autocorrelation
*+ the panel dataset has a short time dimension (T=6) and a larger country dimension (N=36)
*(in large T panels, a shock to the country's fixed effect which shows in the error
*term will decline with time and the correlation of the lagged dependent variable
*wth the error term will be insignificant (Roodman, 2006) -> Arellano-Bond estimaor useless in this case)

 *GMM maybe better than 2SLS when the IV is weak. Indeed, with weak 
 *instruments, the fixed effects IV estimators are likely to be biased.
 
  
/*11 Run again the GMM estimation using a smaller number of instruments 
than in 13, such that there is at least one instrument more than variable
 to be instrumented, and keeping only the lagged levels used as instrument
 which seems to have the largest simple correlation with some of the
 first  differences. Comment. */
 
 *- determine the lagged levels used as IV which seems to have the largest
 * simple correlation with some of the first differences
 
 *1) genererating lagged variables /*
  
so country year
bysort country: gen eda_ret=eda[_n-1]
bysort country: gen policy_ret=policy[_n-1]
bysort country: gen edapolicy_ret=edapolicy[_n-1]

*2)lag 2:
so country year
bysort country: gen eda_ret2=eda[_n-2]
bysort country: gen policy_ret2=policy[_n-2]
bysort country: gen edapolicy_ret2=edapolicy[_n-2]

*lag 3
so country year
bysort country: gen eda_ret3=eda[_n-3]
bysort country: gen policy_ret3=policy[_n-3]
bysort country: gen edapolicy_ret3=edapolicy[_n-3]

* for lag>2, a lot of NA generated -> we stop at lag=2
 
 *3) generate first differences

bysort country: gen eda_diff=eda[_n]-eda[_n-1]
bysort country: gen edapolicy_diff=edapolicy[_n]-edapolicy[_n-1]
bysort country: gen policy_diff=policy[_n]-policy[_n-1]
 
foreach var in eda policy edapolicy{
display "`var'"
corr `var'_diff `var'
corr `var'_ret `var'
corr `var'_ret `var'_diff
corr `var'_ret2 `var'_diff
corr `var'_ret3 `var'_diff
}

foreach var in eda policy edapolicy{
display "`var'"
corr `var'_ret `var'_diff
corr `var'_ret2 `var'_diff
corr `var'_ret3 `var'_diff
}

*-> lag 2 seems the more correlated with first difference
 
 *What is 13??? 
 

