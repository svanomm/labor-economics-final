# Abstract
Additional education is expected to increase wages. This is supported by a wide literature in labor economics. However, measuring the education effect on wages is complicated by various sources of bias. Two common sources of contamination are ability bias (self-selection into education) and self-selection into employment. A common method to control for ability bias is with Instrumental Variables (IV) regression, while a common method to control for employment selection is Heckman correction. Using NLSY97 longitudinal survey data, I employ a conditional mixed process (CMP) method to jointly apply IV and Heckman corrections to a model of female wages. This appears to be the first application of CMP to combining these corrections. I find that IV alone increases the measured education effect (versus OLS), while Heckman correction decreases it. Controlling for both sources of bias, the education effect is slightly larger than OLS, and is consistent with existing literature. The CMP method shows promise for simultaneously correcting biases common to the wage equation.
# Introduction
It is generally known that, all else equal, people with more education tend to make more money. Education is the primary determinant of human capital, the set of abilities and acquired skills workers offer to their employers. A common method to estimate the effect of increased education on wages is to simply compare the average hourly wages of people at various levels of education, and calculate the average wage gap between them. For example, we might compare wages of people with only a high school degree to people who completed one year of college. The assumption would be that the difference in average wages is the effect of pursuing 1 additional year of education.

However, all people have an innate ability/work ethic, and this ability level differs from person to person. Some people end up being more productive than others by chance, and that productivity translates to increased wages regardless of their education. The issue is that people with more innate ability are more likely to pursue additional education, creating an \textbf{ability bias}. One reason for this is that people with more ability are likely to get more value out of education: a greater capacity to understand their classes will transfer to more skills gained in school. People self-select into more education based on their ability level, and both ability level and education determine their wages.

Empirical studies often control for ability bias with \textbf{Instrumental Variables (IV) regression} (also called two-stage least squares or 2SLS). IV regression involves identifying variables that are correlated with the endogenous variable (education) but uncorrelated with the residuals of the wage equation; these variables are called instruments. The IV procedure first regresses educational attainment on the instruments, then uses the predictions from the first step as a variable in the wage regression.\footnote{I have described the two-stage version of the procedure, but other techniques can estimate both equations simultaneously.} This process reduces or eliminates the endogeneity of education in the wage equation, i.e. the ability bias.

We can only estimate a wage equation for people who have a wage, i.e. who choose to work. But selection into employment is not random. In other words, people who choose to work are likely systematically different from people who choose not to work. Assuming they are the same subjects our estimates to sample selection bias. This selection is often studied for women, who historically are more likely to leave the job market to take care of their children. Women with less earnings potential, ability, and/or education are more likely to exit the job market, all else equal.

Studies often control for selection into employment with a Heckman correction procedure. The idea is to first model the decision to work based on observed data, and then incorporate a bias correction term into the wage equation based on the probability of employment.\footnote{I have described the two-stage version of the procedure, but maximum likelihood techniques can estimate both equations simultaneously.} This additional term reduces or eliminates the endogeneity of the decision to work, i.e. the employment selection.

While both methods are commonly employed to reduce bias in the wage equation, they are rarely combined in a joint specification. Conceptually, applying both bias correction techniques seems attractive. After all, employment selection bias does not disappear when controlling for ability bias, and vice versa. Practically speaking, however, combining these approaches is more complicated and computationally difficult. Fortunately, as discussed below, conditional mixed process (CMP) analysis allows for a straightforward application of such corrections.
# Literature Review
## Education Effect for Women
## Conditional Mixed Process (CMP)

# Data
For my analysis, I use the Bureau of Labor Statistics National Longitudinal Survey of Youth 1997 (NLSY97).\footnote{\url{https://nlsinfo.org/investigator/pages/home}.} The BLS describes the data as:\footnote{\url{https://www.bls.gov/nls/nlsy97.htm}.}
\begin{quote}
    a nationally representative sample of 8,984 men and women born during the years 1980 through 1984 and living in the United States at the time of the initial survey in 1997.  Participants were ages 12 to 16 as of December 31, 1996.  Interviews were conducted annually from 1997 to 2011 and biennially since then.  The ongoing cohort has been surveyed 21 times as of date.  Data are available from Round 1 (1997–98) through Round 20 (2021–22). 
\end{quote}
I collected the following variables from the data:
\begin{itemize}
    \item \textbf{Demographics}: Sex, race, birth year, annual Census geographic region, a variety of questions on drug use in 1997/98, marital status, number of children in the household
    \item \textbf{Labor Statistics}: Hourly wage, hours worked per year, years of education
    \item \textbf{Parent Demographics}: Father's years of education, mother's years of education, parental divorce status in 2002
    \item \textbf{School Quality}: A variety of questions about school in 1997
\end{itemize}
\noindent
Variables that could change over time (such as wage and years of education) were collected for 2010, 2011, 2013, 2015, 2017, 2019, and 2021, giving up to 7 years of panel data on wage and education per person (subject to panel attrition). The data were also limited to females only.

The drug use questions included:
\begin{itemize}
    \item During the past 30 days, on how many days did you smoke a cigarette?
    \item In the past 30 days, on the days you drank alcohol, about how many drinks did you usually have?
    \item On how many days have you used marijuana in the last 30 days?
    \item Have you ever used any drugs like cocaine or crack or heroin, or any other substance not prescribed by a doctor, in order to get high or to achieve an altered state?\footnote{This question was asked in 1998, instead of 1997.}
\end{itemize}

I use the drug questions to create a ``Drug Intensity'' score, where a higher value indicates a more extensive use of drugs during childhood.\footnote{The score is created by adding the number of days the respondent used each of the listed drugs (in the past 30 days), with equal weight. Since the cocaine question is a Yes/No, I assign a value of 15 to a Yes. The lowest possible score is 0 and highest possible score is 105.} I hypothesize that more drug use in childhood has a negative correlation with educational attainment, but not with wages in 2021 (after controlling for the other variables).
\\\\
The school questions included:
\begin{itemize}
    \item I had something of value stolen from me at school. (Yes/No)
    \item Someone threatened to hurt me at school. (Yes/No)
    \item I got into a physical fight at school. (Yes/No)
    \item I was late for school without an excuse. (Yes/No)
    \item How many days were you absent from school during the Fall term?
    \item The teachers are good. Do you strongly agree, agree, disagree, or strongly disagree?
    \item The teachers are interested in the students.
    \item Disruptions by other students [get/gets] in the way of my learning.
    \item Students are graded fairly.
    \item There is a lot of cheating on tests and assignments.
    \item Discipline is fair.
    \item I [feel/felt] safe at this school.
\end{itemize}
I use the school questions to create a ``School Quality'' score, where a higher value indicates that the student is more likely to enjoy school.\footnote{The score is created by adding 1 for answering Yes to a positive-sentiment question and subtracting 1 for answering Yes to a negative-sentiment question. The lowest possible score is -7 and highest possible score is 5.} I hypothesize that a higher score in childhood has a positive correlation with educational attainment, but not with wages in 2021 (after controlling for the other variables).
\\\\
A summary of my collected variables is below.
\input{summary_statistics_table}

# Methodology
Below, I describe the econometric specifications tested with the panel data. All specifications use standard errors clustered by survey participant.
## OLS
Two main OLS models were tested. First, a simple Mincer equation regresses log of wages on years of education, work experience, and work experience squared. Since we have multiple years of data per person, these variables change from year to year.\footnote{While I analyze the 2010-2021 period, where subjects were at least 26 years old, approximately one out of four subjects obtained additional education during the analysis period.} Next, we add control variables for Black, Hispanic, number of children, and fixed effects by Census region. The latter two variables are allowed to change over time.
## Instrumental Variables (IV)
The instruments used in my analysis are: Father's Education, 
Mother's Education, Parents Divorced, Drug Intensity Score, and School Quality Score. 

The use of parents' education as an instrument is well known in the literature; it is fairly obvious that parents have a significant influence on their child's educational attainment, and that parents with more education will generally encourage their children to get more education as well. It is less obvious that parental education has no effect on their child's eventual wage, but on average it should not affect wages much outside of the education effect.\footnote{One could imagine that some well-educated parents can help their children find better jobs, though this seems unlikely to occur often. Additionally, well-educated parents might pass on better genes to their children.}

My additional instruments are motivated by the idea that personal hardship and attitude to education during childhood should influence educational attainment while not correlating with residuals from the wage equation. I expected that children whose parents were divorced may associate school with that negative experience, thus pushing them away from education. I hypothesized that children who were abusing drugs have preferences for riskier and more immediate benefits, rejecting education. Finally, children who liked going to school should pursue more school.

One of the difficult problems with IV models is that there is no way to empirically test the exclusion restriction, i.e. that the instrument is uncorrelated with the wage equation residuals. However, the child's drug use and attitude toward school are far removed in time from their eventual wages.

I test two IV models. The first IV model only uses parents' education as instruments, while the second IV model includes my additional instruments as well. The wage equation is the same as the OLS model with controls specified above, though including the IV correction term.
## Heckman correction
While the OLS and IV models above are restricted to wage earners only, the following models use all available data.

The selection equation regresses a flag for working status on Black, Hispanic, number of children, age, and marital status. The number of children and marital status variables are expected to affect women's work status: women with more children are less likely to work, and some married women are also less likely to work.

 The wage equation is the same as the OLS model with controls specified above, though including the selection correction term. The model is estimated using Stata's `heckman` command, which uses a Maximum Likelihood Estimation approach rather than a two-stage approach (though the results are practically the same either way).
## Heckman + IV with CMP
CMP analysis is used to combine the endogenous education correction from IV with the selection probability correction from the Heckit model. This is made straightforward with the Stata package `cmp`, created by Roodman.

The model estimates a system of three recursive equations: the wage equation as specified above, the education model from the IV first stage, and the selection equation on working status from the Heckit model. The only other input to the cmp function is an indication of the type of each equation. One additional note is that in the CMP model, the education equation is estimated on \textit{all} participants, not just wage earners.

**below is not accurate description of wooldridge method**
An alternative approach to CMP could 1) run the selection model to create the predicted IMR, 2) run the education model to create the predicted education, and 3) include the predicted IMR and predicted education in the wage equation.\footnote{See Wooldridge Procedure 17.2} This yields a similar estimated effect to the CMP results. However, the standard errors in step 3 are incorrect. CMP estimates the entire system in one step, provides consistent clustered standard errors, and allows for the residuals between step 1 and 2 to be correlated. CMP is also easily extended to include additional bias correction techniques.}
## Sensitivities
Finally, we report some sensitivity models to ensure the robustness of our results. For the OLS model, we include fixed effects by participant as a sensitivity. For the IV model, we use Stata's `xtivreg` command to incorporate Random Effects (RE) into both of the main models. For the Heckman model, we also include RE by participant.\footnote{This is accomplished through the `cmp` command rather than Stata's `xtheckman`, which took far longer to compute.} And for the CMP model, we also include RE by participant.
# Results
## Instrumental Variables (IV)
## Heckman correction
## Heckman + IV with CMP
## Sensitivities
Sensitivity regression results are reported in the Appendix below. While the patterns described above are consistent in the sensitivities as well, the incorporation of fixed/random effects reduces the measure education effect across the board. [This result is consistent with existing literature.]\footnote{} All measured effects continue to be positive and statistically significant.
# Discussion
# Conclusion 

# Appendix: Sensitivities