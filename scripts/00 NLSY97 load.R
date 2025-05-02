library(here)

new_data <- read.table(here('./data/nlsy97-raw.dat'), sep=' ')
names(new_data) <- c('R0000100',
'R0068900',
'R0069000',
'R0069100',
'R0069200',
'R0069300',
'R0069400',
'R0069500',
'R0069600',
'R0069700',
'R0069800',
'R0069900',
'R0070000',
'R0358100',
'R0358500',
'R0359100',
'R0536300',
'R0536401',
'R0536402',
'R1200300',
'R1235800',
'R1302400',
'R1302500',
'R1482600',
'R2191500',
'S1249300',
'T5202300',
'T5206700',
'T5208500',
'T5208600',
'T5208700',
'T5208800',
'T5208900',
'T5209000',
'T5209100',
'T5209200',
'T5209300',
'T5211400',
'T5211700',
'T6652100',
'T6656500',
'T6658700',
'T6658800',
'T6658900',
'T6659000',
'T6659100',
'T6659200',
'T6659300',
'T6659400',
'T6659500',
'T6659600',
'T6659700',
'T6659800',
'T6659900',
'T6662800',
'T6663100',
'T8123700',
'T8128900',
'T8130800',
'T8130900',
'T8131000',
'T8131100',
'T8131200',
'T8131300',
'T8131400',
'T8131500',
'T8131600',
'T8131700',
'T8134000',
'T8134300',
'U0001900',
'U0008800',
'U0010700',
'U0010800',
'U0010900',
'U0011000',
'U0011100',
'U0011200',
'U0011300',
'U0011400',
'U0011500',
'U0011600',
'U0011700',
'U0011800',
'U0014500',
'U0014800',
'U0014901',
'U1838600',
'U1845400',
'U1847600',
'U1847700',
'U1847800',
'U1847900',
'U1848000',
'U1848100',
'U1848200',
'U1848300',
'U1848400',
'U1848500',
'U1848600',
'U1848700',
'U1848800',
'U1848900',
'U1849000',
'U1852300',
'U1852600',
'U1852800',
'U3438000',
'U3443900',
'U3445700',
'U3445800',
'U3445900',
'U3446000',
'U3446100',
'U3446200',
'U3446300',
'U3446400',
'U3446500',
'U3446600',
'U3446700',
'U3451400',
'U3451900',
'U4943100',
'U4949600',
'U4951300',
'U4951400',
'U4951500',
'U4951600',
'U4951700',
'U4951800',
'U4951900',
'U4952000',
'U4952100',
'U4952200',
'U4954500',
'U4955000',
'Z9065401',
'Z9068800',
'Z9068900',
'Z9069000',
'Z9069100',
'Z9069200',
'Z9069300',
'Z9069400',
'Z9069500',
'Z9069600',
'Z9069700',
'Z9069701',
'Z9069702',
'Z9069703',
'Z9069704',
'Z9069705',
'Z9069706',
'Z9069707',
'Z9069708',
'Z9069709',
'Z9069710',
'Z9069711',
'Z9069712',
'Z9069713',
'Z9069800',
'Z9069900',
'Z9070000',
'Z9070100',
'Z9070200',
'Z9070300',
'Z9070400',
'Z9070500',
'Z9070600',
'Z9070700',
'Z9070800',
'Z9070900',
'Z9071000',
'Z9071100',
'Z9071200',
'Z9071300',
'Z9071400',
'Z9071500',
'Z9071600',
'Z9071700',
'Z9085100')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0069400 <- factor(data$R0069400,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0069500 <- factor(data$R0069500,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0069600 <- factor(data$R0069600,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0069700 <- factor(data$R0069700,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0069800 <- factor(data$R0069800,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0069900 <- factor(data$R0069900,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0070000 <- factor(data$R0070000,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Strongly agree",
"Agree",
"Disagree",
"Strongly Disagree"))
  data$R0536300 <- factor(data$R0536300,
levels=c(0.0,1.0,2.0),
labels=c("No Information",
"Male",
"Female"))
  data$R0536401 <- factor(data$R0536401,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
labels=c("1: January",
"2: February",
"3: March",
"4: April",
"5: May",
"6: June",
"7: July",
"8: August",
"9: September",
"10: October",
"11: November",
"12: December"))
  data$R1200300 <- factor(data$R1200300,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$R1235800 <- factor(data$R1235800,
levels=c(0.0,1.0),
labels=c("Oversample",
"Cross-sectional"))
  data$R1302400 <- factor(data$R1302400,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302500 <- factor(data$R1302500,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1482600 <- factor(data$R1482600,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Black",
"Hispanic",
"Mixed Race (Non-Hispanic)",
"Non-Black / Non-Hispanic"))
  data$R2191500 <- factor(data$R2191500,
levels=c(0.0,1.0),
labels=c("No",
"Yes"))
  data$S1249300 <- factor(data$S1249300,
levels=c(0.0,1.0),
labels=c("NO",
"YES"))
  data$T5202300 <- factor(data$T5202300,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$T5206700 <- factor(data$T5206700,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$T5211400 <- factor(data$T5211400,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$T6652100 <- factor(data$T6652100,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$T6656500 <- factor(data$T6656500,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$T6662800 <- factor(data$T6662800,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$T8123700 <- factor(data$T8123700,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$T8128900 <- factor(data$T8128900,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$T8134000 <- factor(data$T8134000,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U0001900 <- factor(data$U0001900,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$U0008800 <- factor(data$U0008800,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$U0014500 <- factor(data$U0014500,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U1838600 <- factor(data$U1838600,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$U1845400 <- factor(data$U1845400,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$U1852300 <- factor(data$U1852300,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U3438000 <- factor(data$U3438000,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$U3443900 <- factor(data$U3443900,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$U3451400 <- factor(data$U3451400,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U4943100 <- factor(data$U4943100,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)",
"North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)",
"South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)",
"West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)"))
  data$U4949600 <- factor(data$U4949600,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$U4954500 <- factor(data$U4954500,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$Z9085100 <- factor(data$Z9085100,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0),
labels=c("Round 1",
"Round 2",
"Round 3",
"Round 4",
"Round 5",
"Round 6",
"Round 7",
"Round 8",
"Round 9",
"Round 10",
"Round 11",
"Round 12",
"Round 13",
"Round 14",
"Round 15",
"Round 16",
"Round 17",
"Round 18",
"Round 19",
"Round 20"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
data$R0000100 <- factor(data$R0000100,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999"))
data$R0068900[1.0 <= data$R0068900 & data$R0068900 <= 4.0] <- 1.0
data$R0068900[5.0 <= data$R0068900 & data$R0068900 <= 9.0] <- 5.0
data$R0068900[10.0 <= data$R0068900 & data$R0068900 <= 14.0] <- 10.0
data$R0068900[15.0 <= data$R0068900 & data$R0068900 <= 19.0] <- 15.0
data$R0068900[20.0 <= data$R0068900 & data$R0068900 <= 24.0] <- 20.0
data$R0068900[25.0 <= data$R0068900 & data$R0068900 <= 29.0] <- 25.0
data$R0068900[30.0 <= data$R0068900 & data$R0068900 <= 34.0] <- 30.0
data$R0068900[35.0 <= data$R0068900 & data$R0068900 <= 39.0] <- 35.0
data$R0068900[40.0 <= data$R0068900 & data$R0068900 <= 44.0] <- 40.0
data$R0068900[45.0 <= data$R0068900 & data$R0068900 <= 49.0] <- 45.0
data$R0068900[50.0 <= data$R0068900 & data$R0068900 <= 9.9999999E7] <- 50.0
data$R0068900 <- factor(data$R0068900,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30 TO 34",
"35 TO 39",
"40 TO 44",
"45 TO 49",
"50 TO 99999999: 50+"))
data$R0069000[1.0 <= data$R0069000 & data$R0069000 <= 4.0] <- 1.0
data$R0069000[5.0 <= data$R0069000 & data$R0069000 <= 9.0] <- 5.0
data$R0069000[10.0 <= data$R0069000 & data$R0069000 <= 14.0] <- 10.0
data$R0069000[15.0 <= data$R0069000 & data$R0069000 <= 19.0] <- 15.0
data$R0069000[20.0 <= data$R0069000 & data$R0069000 <= 24.0] <- 20.0
data$R0069000[25.0 <= data$R0069000 & data$R0069000 <= 29.0] <- 25.0
data$R0069000[30.0 <= data$R0069000 & data$R0069000 <= 34.0] <- 30.0
data$R0069000[35.0 <= data$R0069000 & data$R0069000 <= 39.0] <- 35.0
data$R0069000[40.0 <= data$R0069000 & data$R0069000 <= 44.0] <- 40.0
data$R0069000[45.0 <= data$R0069000 & data$R0069000 <= 49.0] <- 45.0
data$R0069000[50.0 <= data$R0069000 & data$R0069000 <= 9.9999999E7] <- 50.0
data$R0069000 <- factor(data$R0069000,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30 TO 34",
"35 TO 39",
"40 TO 44",
"45 TO 49",
"50 TO 99999999: 50+"))
data$R0069100[1.0 <= data$R0069100 & data$R0069100 <= 4.0] <- 1.0
data$R0069100[5.0 <= data$R0069100 & data$R0069100 <= 9.0] <- 5.0
data$R0069100[10.0 <= data$R0069100 & data$R0069100 <= 14.0] <- 10.0
data$R0069100[15.0 <= data$R0069100 & data$R0069100 <= 19.0] <- 15.0
data$R0069100[20.0 <= data$R0069100 & data$R0069100 <= 24.0] <- 20.0
data$R0069100[25.0 <= data$R0069100 & data$R0069100 <= 29.0] <- 25.0
data$R0069100[30.0 <= data$R0069100 & data$R0069100 <= 34.0] <- 30.0
data$R0069100[35.0 <= data$R0069100 & data$R0069100 <= 39.0] <- 35.0
data$R0069100[40.0 <= data$R0069100 & data$R0069100 <= 44.0] <- 40.0
data$R0069100[45.0 <= data$R0069100 & data$R0069100 <= 49.0] <- 45.0
data$R0069100[50.0 <= data$R0069100 & data$R0069100 <= 9.9999999E7] <- 50.0
data$R0069100 <- factor(data$R0069100,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30 TO 34",
"35 TO 39",
"40 TO 44",
"45 TO 49",
"50 TO 99999999: 50+"))
data$R0069200[1.0 <= data$R0069200 & data$R0069200 <= 4.0] <- 1.0
data$R0069200[5.0 <= data$R0069200 & data$R0069200 <= 9.0] <- 5.0
data$R0069200[10.0 <= data$R0069200 & data$R0069200 <= 14.0] <- 10.0
data$R0069200[15.0 <= data$R0069200 & data$R0069200 <= 19.0] <- 15.0
data$R0069200[20.0 <= data$R0069200 & data$R0069200 <= 24.0] <- 20.0
data$R0069200[25.0 <= data$R0069200 & data$R0069200 <= 29.0] <- 25.0
data$R0069200[30.0 <= data$R0069200 & data$R0069200 <= 34.0] <- 30.0
data$R0069200[35.0 <= data$R0069200 & data$R0069200 <= 39.0] <- 35.0
data$R0069200[40.0 <= data$R0069200 & data$R0069200 <= 44.0] <- 40.0
data$R0069200[45.0 <= data$R0069200 & data$R0069200 <= 49.0] <- 45.0
data$R0069200[50.0 <= data$R0069200 & data$R0069200 <= 9.9999999E7] <- 50.0
data$R0069200 <- factor(data$R0069200,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30 TO 34",
"35 TO 39",
"40 TO 44",
"45 TO 49",
"50 TO 99999999: 50+"))
data$R0069300[1.0 <= data$R0069300 & data$R0069300 <= 4.0] <- 1.0
data$R0069300[5.0 <= data$R0069300 & data$R0069300 <= 9.0] <- 5.0
data$R0069300[10.0 <= data$R0069300 & data$R0069300 <= 14.0] <- 10.0
data$R0069300[15.0 <= data$R0069300 & data$R0069300 <= 19.0] <- 15.0
data$R0069300[20.0 <= data$R0069300 & data$R0069300 <= 24.0] <- 20.0
data$R0069300[25.0 <= data$R0069300 & data$R0069300 <= 29.0] <- 25.0
data$R0069300[30.0 <= data$R0069300 & data$R0069300 <= 34.0] <- 30.0
data$R0069300[35.0 <= data$R0069300 & data$R0069300 <= 39.0] <- 35.0
data$R0069300[40.0 <= data$R0069300 & data$R0069300 <= 44.0] <- 40.0
data$R0069300[45.0 <= data$R0069300 & data$R0069300 <= 49.0] <- 45.0
data$R0069300[50.0 <= data$R0069300 & data$R0069300 <= 9.9999999E7] <- 50.0
data$R0069300 <- factor(data$R0069300,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30 TO 34",
"35 TO 39",
"40 TO 44",
"45 TO 49",
"50 TO 99999999: 50+"))
data$R0358100[1.0 <= data$R0358100 & data$R0358100 <= 4.0] <- 1.0
data$R0358100[5.0 <= data$R0358100 & data$R0358100 <= 9.0] <- 5.0
data$R0358100[10.0 <= data$R0358100 & data$R0358100 <= 14.0] <- 10.0
data$R0358100[15.0 <= data$R0358100 & data$R0358100 <= 19.0] <- 15.0
data$R0358100[20.0 <= data$R0358100 & data$R0358100 <= 24.0] <- 20.0
data$R0358100[25.0 <= data$R0358100 & data$R0358100 <= 29.0] <- 25.0
data$R0358100 <- factor(data$R0358100,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30"))
data$R0358500[1.0 <= data$R0358500 & data$R0358500 <= 4.0] <- 1.0
data$R0358500[5.0 <= data$R0358500 & data$R0358500 <= 9.0] <- 5.0
data$R0358500[10.0 <= data$R0358500 & data$R0358500 <= 14.0] <- 10.0
data$R0358500[15.0 <= data$R0358500 & data$R0358500 <= 19.0] <- 15.0
data$R0358500[20.0 <= data$R0358500 & data$R0358500 <= 24.0] <- 20.0
data$R0358500[25.0 <= data$R0358500 & data$R0358500 <= 29.0] <- 25.0
data$R0358500 <- factor(data$R0358500,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30"))
data$R0359100[1.0 <= data$R0359100 & data$R0359100 <= 4.0] <- 1.0
data$R0359100[5.0 <= data$R0359100 & data$R0359100 <= 9.0] <- 5.0
data$R0359100[10.0 <= data$R0359100 & data$R0359100 <= 14.0] <- 10.0
data$R0359100[15.0 <= data$R0359100 & data$R0359100 <= 19.0] <- 15.0
data$R0359100[20.0 <= data$R0359100 & data$R0359100 <= 24.0] <- 20.0
data$R0359100[25.0 <= data$R0359100 & data$R0359100 <= 29.0] <- 25.0
data$R0359100 <- factor(data$R0359100,
levels=c(0.0,1.0,5.0,10.0,15.0,20.0,25.0,30.0),
labels=c("0",
"1 TO 4",
"5 TO 9",
"10 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30"))
data$T5208500[1.0 <= data$T5208500 & data$T5208500 <= 599.0] <- 1.0
data$T5208500[600.0 <= data$T5208500 & data$T5208500 <= 999.0] <- 600.0
data$T5208500[1000.0 <= data$T5208500 & data$T5208500 <= 2099.0] <- 1000.0
data$T5208500[2100.0 <= data$T5208500 & data$T5208500 <= 4099.0] <- 2100.0
data$T5208500[4100.0 <= data$T5208500 & data$T5208500 <= 6099.0] <- 4100.0
data$T5208500[6100.0 <= data$T5208500 & data$T5208500 <= 9999.0] <- 6100.0
data$T5208500[10000.0 <= data$T5208500 & data$T5208500 <= 9999999.0] <- 10000.0
data$T5208500 <- factor(data$T5208500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5208600[1.0 <= data$T5208600 & data$T5208600 <= 599.0] <- 1.0
data$T5208600[600.0 <= data$T5208600 & data$T5208600 <= 999.0] <- 600.0
data$T5208600[1000.0 <= data$T5208600 & data$T5208600 <= 2099.0] <- 1000.0
data$T5208600[2100.0 <= data$T5208600 & data$T5208600 <= 4099.0] <- 2100.0
data$T5208600[4100.0 <= data$T5208600 & data$T5208600 <= 6099.0] <- 4100.0
data$T5208600[6100.0 <= data$T5208600 & data$T5208600 <= 9999.0] <- 6100.0
data$T5208600[10000.0 <= data$T5208600 & data$T5208600 <= 9999999.0] <- 10000.0
data$T5208600 <- factor(data$T5208600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5208700[1.0 <= data$T5208700 & data$T5208700 <= 599.0] <- 1.0
data$T5208700[600.0 <= data$T5208700 & data$T5208700 <= 999.0] <- 600.0
data$T5208700[1000.0 <= data$T5208700 & data$T5208700 <= 2099.0] <- 1000.0
data$T5208700[2100.0 <= data$T5208700 & data$T5208700 <= 4099.0] <- 2100.0
data$T5208700[4100.0 <= data$T5208700 & data$T5208700 <= 6099.0] <- 4100.0
data$T5208700[6100.0 <= data$T5208700 & data$T5208700 <= 9999.0] <- 6100.0
data$T5208700[10000.0 <= data$T5208700 & data$T5208700 <= 9999999.0] <- 10000.0
data$T5208700 <- factor(data$T5208700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5208800[1.0 <= data$T5208800 & data$T5208800 <= 599.0] <- 1.0
data$T5208800[600.0 <= data$T5208800 & data$T5208800 <= 999.0] <- 600.0
data$T5208800[1000.0 <= data$T5208800 & data$T5208800 <= 2099.0] <- 1000.0
data$T5208800[2100.0 <= data$T5208800 & data$T5208800 <= 4099.0] <- 2100.0
data$T5208800[4100.0 <= data$T5208800 & data$T5208800 <= 6099.0] <- 4100.0
data$T5208800[6100.0 <= data$T5208800 & data$T5208800 <= 9999.0] <- 6100.0
data$T5208800[10000.0 <= data$T5208800 & data$T5208800 <= 9999999.0] <- 10000.0
data$T5208800 <- factor(data$T5208800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5208900[1.0 <= data$T5208900 & data$T5208900 <= 599.0] <- 1.0
data$T5208900[600.0 <= data$T5208900 & data$T5208900 <= 999.0] <- 600.0
data$T5208900[1000.0 <= data$T5208900 & data$T5208900 <= 2099.0] <- 1000.0
data$T5208900[2100.0 <= data$T5208900 & data$T5208900 <= 4099.0] <- 2100.0
data$T5208900[4100.0 <= data$T5208900 & data$T5208900 <= 6099.0] <- 4100.0
data$T5208900[6100.0 <= data$T5208900 & data$T5208900 <= 9999.0] <- 6100.0
data$T5208900[10000.0 <= data$T5208900 & data$T5208900 <= 9999999.0] <- 10000.0
data$T5208900 <- factor(data$T5208900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5209000[1.0 <= data$T5209000 & data$T5209000 <= 599.0] <- 1.0
data$T5209000[600.0 <= data$T5209000 & data$T5209000 <= 999.0] <- 600.0
data$T5209000[1000.0 <= data$T5209000 & data$T5209000 <= 2099.0] <- 1000.0
data$T5209000[2100.0 <= data$T5209000 & data$T5209000 <= 4099.0] <- 2100.0
data$T5209000[4100.0 <= data$T5209000 & data$T5209000 <= 6099.0] <- 4100.0
data$T5209000[6100.0 <= data$T5209000 & data$T5209000 <= 9999.0] <- 6100.0
data$T5209000[10000.0 <= data$T5209000 & data$T5209000 <= 9999999.0] <- 10000.0
data$T5209000 <- factor(data$T5209000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5209100[1.0 <= data$T5209100 & data$T5209100 <= 599.0] <- 1.0
data$T5209100[600.0 <= data$T5209100 & data$T5209100 <= 999.0] <- 600.0
data$T5209100[1000.0 <= data$T5209100 & data$T5209100 <= 2099.0] <- 1000.0
data$T5209100[2100.0 <= data$T5209100 & data$T5209100 <= 4099.0] <- 2100.0
data$T5209100[4100.0 <= data$T5209100 & data$T5209100 <= 6099.0] <- 4100.0
data$T5209100[6100.0 <= data$T5209100 & data$T5209100 <= 9999.0] <- 6100.0
data$T5209100[10000.0 <= data$T5209100 & data$T5209100 <= 9999999.0] <- 10000.0
data$T5209100 <- factor(data$T5209100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5209200[1.0 <= data$T5209200 & data$T5209200 <= 599.0] <- 1.0
data$T5209200[600.0 <= data$T5209200 & data$T5209200 <= 999.0] <- 600.0
data$T5209200[1000.0 <= data$T5209200 & data$T5209200 <= 2099.0] <- 1000.0
data$T5209200[2100.0 <= data$T5209200 & data$T5209200 <= 4099.0] <- 2100.0
data$T5209200[4100.0 <= data$T5209200 & data$T5209200 <= 6099.0] <- 4100.0
data$T5209200[6100.0 <= data$T5209200 & data$T5209200 <= 9999.0] <- 6100.0
data$T5209200[10000.0 <= data$T5209200 & data$T5209200 <= 9999999.0] <- 10000.0
data$T5209200 <- factor(data$T5209200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5209300[1.0 <= data$T5209300 & data$T5209300 <= 599.0] <- 1.0
data$T5209300[600.0 <= data$T5209300 & data$T5209300 <= 999.0] <- 600.0
data$T5209300[1000.0 <= data$T5209300 & data$T5209300 <= 2099.0] <- 1000.0
data$T5209300[2100.0 <= data$T5209300 & data$T5209300 <= 4099.0] <- 2100.0
data$T5209300[4100.0 <= data$T5209300 & data$T5209300 <= 6099.0] <- 4100.0
data$T5209300[6100.0 <= data$T5209300 & data$T5209300 <= 9999.0] <- 6100.0
data$T5209300[10000.0 <= data$T5209300 & data$T5209300 <= 9999999.0] <- 10000.0
data$T5209300 <- factor(data$T5209300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T5211700[10.0 <= data$T5211700 & data$T5211700 <= 999.0] <- 10.0
data$T5211700 <- factor(data$T5211700,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$T6658700[1.0 <= data$T6658700 & data$T6658700 <= 599.0] <- 1.0
data$T6658700[600.0 <= data$T6658700 & data$T6658700 <= 999.0] <- 600.0
data$T6658700[1000.0 <= data$T6658700 & data$T6658700 <= 2099.0] <- 1000.0
data$T6658700[2100.0 <= data$T6658700 & data$T6658700 <= 4099.0] <- 2100.0
data$T6658700[4100.0 <= data$T6658700 & data$T6658700 <= 6099.0] <- 4100.0
data$T6658700[6100.0 <= data$T6658700 & data$T6658700 <= 9999.0] <- 6100.0
data$T6658700[10000.0 <= data$T6658700 & data$T6658700 <= 9999999.0] <- 10000.0
data$T6658700 <- factor(data$T6658700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6658800[1.0 <= data$T6658800 & data$T6658800 <= 599.0] <- 1.0
data$T6658800[600.0 <= data$T6658800 & data$T6658800 <= 999.0] <- 600.0
data$T6658800[1000.0 <= data$T6658800 & data$T6658800 <= 2099.0] <- 1000.0
data$T6658800[2100.0 <= data$T6658800 & data$T6658800 <= 4099.0] <- 2100.0
data$T6658800[4100.0 <= data$T6658800 & data$T6658800 <= 6099.0] <- 4100.0
data$T6658800[6100.0 <= data$T6658800 & data$T6658800 <= 9999.0] <- 6100.0
data$T6658800[10000.0 <= data$T6658800 & data$T6658800 <= 9999999.0] <- 10000.0
data$T6658800 <- factor(data$T6658800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6658900[1.0 <= data$T6658900 & data$T6658900 <= 599.0] <- 1.0
data$T6658900[600.0 <= data$T6658900 & data$T6658900 <= 999.0] <- 600.0
data$T6658900[1000.0 <= data$T6658900 & data$T6658900 <= 2099.0] <- 1000.0
data$T6658900[2100.0 <= data$T6658900 & data$T6658900 <= 4099.0] <- 2100.0
data$T6658900[4100.0 <= data$T6658900 & data$T6658900 <= 6099.0] <- 4100.0
data$T6658900[6100.0 <= data$T6658900 & data$T6658900 <= 9999.0] <- 6100.0
data$T6658900[10000.0 <= data$T6658900 & data$T6658900 <= 9999999.0] <- 10000.0
data$T6658900 <- factor(data$T6658900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659000[1.0 <= data$T6659000 & data$T6659000 <= 599.0] <- 1.0
data$T6659000[600.0 <= data$T6659000 & data$T6659000 <= 999.0] <- 600.0
data$T6659000[1000.0 <= data$T6659000 & data$T6659000 <= 2099.0] <- 1000.0
data$T6659000[2100.0 <= data$T6659000 & data$T6659000 <= 4099.0] <- 2100.0
data$T6659000[4100.0 <= data$T6659000 & data$T6659000 <= 6099.0] <- 4100.0
data$T6659000[6100.0 <= data$T6659000 & data$T6659000 <= 9999.0] <- 6100.0
data$T6659000[10000.0 <= data$T6659000 & data$T6659000 <= 9999999.0] <- 10000.0
data$T6659000 <- factor(data$T6659000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659100[1.0 <= data$T6659100 & data$T6659100 <= 599.0] <- 1.0
data$T6659100[600.0 <= data$T6659100 & data$T6659100 <= 999.0] <- 600.0
data$T6659100[1000.0 <= data$T6659100 & data$T6659100 <= 2099.0] <- 1000.0
data$T6659100[2100.0 <= data$T6659100 & data$T6659100 <= 4099.0] <- 2100.0
data$T6659100[4100.0 <= data$T6659100 & data$T6659100 <= 6099.0] <- 4100.0
data$T6659100[6100.0 <= data$T6659100 & data$T6659100 <= 9999.0] <- 6100.0
data$T6659100[10000.0 <= data$T6659100 & data$T6659100 <= 9999999.0] <- 10000.0
data$T6659100 <- factor(data$T6659100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659200[1.0 <= data$T6659200 & data$T6659200 <= 599.0] <- 1.0
data$T6659200[600.0 <= data$T6659200 & data$T6659200 <= 999.0] <- 600.0
data$T6659200[1000.0 <= data$T6659200 & data$T6659200 <= 2099.0] <- 1000.0
data$T6659200[2100.0 <= data$T6659200 & data$T6659200 <= 4099.0] <- 2100.0
data$T6659200[4100.0 <= data$T6659200 & data$T6659200 <= 6099.0] <- 4100.0
data$T6659200[6100.0 <= data$T6659200 & data$T6659200 <= 9999.0] <- 6100.0
data$T6659200[10000.0 <= data$T6659200 & data$T6659200 <= 9999999.0] <- 10000.0
data$T6659200 <- factor(data$T6659200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659300[1.0 <= data$T6659300 & data$T6659300 <= 599.0] <- 1.0
data$T6659300[600.0 <= data$T6659300 & data$T6659300 <= 999.0] <- 600.0
data$T6659300[1000.0 <= data$T6659300 & data$T6659300 <= 2099.0] <- 1000.0
data$T6659300[2100.0 <= data$T6659300 & data$T6659300 <= 4099.0] <- 2100.0
data$T6659300[4100.0 <= data$T6659300 & data$T6659300 <= 6099.0] <- 4100.0
data$T6659300[6100.0 <= data$T6659300 & data$T6659300 <= 9999.0] <- 6100.0
data$T6659300[10000.0 <= data$T6659300 & data$T6659300 <= 9999999.0] <- 10000.0
data$T6659300 <- factor(data$T6659300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659400[1.0 <= data$T6659400 & data$T6659400 <= 599.0] <- 1.0
data$T6659400[600.0 <= data$T6659400 & data$T6659400 <= 999.0] <- 600.0
data$T6659400[1000.0 <= data$T6659400 & data$T6659400 <= 2099.0] <- 1000.0
data$T6659400[2100.0 <= data$T6659400 & data$T6659400 <= 4099.0] <- 2100.0
data$T6659400[4100.0 <= data$T6659400 & data$T6659400 <= 6099.0] <- 4100.0
data$T6659400[6100.0 <= data$T6659400 & data$T6659400 <= 9999.0] <- 6100.0
data$T6659400[10000.0 <= data$T6659400 & data$T6659400 <= 9999999.0] <- 10000.0
data$T6659400 <- factor(data$T6659400,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659500[1.0 <= data$T6659500 & data$T6659500 <= 599.0] <- 1.0
data$T6659500[600.0 <= data$T6659500 & data$T6659500 <= 999.0] <- 600.0
data$T6659500[1000.0 <= data$T6659500 & data$T6659500 <= 2099.0] <- 1000.0
data$T6659500[2100.0 <= data$T6659500 & data$T6659500 <= 4099.0] <- 2100.0
data$T6659500[4100.0 <= data$T6659500 & data$T6659500 <= 6099.0] <- 4100.0
data$T6659500[6100.0 <= data$T6659500 & data$T6659500 <= 9999.0] <- 6100.0
data$T6659500[10000.0 <= data$T6659500 & data$T6659500 <= 9999999.0] <- 10000.0
data$T6659500 <- factor(data$T6659500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659600[1.0 <= data$T6659600 & data$T6659600 <= 599.0] <- 1.0
data$T6659600[600.0 <= data$T6659600 & data$T6659600 <= 999.0] <- 600.0
data$T6659600[1000.0 <= data$T6659600 & data$T6659600 <= 2099.0] <- 1000.0
data$T6659600[2100.0 <= data$T6659600 & data$T6659600 <= 4099.0] <- 2100.0
data$T6659600[4100.0 <= data$T6659600 & data$T6659600 <= 6099.0] <- 4100.0
data$T6659600[6100.0 <= data$T6659600 & data$T6659600 <= 9999.0] <- 6100.0
data$T6659600[10000.0 <= data$T6659600 & data$T6659600 <= 9999999.0] <- 10000.0
data$T6659600 <- factor(data$T6659600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659700[1.0 <= data$T6659700 & data$T6659700 <= 599.0] <- 1.0
data$T6659700[600.0 <= data$T6659700 & data$T6659700 <= 999.0] <- 600.0
data$T6659700[1000.0 <= data$T6659700 & data$T6659700 <= 2099.0] <- 1000.0
data$T6659700[2100.0 <= data$T6659700 & data$T6659700 <= 4099.0] <- 2100.0
data$T6659700[4100.0 <= data$T6659700 & data$T6659700 <= 6099.0] <- 4100.0
data$T6659700[6100.0 <= data$T6659700 & data$T6659700 <= 9999.0] <- 6100.0
data$T6659700[10000.0 <= data$T6659700 & data$T6659700 <= 9999999.0] <- 10000.0
data$T6659700 <- factor(data$T6659700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659800[1.0 <= data$T6659800 & data$T6659800 <= 599.0] <- 1.0
data$T6659800[600.0 <= data$T6659800 & data$T6659800 <= 999.0] <- 600.0
data$T6659800[1000.0 <= data$T6659800 & data$T6659800 <= 2099.0] <- 1000.0
data$T6659800[2100.0 <= data$T6659800 & data$T6659800 <= 4099.0] <- 2100.0
data$T6659800[4100.0 <= data$T6659800 & data$T6659800 <= 6099.0] <- 4100.0
data$T6659800[6100.0 <= data$T6659800 & data$T6659800 <= 9999.0] <- 6100.0
data$T6659800[10000.0 <= data$T6659800 & data$T6659800 <= 9999999.0] <- 10000.0
data$T6659800 <- factor(data$T6659800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6659900[1.0 <= data$T6659900 & data$T6659900 <= 599.0] <- 1.0
data$T6659900[600.0 <= data$T6659900 & data$T6659900 <= 999.0] <- 600.0
data$T6659900[1000.0 <= data$T6659900 & data$T6659900 <= 2099.0] <- 1000.0
data$T6659900[2100.0 <= data$T6659900 & data$T6659900 <= 4099.0] <- 2100.0
data$T6659900[4100.0 <= data$T6659900 & data$T6659900 <= 6099.0] <- 4100.0
data$T6659900[6100.0 <= data$T6659900 & data$T6659900 <= 9999.0] <- 6100.0
data$T6659900[10000.0 <= data$T6659900 & data$T6659900 <= 9999999.0] <- 10000.0
data$T6659900 <- factor(data$T6659900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T6663100[10.0 <= data$T6663100 & data$T6663100 <= 999.0] <- 10.0
data$T6663100 <- factor(data$T6663100,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$T8130800[1.0 <= data$T8130800 & data$T8130800 <= 599.0] <- 1.0
data$T8130800[600.0 <= data$T8130800 & data$T8130800 <= 999.0] <- 600.0
data$T8130800[1000.0 <= data$T8130800 & data$T8130800 <= 2099.0] <- 1000.0
data$T8130800[2100.0 <= data$T8130800 & data$T8130800 <= 4099.0] <- 2100.0
data$T8130800[4100.0 <= data$T8130800 & data$T8130800 <= 6099.0] <- 4100.0
data$T8130800[6100.0 <= data$T8130800 & data$T8130800 <= 9999.0] <- 6100.0
data$T8130800[10000.0 <= data$T8130800 & data$T8130800 <= 9999999.0] <- 10000.0
data$T8130800 <- factor(data$T8130800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8130900[1.0 <= data$T8130900 & data$T8130900 <= 599.0] <- 1.0
data$T8130900[600.0 <= data$T8130900 & data$T8130900 <= 999.0] <- 600.0
data$T8130900[1000.0 <= data$T8130900 & data$T8130900 <= 2099.0] <- 1000.0
data$T8130900[2100.0 <= data$T8130900 & data$T8130900 <= 4099.0] <- 2100.0
data$T8130900[4100.0 <= data$T8130900 & data$T8130900 <= 6099.0] <- 4100.0
data$T8130900[6100.0 <= data$T8130900 & data$T8130900 <= 9999.0] <- 6100.0
data$T8130900[10000.0 <= data$T8130900 & data$T8130900 <= 9999999.0] <- 10000.0
data$T8130900 <- factor(data$T8130900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131000[1.0 <= data$T8131000 & data$T8131000 <= 599.0] <- 1.0
data$T8131000[600.0 <= data$T8131000 & data$T8131000 <= 999.0] <- 600.0
data$T8131000[1000.0 <= data$T8131000 & data$T8131000 <= 2099.0] <- 1000.0
data$T8131000[2100.0 <= data$T8131000 & data$T8131000 <= 4099.0] <- 2100.0
data$T8131000[4100.0 <= data$T8131000 & data$T8131000 <= 6099.0] <- 4100.0
data$T8131000[6100.0 <= data$T8131000 & data$T8131000 <= 9999.0] <- 6100.0
data$T8131000[10000.0 <= data$T8131000 & data$T8131000 <= 9999999.0] <- 10000.0
data$T8131000 <- factor(data$T8131000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131100[1.0 <= data$T8131100 & data$T8131100 <= 599.0] <- 1.0
data$T8131100[600.0 <= data$T8131100 & data$T8131100 <= 999.0] <- 600.0
data$T8131100[1000.0 <= data$T8131100 & data$T8131100 <= 2099.0] <- 1000.0
data$T8131100[2100.0 <= data$T8131100 & data$T8131100 <= 4099.0] <- 2100.0
data$T8131100[4100.0 <= data$T8131100 & data$T8131100 <= 6099.0] <- 4100.0
data$T8131100[6100.0 <= data$T8131100 & data$T8131100 <= 9999.0] <- 6100.0
data$T8131100[10000.0 <= data$T8131100 & data$T8131100 <= 9999999.0] <- 10000.0
data$T8131100 <- factor(data$T8131100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131200[1.0 <= data$T8131200 & data$T8131200 <= 599.0] <- 1.0
data$T8131200[600.0 <= data$T8131200 & data$T8131200 <= 999.0] <- 600.0
data$T8131200[1000.0 <= data$T8131200 & data$T8131200 <= 2099.0] <- 1000.0
data$T8131200[2100.0 <= data$T8131200 & data$T8131200 <= 4099.0] <- 2100.0
data$T8131200[4100.0 <= data$T8131200 & data$T8131200 <= 6099.0] <- 4100.0
data$T8131200[6100.0 <= data$T8131200 & data$T8131200 <= 9999.0] <- 6100.0
data$T8131200[10000.0 <= data$T8131200 & data$T8131200 <= 9999999.0] <- 10000.0
data$T8131200 <- factor(data$T8131200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131300[1.0 <= data$T8131300 & data$T8131300 <= 599.0] <- 1.0
data$T8131300[600.0 <= data$T8131300 & data$T8131300 <= 999.0] <- 600.0
data$T8131300[1000.0 <= data$T8131300 & data$T8131300 <= 2099.0] <- 1000.0
data$T8131300[2100.0 <= data$T8131300 & data$T8131300 <= 4099.0] <- 2100.0
data$T8131300[4100.0 <= data$T8131300 & data$T8131300 <= 6099.0] <- 4100.0
data$T8131300[6100.0 <= data$T8131300 & data$T8131300 <= 9999.0] <- 6100.0
data$T8131300[10000.0 <= data$T8131300 & data$T8131300 <= 9999999.0] <- 10000.0
data$T8131300 <- factor(data$T8131300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131400[1.0 <= data$T8131400 & data$T8131400 <= 599.0] <- 1.0
data$T8131400[600.0 <= data$T8131400 & data$T8131400 <= 999.0] <- 600.0
data$T8131400[1000.0 <= data$T8131400 & data$T8131400 <= 2099.0] <- 1000.0
data$T8131400[2100.0 <= data$T8131400 & data$T8131400 <= 4099.0] <- 2100.0
data$T8131400[4100.0 <= data$T8131400 & data$T8131400 <= 6099.0] <- 4100.0
data$T8131400[6100.0 <= data$T8131400 & data$T8131400 <= 9999.0] <- 6100.0
data$T8131400[10000.0 <= data$T8131400 & data$T8131400 <= 9999999.0] <- 10000.0
data$T8131400 <- factor(data$T8131400,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131500[1.0 <= data$T8131500 & data$T8131500 <= 599.0] <- 1.0
data$T8131500[600.0 <= data$T8131500 & data$T8131500 <= 999.0] <- 600.0
data$T8131500[1000.0 <= data$T8131500 & data$T8131500 <= 2099.0] <- 1000.0
data$T8131500[2100.0 <= data$T8131500 & data$T8131500 <= 4099.0] <- 2100.0
data$T8131500[4100.0 <= data$T8131500 & data$T8131500 <= 6099.0] <- 4100.0
data$T8131500[6100.0 <= data$T8131500 & data$T8131500 <= 9999.0] <- 6100.0
data$T8131500[10000.0 <= data$T8131500 & data$T8131500 <= 9999999.0] <- 10000.0
data$T8131500 <- factor(data$T8131500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131600[1.0 <= data$T8131600 & data$T8131600 <= 599.0] <- 1.0
data$T8131600[600.0 <= data$T8131600 & data$T8131600 <= 999.0] <- 600.0
data$T8131600[1000.0 <= data$T8131600 & data$T8131600 <= 2099.0] <- 1000.0
data$T8131600[2100.0 <= data$T8131600 & data$T8131600 <= 4099.0] <- 2100.0
data$T8131600[4100.0 <= data$T8131600 & data$T8131600 <= 6099.0] <- 4100.0
data$T8131600[6100.0 <= data$T8131600 & data$T8131600 <= 9999.0] <- 6100.0
data$T8131600[10000.0 <= data$T8131600 & data$T8131600 <= 9999999.0] <- 10000.0
data$T8131600 <- factor(data$T8131600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8131700[1.0 <= data$T8131700 & data$T8131700 <= 599.0] <- 1.0
data$T8131700[600.0 <= data$T8131700 & data$T8131700 <= 999.0] <- 600.0
data$T8131700[1000.0 <= data$T8131700 & data$T8131700 <= 2099.0] <- 1000.0
data$T8131700[2100.0 <= data$T8131700 & data$T8131700 <= 4099.0] <- 2100.0
data$T8131700[4100.0 <= data$T8131700 & data$T8131700 <= 6099.0] <- 4100.0
data$T8131700[6100.0 <= data$T8131700 & data$T8131700 <= 9999.0] <- 6100.0
data$T8131700[10000.0 <= data$T8131700 & data$T8131700 <= 9999999.0] <- 10000.0
data$T8131700 <- factor(data$T8131700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$T8134300[10.0 <= data$T8134300 & data$T8134300 <= 999.0] <- 10.0
data$T8134300 <- factor(data$T8134300,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$U0010700[1.0 <= data$U0010700 & data$U0010700 <= 599.0] <- 1.0
data$U0010700[600.0 <= data$U0010700 & data$U0010700 <= 999.0] <- 600.0
data$U0010700[1000.0 <= data$U0010700 & data$U0010700 <= 2099.0] <- 1000.0
data$U0010700[2100.0 <= data$U0010700 & data$U0010700 <= 4099.0] <- 2100.0
data$U0010700[4100.0 <= data$U0010700 & data$U0010700 <= 6099.0] <- 4100.0
data$U0010700[6100.0 <= data$U0010700 & data$U0010700 <= 9999.0] <- 6100.0
data$U0010700[10000.0 <= data$U0010700 & data$U0010700 <= 9999999.0] <- 10000.0
data$U0010700 <- factor(data$U0010700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0010800[1.0 <= data$U0010800 & data$U0010800 <= 599.0] <- 1.0
data$U0010800[600.0 <= data$U0010800 & data$U0010800 <= 999.0] <- 600.0
data$U0010800[1000.0 <= data$U0010800 & data$U0010800 <= 2099.0] <- 1000.0
data$U0010800[2100.0 <= data$U0010800 & data$U0010800 <= 4099.0] <- 2100.0
data$U0010800[4100.0 <= data$U0010800 & data$U0010800 <= 6099.0] <- 4100.0
data$U0010800[6100.0 <= data$U0010800 & data$U0010800 <= 9999.0] <- 6100.0
data$U0010800[10000.0 <= data$U0010800 & data$U0010800 <= 9999999.0] <- 10000.0
data$U0010800 <- factor(data$U0010800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0010900[1.0 <= data$U0010900 & data$U0010900 <= 599.0] <- 1.0
data$U0010900[600.0 <= data$U0010900 & data$U0010900 <= 999.0] <- 600.0
data$U0010900[1000.0 <= data$U0010900 & data$U0010900 <= 2099.0] <- 1000.0
data$U0010900[2100.0 <= data$U0010900 & data$U0010900 <= 4099.0] <- 2100.0
data$U0010900[4100.0 <= data$U0010900 & data$U0010900 <= 6099.0] <- 4100.0
data$U0010900[6100.0 <= data$U0010900 & data$U0010900 <= 9999.0] <- 6100.0
data$U0010900[10000.0 <= data$U0010900 & data$U0010900 <= 9999999.0] <- 10000.0
data$U0010900 <- factor(data$U0010900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011000[1.0 <= data$U0011000 & data$U0011000 <= 599.0] <- 1.0
data$U0011000[600.0 <= data$U0011000 & data$U0011000 <= 999.0] <- 600.0
data$U0011000[1000.0 <= data$U0011000 & data$U0011000 <= 2099.0] <- 1000.0
data$U0011000[2100.0 <= data$U0011000 & data$U0011000 <= 4099.0] <- 2100.0
data$U0011000[4100.0 <= data$U0011000 & data$U0011000 <= 6099.0] <- 4100.0
data$U0011000[6100.0 <= data$U0011000 & data$U0011000 <= 9999.0] <- 6100.0
data$U0011000[10000.0 <= data$U0011000 & data$U0011000 <= 9999999.0] <- 10000.0
data$U0011000 <- factor(data$U0011000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011100[1.0 <= data$U0011100 & data$U0011100 <= 599.0] <- 1.0
data$U0011100[600.0 <= data$U0011100 & data$U0011100 <= 999.0] <- 600.0
data$U0011100[1000.0 <= data$U0011100 & data$U0011100 <= 2099.0] <- 1000.0
data$U0011100[2100.0 <= data$U0011100 & data$U0011100 <= 4099.0] <- 2100.0
data$U0011100[4100.0 <= data$U0011100 & data$U0011100 <= 6099.0] <- 4100.0
data$U0011100[6100.0 <= data$U0011100 & data$U0011100 <= 9999.0] <- 6100.0
data$U0011100[10000.0 <= data$U0011100 & data$U0011100 <= 9999999.0] <- 10000.0
data$U0011100 <- factor(data$U0011100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011200[1.0 <= data$U0011200 & data$U0011200 <= 599.0] <- 1.0
data$U0011200[600.0 <= data$U0011200 & data$U0011200 <= 999.0] <- 600.0
data$U0011200[1000.0 <= data$U0011200 & data$U0011200 <= 2099.0] <- 1000.0
data$U0011200[2100.0 <= data$U0011200 & data$U0011200 <= 4099.0] <- 2100.0
data$U0011200[4100.0 <= data$U0011200 & data$U0011200 <= 6099.0] <- 4100.0
data$U0011200[6100.0 <= data$U0011200 & data$U0011200 <= 9999.0] <- 6100.0
data$U0011200[10000.0 <= data$U0011200 & data$U0011200 <= 9999999.0] <- 10000.0
data$U0011200 <- factor(data$U0011200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011300[1.0 <= data$U0011300 & data$U0011300 <= 599.0] <- 1.0
data$U0011300[600.0 <= data$U0011300 & data$U0011300 <= 999.0] <- 600.0
data$U0011300[1000.0 <= data$U0011300 & data$U0011300 <= 2099.0] <- 1000.0
data$U0011300[2100.0 <= data$U0011300 & data$U0011300 <= 4099.0] <- 2100.0
data$U0011300[4100.0 <= data$U0011300 & data$U0011300 <= 6099.0] <- 4100.0
data$U0011300[6100.0 <= data$U0011300 & data$U0011300 <= 9999.0] <- 6100.0
data$U0011300[10000.0 <= data$U0011300 & data$U0011300 <= 9999999.0] <- 10000.0
data$U0011300 <- factor(data$U0011300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011400[1.0 <= data$U0011400 & data$U0011400 <= 599.0] <- 1.0
data$U0011400[600.0 <= data$U0011400 & data$U0011400 <= 999.0] <- 600.0
data$U0011400[1000.0 <= data$U0011400 & data$U0011400 <= 2099.0] <- 1000.0
data$U0011400[2100.0 <= data$U0011400 & data$U0011400 <= 4099.0] <- 2100.0
data$U0011400[4100.0 <= data$U0011400 & data$U0011400 <= 6099.0] <- 4100.0
data$U0011400[6100.0 <= data$U0011400 & data$U0011400 <= 9999.0] <- 6100.0
data$U0011400[10000.0 <= data$U0011400 & data$U0011400 <= 9999999.0] <- 10000.0
data$U0011400 <- factor(data$U0011400,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011500[1.0 <= data$U0011500 & data$U0011500 <= 599.0] <- 1.0
data$U0011500[600.0 <= data$U0011500 & data$U0011500 <= 999.0] <- 600.0
data$U0011500[1000.0 <= data$U0011500 & data$U0011500 <= 2099.0] <- 1000.0
data$U0011500[2100.0 <= data$U0011500 & data$U0011500 <= 4099.0] <- 2100.0
data$U0011500[4100.0 <= data$U0011500 & data$U0011500 <= 6099.0] <- 4100.0
data$U0011500[6100.0 <= data$U0011500 & data$U0011500 <= 9999.0] <- 6100.0
data$U0011500[10000.0 <= data$U0011500 & data$U0011500 <= 9999999.0] <- 10000.0
data$U0011500 <- factor(data$U0011500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011600[1.0 <= data$U0011600 & data$U0011600 <= 599.0] <- 1.0
data$U0011600[600.0 <= data$U0011600 & data$U0011600 <= 999.0] <- 600.0
data$U0011600[1000.0 <= data$U0011600 & data$U0011600 <= 2099.0] <- 1000.0
data$U0011600[2100.0 <= data$U0011600 & data$U0011600 <= 4099.0] <- 2100.0
data$U0011600[4100.0 <= data$U0011600 & data$U0011600 <= 6099.0] <- 4100.0
data$U0011600[6100.0 <= data$U0011600 & data$U0011600 <= 9999.0] <- 6100.0
data$U0011600[10000.0 <= data$U0011600 & data$U0011600 <= 9999999.0] <- 10000.0
data$U0011600 <- factor(data$U0011600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011700[1.0 <= data$U0011700 & data$U0011700 <= 599.0] <- 1.0
data$U0011700[600.0 <= data$U0011700 & data$U0011700 <= 999.0] <- 600.0
data$U0011700[1000.0 <= data$U0011700 & data$U0011700 <= 2099.0] <- 1000.0
data$U0011700[2100.0 <= data$U0011700 & data$U0011700 <= 4099.0] <- 2100.0
data$U0011700[4100.0 <= data$U0011700 & data$U0011700 <= 6099.0] <- 4100.0
data$U0011700[6100.0 <= data$U0011700 & data$U0011700 <= 9999.0] <- 6100.0
data$U0011700[10000.0 <= data$U0011700 & data$U0011700 <= 9999999.0] <- 10000.0
data$U0011700 <- factor(data$U0011700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0011800[1.0 <= data$U0011800 & data$U0011800 <= 599.0] <- 1.0
data$U0011800[600.0 <= data$U0011800 & data$U0011800 <= 999.0] <- 600.0
data$U0011800[1000.0 <= data$U0011800 & data$U0011800 <= 2099.0] <- 1000.0
data$U0011800[2100.0 <= data$U0011800 & data$U0011800 <= 4099.0] <- 2100.0
data$U0011800[4100.0 <= data$U0011800 & data$U0011800 <= 6099.0] <- 4100.0
data$U0011800[6100.0 <= data$U0011800 & data$U0011800 <= 9999.0] <- 6100.0
data$U0011800[10000.0 <= data$U0011800 & data$U0011800 <= 9999999.0] <- 10000.0
data$U0011800 <- factor(data$U0011800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U0014800[10.0 <= data$U0014800 & data$U0014800 <= 999.0] <- 10.0
data$U0014800 <- factor(data$U0014800,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$U0014901[10.0 <= data$U0014901 & data$U0014901 <= 999.0] <- 10.0
data$U0014901 <- factor(data$U0014901,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$U1847600[1.0 <= data$U1847600 & data$U1847600 <= 599.0] <- 1.0
data$U1847600[600.0 <= data$U1847600 & data$U1847600 <= 999.0] <- 600.0
data$U1847600[1000.0 <= data$U1847600 & data$U1847600 <= 2099.0] <- 1000.0
data$U1847600[2100.0 <= data$U1847600 & data$U1847600 <= 4099.0] <- 2100.0
data$U1847600[4100.0 <= data$U1847600 & data$U1847600 <= 6099.0] <- 4100.0
data$U1847600[6100.0 <= data$U1847600 & data$U1847600 <= 9999.0] <- 6100.0
data$U1847600[10000.0 <= data$U1847600 & data$U1847600 <= 9999999.0] <- 10000.0
data$U1847600 <- factor(data$U1847600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1847700[1.0 <= data$U1847700 & data$U1847700 <= 599.0] <- 1.0
data$U1847700[600.0 <= data$U1847700 & data$U1847700 <= 999.0] <- 600.0
data$U1847700[1000.0 <= data$U1847700 & data$U1847700 <= 2099.0] <- 1000.0
data$U1847700[2100.0 <= data$U1847700 & data$U1847700 <= 4099.0] <- 2100.0
data$U1847700[4100.0 <= data$U1847700 & data$U1847700 <= 6099.0] <- 4100.0
data$U1847700[6100.0 <= data$U1847700 & data$U1847700 <= 9999.0] <- 6100.0
data$U1847700[10000.0 <= data$U1847700 & data$U1847700 <= 9999999.0] <- 10000.0
data$U1847700 <- factor(data$U1847700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1847800[1.0 <= data$U1847800 & data$U1847800 <= 599.0] <- 1.0
data$U1847800[600.0 <= data$U1847800 & data$U1847800 <= 999.0] <- 600.0
data$U1847800[1000.0 <= data$U1847800 & data$U1847800 <= 2099.0] <- 1000.0
data$U1847800[2100.0 <= data$U1847800 & data$U1847800 <= 4099.0] <- 2100.0
data$U1847800[4100.0 <= data$U1847800 & data$U1847800 <= 6099.0] <- 4100.0
data$U1847800[6100.0 <= data$U1847800 & data$U1847800 <= 9999.0] <- 6100.0
data$U1847800[10000.0 <= data$U1847800 & data$U1847800 <= 9999999.0] <- 10000.0
data$U1847800 <- factor(data$U1847800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1847900[1.0 <= data$U1847900 & data$U1847900 <= 599.0] <- 1.0
data$U1847900[600.0 <= data$U1847900 & data$U1847900 <= 999.0] <- 600.0
data$U1847900[1000.0 <= data$U1847900 & data$U1847900 <= 2099.0] <- 1000.0
data$U1847900[2100.0 <= data$U1847900 & data$U1847900 <= 4099.0] <- 2100.0
data$U1847900[4100.0 <= data$U1847900 & data$U1847900 <= 6099.0] <- 4100.0
data$U1847900[6100.0 <= data$U1847900 & data$U1847900 <= 9999.0] <- 6100.0
data$U1847900[10000.0 <= data$U1847900 & data$U1847900 <= 9999999.0] <- 10000.0
data$U1847900 <- factor(data$U1847900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848000[1.0 <= data$U1848000 & data$U1848000 <= 599.0] <- 1.0
data$U1848000[600.0 <= data$U1848000 & data$U1848000 <= 999.0] <- 600.0
data$U1848000[1000.0 <= data$U1848000 & data$U1848000 <= 2099.0] <- 1000.0
data$U1848000[2100.0 <= data$U1848000 & data$U1848000 <= 4099.0] <- 2100.0
data$U1848000[4100.0 <= data$U1848000 & data$U1848000 <= 6099.0] <- 4100.0
data$U1848000[6100.0 <= data$U1848000 & data$U1848000 <= 9999.0] <- 6100.0
data$U1848000[10000.0 <= data$U1848000 & data$U1848000 <= 9999999.0] <- 10000.0
data$U1848000 <- factor(data$U1848000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848100[1.0 <= data$U1848100 & data$U1848100 <= 599.0] <- 1.0
data$U1848100[600.0 <= data$U1848100 & data$U1848100 <= 999.0] <- 600.0
data$U1848100[1000.0 <= data$U1848100 & data$U1848100 <= 2099.0] <- 1000.0
data$U1848100[2100.0 <= data$U1848100 & data$U1848100 <= 4099.0] <- 2100.0
data$U1848100[4100.0 <= data$U1848100 & data$U1848100 <= 6099.0] <- 4100.0
data$U1848100[6100.0 <= data$U1848100 & data$U1848100 <= 9999.0] <- 6100.0
data$U1848100[10000.0 <= data$U1848100 & data$U1848100 <= 9999999.0] <- 10000.0
data$U1848100 <- factor(data$U1848100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848200[1.0 <= data$U1848200 & data$U1848200 <= 599.0] <- 1.0
data$U1848200[600.0 <= data$U1848200 & data$U1848200 <= 999.0] <- 600.0
data$U1848200[1000.0 <= data$U1848200 & data$U1848200 <= 2099.0] <- 1000.0
data$U1848200[2100.0 <= data$U1848200 & data$U1848200 <= 4099.0] <- 2100.0
data$U1848200[4100.0 <= data$U1848200 & data$U1848200 <= 6099.0] <- 4100.0
data$U1848200[6100.0 <= data$U1848200 & data$U1848200 <= 9999.0] <- 6100.0
data$U1848200[10000.0 <= data$U1848200 & data$U1848200 <= 9999999.0] <- 10000.0
data$U1848200 <- factor(data$U1848200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848300[1.0 <= data$U1848300 & data$U1848300 <= 599.0] <- 1.0
data$U1848300[600.0 <= data$U1848300 & data$U1848300 <= 999.0] <- 600.0
data$U1848300[1000.0 <= data$U1848300 & data$U1848300 <= 2099.0] <- 1000.0
data$U1848300[2100.0 <= data$U1848300 & data$U1848300 <= 4099.0] <- 2100.0
data$U1848300[4100.0 <= data$U1848300 & data$U1848300 <= 6099.0] <- 4100.0
data$U1848300[6100.0 <= data$U1848300 & data$U1848300 <= 9999.0] <- 6100.0
data$U1848300[10000.0 <= data$U1848300 & data$U1848300 <= 9999999.0] <- 10000.0
data$U1848300 <- factor(data$U1848300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848400[1.0 <= data$U1848400 & data$U1848400 <= 599.0] <- 1.0
data$U1848400[600.0 <= data$U1848400 & data$U1848400 <= 999.0] <- 600.0
data$U1848400[1000.0 <= data$U1848400 & data$U1848400 <= 2099.0] <- 1000.0
data$U1848400[2100.0 <= data$U1848400 & data$U1848400 <= 4099.0] <- 2100.0
data$U1848400[4100.0 <= data$U1848400 & data$U1848400 <= 6099.0] <- 4100.0
data$U1848400[6100.0 <= data$U1848400 & data$U1848400 <= 9999.0] <- 6100.0
data$U1848400[10000.0 <= data$U1848400 & data$U1848400 <= 9999999.0] <- 10000.0
data$U1848400 <- factor(data$U1848400,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848500[1.0 <= data$U1848500 & data$U1848500 <= 599.0] <- 1.0
data$U1848500[600.0 <= data$U1848500 & data$U1848500 <= 999.0] <- 600.0
data$U1848500[1000.0 <= data$U1848500 & data$U1848500 <= 2099.0] <- 1000.0
data$U1848500[2100.0 <= data$U1848500 & data$U1848500 <= 4099.0] <- 2100.0
data$U1848500[4100.0 <= data$U1848500 & data$U1848500 <= 6099.0] <- 4100.0
data$U1848500[6100.0 <= data$U1848500 & data$U1848500 <= 9999.0] <- 6100.0
data$U1848500[10000.0 <= data$U1848500 & data$U1848500 <= 9999999.0] <- 10000.0
data$U1848500 <- factor(data$U1848500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848600[1.0 <= data$U1848600 & data$U1848600 <= 599.0] <- 1.0
data$U1848600[600.0 <= data$U1848600 & data$U1848600 <= 999.0] <- 600.0
data$U1848600[1000.0 <= data$U1848600 & data$U1848600 <= 2099.0] <- 1000.0
data$U1848600[2100.0 <= data$U1848600 & data$U1848600 <= 4099.0] <- 2100.0
data$U1848600[4100.0 <= data$U1848600 & data$U1848600 <= 6099.0] <- 4100.0
data$U1848600[6100.0 <= data$U1848600 & data$U1848600 <= 9999.0] <- 6100.0
data$U1848600[10000.0 <= data$U1848600 & data$U1848600 <= 9999999.0] <- 10000.0
data$U1848600 <- factor(data$U1848600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848700[1.0 <= data$U1848700 & data$U1848700 <= 599.0] <- 1.0
data$U1848700[600.0 <= data$U1848700 & data$U1848700 <= 999.0] <- 600.0
data$U1848700[1000.0 <= data$U1848700 & data$U1848700 <= 2099.0] <- 1000.0
data$U1848700[2100.0 <= data$U1848700 & data$U1848700 <= 4099.0] <- 2100.0
data$U1848700[4100.0 <= data$U1848700 & data$U1848700 <= 6099.0] <- 4100.0
data$U1848700[6100.0 <= data$U1848700 & data$U1848700 <= 9999.0] <- 6100.0
data$U1848700[10000.0 <= data$U1848700 & data$U1848700 <= 9999999.0] <- 10000.0
data$U1848700 <- factor(data$U1848700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848800[1.0 <= data$U1848800 & data$U1848800 <= 599.0] <- 1.0
data$U1848800[600.0 <= data$U1848800 & data$U1848800 <= 999.0] <- 600.0
data$U1848800[1000.0 <= data$U1848800 & data$U1848800 <= 2099.0] <- 1000.0
data$U1848800[2100.0 <= data$U1848800 & data$U1848800 <= 4099.0] <- 2100.0
data$U1848800[4100.0 <= data$U1848800 & data$U1848800 <= 6099.0] <- 4100.0
data$U1848800[6100.0 <= data$U1848800 & data$U1848800 <= 9999.0] <- 6100.0
data$U1848800[10000.0 <= data$U1848800 & data$U1848800 <= 9999999.0] <- 10000.0
data$U1848800 <- factor(data$U1848800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1848900[1.0 <= data$U1848900 & data$U1848900 <= 599.0] <- 1.0
data$U1848900[600.0 <= data$U1848900 & data$U1848900 <= 999.0] <- 600.0
data$U1848900[1000.0 <= data$U1848900 & data$U1848900 <= 2099.0] <- 1000.0
data$U1848900[2100.0 <= data$U1848900 & data$U1848900 <= 4099.0] <- 2100.0
data$U1848900[4100.0 <= data$U1848900 & data$U1848900 <= 6099.0] <- 4100.0
data$U1848900[6100.0 <= data$U1848900 & data$U1848900 <= 9999.0] <- 6100.0
data$U1848900[10000.0 <= data$U1848900 & data$U1848900 <= 9999999.0] <- 10000.0
data$U1848900 <- factor(data$U1848900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1849000[1.0 <= data$U1849000 & data$U1849000 <= 599.0] <- 1.0
data$U1849000[600.0 <= data$U1849000 & data$U1849000 <= 999.0] <- 600.0
data$U1849000[1000.0 <= data$U1849000 & data$U1849000 <= 2099.0] <- 1000.0
data$U1849000[2100.0 <= data$U1849000 & data$U1849000 <= 4099.0] <- 2100.0
data$U1849000[4100.0 <= data$U1849000 & data$U1849000 <= 6099.0] <- 4100.0
data$U1849000[6100.0 <= data$U1849000 & data$U1849000 <= 9999.0] <- 6100.0
data$U1849000[10000.0 <= data$U1849000 & data$U1849000 <= 9999999.0] <- 10000.0
data$U1849000 <- factor(data$U1849000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U1852600[10.0 <= data$U1852600 & data$U1852600 <= 999.0] <- 10.0
data$U1852600 <- factor(data$U1852600,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$U1852800[10.0 <= data$U1852800 & data$U1852800 <= 999.0] <- 10.0
data$U1852800 <- factor(data$U1852800,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$U3445700[1.0 <= data$U3445700 & data$U3445700 <= 599.0] <- 1.0
data$U3445700[600.0 <= data$U3445700 & data$U3445700 <= 999.0] <- 600.0
data$U3445700[1000.0 <= data$U3445700 & data$U3445700 <= 2099.0] <- 1000.0
data$U3445700[2100.0 <= data$U3445700 & data$U3445700 <= 4099.0] <- 2100.0
data$U3445700[4100.0 <= data$U3445700 & data$U3445700 <= 6099.0] <- 4100.0
data$U3445700[6100.0 <= data$U3445700 & data$U3445700 <= 9999.0] <- 6100.0
data$U3445700[10000.0 <= data$U3445700 & data$U3445700 <= 9999999.0] <- 10000.0
data$U3445700 <- factor(data$U3445700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3445800[1.0 <= data$U3445800 & data$U3445800 <= 599.0] <- 1.0
data$U3445800[600.0 <= data$U3445800 & data$U3445800 <= 999.0] <- 600.0
data$U3445800[1000.0 <= data$U3445800 & data$U3445800 <= 2099.0] <- 1000.0
data$U3445800[2100.0 <= data$U3445800 & data$U3445800 <= 4099.0] <- 2100.0
data$U3445800[4100.0 <= data$U3445800 & data$U3445800 <= 6099.0] <- 4100.0
data$U3445800[6100.0 <= data$U3445800 & data$U3445800 <= 9999.0] <- 6100.0
data$U3445800[10000.0 <= data$U3445800 & data$U3445800 <= 9999999.0] <- 10000.0
data$U3445800 <- factor(data$U3445800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3445900[1.0 <= data$U3445900 & data$U3445900 <= 599.0] <- 1.0
data$U3445900[600.0 <= data$U3445900 & data$U3445900 <= 999.0] <- 600.0
data$U3445900[1000.0 <= data$U3445900 & data$U3445900 <= 2099.0] <- 1000.0
data$U3445900[2100.0 <= data$U3445900 & data$U3445900 <= 4099.0] <- 2100.0
data$U3445900[4100.0 <= data$U3445900 & data$U3445900 <= 6099.0] <- 4100.0
data$U3445900[6100.0 <= data$U3445900 & data$U3445900 <= 9999.0] <- 6100.0
data$U3445900[10000.0 <= data$U3445900 & data$U3445900 <= 9999999.0] <- 10000.0
data$U3445900 <- factor(data$U3445900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446000[1.0 <= data$U3446000 & data$U3446000 <= 599.0] <- 1.0
data$U3446000[600.0 <= data$U3446000 & data$U3446000 <= 999.0] <- 600.0
data$U3446000[1000.0 <= data$U3446000 & data$U3446000 <= 2099.0] <- 1000.0
data$U3446000[2100.0 <= data$U3446000 & data$U3446000 <= 4099.0] <- 2100.0
data$U3446000[4100.0 <= data$U3446000 & data$U3446000 <= 6099.0] <- 4100.0
data$U3446000[6100.0 <= data$U3446000 & data$U3446000 <= 9999.0] <- 6100.0
data$U3446000[10000.0 <= data$U3446000 & data$U3446000 <= 9999999.0] <- 10000.0
data$U3446000 <- factor(data$U3446000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446100[1.0 <= data$U3446100 & data$U3446100 <= 599.0] <- 1.0
data$U3446100[600.0 <= data$U3446100 & data$U3446100 <= 999.0] <- 600.0
data$U3446100[1000.0 <= data$U3446100 & data$U3446100 <= 2099.0] <- 1000.0
data$U3446100[2100.0 <= data$U3446100 & data$U3446100 <= 4099.0] <- 2100.0
data$U3446100[4100.0 <= data$U3446100 & data$U3446100 <= 6099.0] <- 4100.0
data$U3446100[6100.0 <= data$U3446100 & data$U3446100 <= 9999.0] <- 6100.0
data$U3446100[10000.0 <= data$U3446100 & data$U3446100 <= 9999999.0] <- 10000.0
data$U3446100 <- factor(data$U3446100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446200[1.0 <= data$U3446200 & data$U3446200 <= 599.0] <- 1.0
data$U3446200[600.0 <= data$U3446200 & data$U3446200 <= 999.0] <- 600.0
data$U3446200[1000.0 <= data$U3446200 & data$U3446200 <= 2099.0] <- 1000.0
data$U3446200[2100.0 <= data$U3446200 & data$U3446200 <= 4099.0] <- 2100.0
data$U3446200[4100.0 <= data$U3446200 & data$U3446200 <= 6099.0] <- 4100.0
data$U3446200[6100.0 <= data$U3446200 & data$U3446200 <= 9999.0] <- 6100.0
data$U3446200[10000.0 <= data$U3446200 & data$U3446200 <= 9999999.0] <- 10000.0
data$U3446200 <- factor(data$U3446200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446300[1.0 <= data$U3446300 & data$U3446300 <= 599.0] <- 1.0
data$U3446300[600.0 <= data$U3446300 & data$U3446300 <= 999.0] <- 600.0
data$U3446300[1000.0 <= data$U3446300 & data$U3446300 <= 2099.0] <- 1000.0
data$U3446300[2100.0 <= data$U3446300 & data$U3446300 <= 4099.0] <- 2100.0
data$U3446300[4100.0 <= data$U3446300 & data$U3446300 <= 6099.0] <- 4100.0
data$U3446300[6100.0 <= data$U3446300 & data$U3446300 <= 9999.0] <- 6100.0
data$U3446300[10000.0 <= data$U3446300 & data$U3446300 <= 9999999.0] <- 10000.0
data$U3446300 <- factor(data$U3446300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446400[1.0 <= data$U3446400 & data$U3446400 <= 599.0] <- 1.0
data$U3446400[600.0 <= data$U3446400 & data$U3446400 <= 999.0] <- 600.0
data$U3446400[1000.0 <= data$U3446400 & data$U3446400 <= 2099.0] <- 1000.0
data$U3446400[2100.0 <= data$U3446400 & data$U3446400 <= 4099.0] <- 2100.0
data$U3446400[4100.0 <= data$U3446400 & data$U3446400 <= 6099.0] <- 4100.0
data$U3446400[6100.0 <= data$U3446400 & data$U3446400 <= 9999.0] <- 6100.0
data$U3446400[10000.0 <= data$U3446400 & data$U3446400 <= 9999999.0] <- 10000.0
data$U3446400 <- factor(data$U3446400,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446500[1.0 <= data$U3446500 & data$U3446500 <= 599.0] <- 1.0
data$U3446500[600.0 <= data$U3446500 & data$U3446500 <= 999.0] <- 600.0
data$U3446500[1000.0 <= data$U3446500 & data$U3446500 <= 2099.0] <- 1000.0
data$U3446500[2100.0 <= data$U3446500 & data$U3446500 <= 4099.0] <- 2100.0
data$U3446500[4100.0 <= data$U3446500 & data$U3446500 <= 6099.0] <- 4100.0
data$U3446500[6100.0 <= data$U3446500 & data$U3446500 <= 9999.0] <- 6100.0
data$U3446500[10000.0 <= data$U3446500 & data$U3446500 <= 9999999.0] <- 10000.0
data$U3446500 <- factor(data$U3446500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446600[1.0 <= data$U3446600 & data$U3446600 <= 599.0] <- 1.0
data$U3446600[600.0 <= data$U3446600 & data$U3446600 <= 999.0] <- 600.0
data$U3446600[1000.0 <= data$U3446600 & data$U3446600 <= 2099.0] <- 1000.0
data$U3446600[2100.0 <= data$U3446600 & data$U3446600 <= 4099.0] <- 2100.0
data$U3446600[4100.0 <= data$U3446600 & data$U3446600 <= 6099.0] <- 4100.0
data$U3446600[6100.0 <= data$U3446600 & data$U3446600 <= 9999.0] <- 6100.0
data$U3446600[10000.0 <= data$U3446600 & data$U3446600 <= 9999999.0] <- 10000.0
data$U3446600 <- factor(data$U3446600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3446700[1.0 <= data$U3446700 & data$U3446700 <= 599.0] <- 1.0
data$U3446700[600.0 <= data$U3446700 & data$U3446700 <= 999.0] <- 600.0
data$U3446700[1000.0 <= data$U3446700 & data$U3446700 <= 2099.0] <- 1000.0
data$U3446700[2100.0 <= data$U3446700 & data$U3446700 <= 4099.0] <- 2100.0
data$U3446700[4100.0 <= data$U3446700 & data$U3446700 <= 6099.0] <- 4100.0
data$U3446700[6100.0 <= data$U3446700 & data$U3446700 <= 9999.0] <- 6100.0
data$U3446700[10000.0 <= data$U3446700 & data$U3446700 <= 9999999.0] <- 10000.0
data$U3446700 <- factor(data$U3446700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U3451900[10.0 <= data$U3451900 & data$U3451900 <= 999.0] <- 10.0
data$U3451900 <- factor(data$U3451900,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$U4951300[1.0 <= data$U4951300 & data$U4951300 <= 599.0] <- 1.0
data$U4951300[600.0 <= data$U4951300 & data$U4951300 <= 999.0] <- 600.0
data$U4951300[1000.0 <= data$U4951300 & data$U4951300 <= 2099.0] <- 1000.0
data$U4951300[2100.0 <= data$U4951300 & data$U4951300 <= 4099.0] <- 2100.0
data$U4951300[4100.0 <= data$U4951300 & data$U4951300 <= 6099.0] <- 4100.0
data$U4951300[6100.0 <= data$U4951300 & data$U4951300 <= 9999.0] <- 6100.0
data$U4951300[10000.0 <= data$U4951300 & data$U4951300 <= 9999999.0] <- 10000.0
data$U4951300 <- factor(data$U4951300,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4951400[1.0 <= data$U4951400 & data$U4951400 <= 599.0] <- 1.0
data$U4951400[600.0 <= data$U4951400 & data$U4951400 <= 999.0] <- 600.0
data$U4951400[1000.0 <= data$U4951400 & data$U4951400 <= 2099.0] <- 1000.0
data$U4951400[2100.0 <= data$U4951400 & data$U4951400 <= 4099.0] <- 2100.0
data$U4951400[4100.0 <= data$U4951400 & data$U4951400 <= 6099.0] <- 4100.0
data$U4951400[6100.0 <= data$U4951400 & data$U4951400 <= 9999.0] <- 6100.0
data$U4951400[10000.0 <= data$U4951400 & data$U4951400 <= 9999999.0] <- 10000.0
data$U4951400 <- factor(data$U4951400,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4951500[1.0 <= data$U4951500 & data$U4951500 <= 599.0] <- 1.0
data$U4951500[600.0 <= data$U4951500 & data$U4951500 <= 999.0] <- 600.0
data$U4951500[1000.0 <= data$U4951500 & data$U4951500 <= 2099.0] <- 1000.0
data$U4951500[2100.0 <= data$U4951500 & data$U4951500 <= 4099.0] <- 2100.0
data$U4951500[4100.0 <= data$U4951500 & data$U4951500 <= 6099.0] <- 4100.0
data$U4951500[6100.0 <= data$U4951500 & data$U4951500 <= 9999.0] <- 6100.0
data$U4951500[10000.0 <= data$U4951500 & data$U4951500 <= 9999999.0] <- 10000.0
data$U4951500 <- factor(data$U4951500,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4951600[1.0 <= data$U4951600 & data$U4951600 <= 599.0] <- 1.0
data$U4951600[600.0 <= data$U4951600 & data$U4951600 <= 999.0] <- 600.0
data$U4951600[1000.0 <= data$U4951600 & data$U4951600 <= 2099.0] <- 1000.0
data$U4951600[2100.0 <= data$U4951600 & data$U4951600 <= 4099.0] <- 2100.0
data$U4951600[4100.0 <= data$U4951600 & data$U4951600 <= 6099.0] <- 4100.0
data$U4951600[6100.0 <= data$U4951600 & data$U4951600 <= 9999.0] <- 6100.0
data$U4951600[10000.0 <= data$U4951600 & data$U4951600 <= 9999999.0] <- 10000.0
data$U4951600 <- factor(data$U4951600,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4951700[1.0 <= data$U4951700 & data$U4951700 <= 599.0] <- 1.0
data$U4951700[600.0 <= data$U4951700 & data$U4951700 <= 999.0] <- 600.0
data$U4951700[1000.0 <= data$U4951700 & data$U4951700 <= 2099.0] <- 1000.0
data$U4951700[2100.0 <= data$U4951700 & data$U4951700 <= 4099.0] <- 2100.0
data$U4951700[4100.0 <= data$U4951700 & data$U4951700 <= 6099.0] <- 4100.0
data$U4951700[6100.0 <= data$U4951700 & data$U4951700 <= 9999.0] <- 6100.0
data$U4951700[10000.0 <= data$U4951700 & data$U4951700 <= 9999999.0] <- 10000.0
data$U4951700 <- factor(data$U4951700,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4951800[1.0 <= data$U4951800 & data$U4951800 <= 599.0] <- 1.0
data$U4951800[600.0 <= data$U4951800 & data$U4951800 <= 999.0] <- 600.0
data$U4951800[1000.0 <= data$U4951800 & data$U4951800 <= 2099.0] <- 1000.0
data$U4951800[2100.0 <= data$U4951800 & data$U4951800 <= 4099.0] <- 2100.0
data$U4951800[4100.0 <= data$U4951800 & data$U4951800 <= 6099.0] <- 4100.0
data$U4951800[6100.0 <= data$U4951800 & data$U4951800 <= 9999.0] <- 6100.0
data$U4951800[10000.0 <= data$U4951800 & data$U4951800 <= 9999999.0] <- 10000.0
data$U4951800 <- factor(data$U4951800,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4951900[1.0 <= data$U4951900 & data$U4951900 <= 599.0] <- 1.0
data$U4951900[600.0 <= data$U4951900 & data$U4951900 <= 999.0] <- 600.0
data$U4951900[1000.0 <= data$U4951900 & data$U4951900 <= 2099.0] <- 1000.0
data$U4951900[2100.0 <= data$U4951900 & data$U4951900 <= 4099.0] <- 2100.0
data$U4951900[4100.0 <= data$U4951900 & data$U4951900 <= 6099.0] <- 4100.0
data$U4951900[6100.0 <= data$U4951900 & data$U4951900 <= 9999.0] <- 6100.0
data$U4951900[10000.0 <= data$U4951900 & data$U4951900 <= 9999999.0] <- 10000.0
data$U4951900 <- factor(data$U4951900,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4952000[1.0 <= data$U4952000 & data$U4952000 <= 599.0] <- 1.0
data$U4952000[600.0 <= data$U4952000 & data$U4952000 <= 999.0] <- 600.0
data$U4952000[1000.0 <= data$U4952000 & data$U4952000 <= 2099.0] <- 1000.0
data$U4952000[2100.0 <= data$U4952000 & data$U4952000 <= 4099.0] <- 2100.0
data$U4952000[4100.0 <= data$U4952000 & data$U4952000 <= 6099.0] <- 4100.0
data$U4952000[6100.0 <= data$U4952000 & data$U4952000 <= 9999.0] <- 6100.0
data$U4952000[10000.0 <= data$U4952000 & data$U4952000 <= 9999999.0] <- 10000.0
data$U4952000 <- factor(data$U4952000,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4952100[1.0 <= data$U4952100 & data$U4952100 <= 599.0] <- 1.0
data$U4952100[600.0 <= data$U4952100 & data$U4952100 <= 999.0] <- 600.0
data$U4952100[1000.0 <= data$U4952100 & data$U4952100 <= 2099.0] <- 1000.0
data$U4952100[2100.0 <= data$U4952100 & data$U4952100 <= 4099.0] <- 2100.0
data$U4952100[4100.0 <= data$U4952100 & data$U4952100 <= 6099.0] <- 4100.0
data$U4952100[6100.0 <= data$U4952100 & data$U4952100 <= 9999.0] <- 6100.0
data$U4952100[10000.0 <= data$U4952100 & data$U4952100 <= 9999999.0] <- 10000.0
data$U4952100 <- factor(data$U4952100,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4952200[1.0 <= data$U4952200 & data$U4952200 <= 599.0] <- 1.0
data$U4952200[600.0 <= data$U4952200 & data$U4952200 <= 999.0] <- 600.0
data$U4952200[1000.0 <= data$U4952200 & data$U4952200 <= 2099.0] <- 1000.0
data$U4952200[2100.0 <= data$U4952200 & data$U4952200 <= 4099.0] <- 2100.0
data$U4952200[4100.0 <= data$U4952200 & data$U4952200 <= 6099.0] <- 4100.0
data$U4952200[6100.0 <= data$U4952200 & data$U4952200 <= 9999.0] <- 6100.0
data$U4952200[10000.0 <= data$U4952200 & data$U4952200 <= 9999999.0] <- 10000.0
data$U4952200 <- factor(data$U4952200,
levels=c(0.0,1.0,600.0,1000.0,2100.0,4100.0,6100.0,10000.0),
labels=c("0",
"1 TO 599: 0.01-5.99",
"600 TO 999: 6.00-9.99",
"1000 TO 2099: 10.00-20.99",
"2100 TO 4099: 21.00-40.99",
"4100 TO 6099: 41.00-60.99",
"6100 TO 9999: 61.00-99.99",
"10000 TO 9999999: 100.00+"))
data$U4955000[10.0 <= data$U4955000 & data$U4955000 <= 999.0] <- 10.0
data$U4955000 <- factor(data$U4955000,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10 TO 999: 10+"))
data$Z9065401[0.0 <= data$Z9065401 & data$Z9065401 <= 50.0] <- 0.0
data$Z9065401[51.0 <= data$Z9065401 & data$Z9065401 <= 100.0] <- 51.0
data$Z9065401[101.0 <= data$Z9065401 & data$Z9065401 <= 150.0] <- 101.0
data$Z9065401[151.0 <= data$Z9065401 & data$Z9065401 <= 200.0] <- 151.0
data$Z9065401[201.0 <= data$Z9065401 & data$Z9065401 <= 250.0] <- 201.0
data$Z9065401[251.0 <= data$Z9065401 & data$Z9065401 <= 300.0] <- 251.0
data$Z9065401[301.0 <= data$Z9065401 & data$Z9065401 <= 350.0] <- 301.0
data$Z9065401[351.0 <= data$Z9065401 & data$Z9065401 <= 400.0] <- 351.0
data$Z9065401[401.0 <= data$Z9065401 & data$Z9065401 <= 450.0] <- 401.0
data$Z9065401[451.0 <= data$Z9065401 & data$Z9065401 <= 500.0] <- 451.0
data$Z9065401[501.0 <= data$Z9065401 & data$Z9065401 <= 550.0] <- 501.0
data$Z9065401[551.0 <= data$Z9065401 & data$Z9065401 <= 600.0] <- 551.0
data$Z9065401[601.0 <= data$Z9065401 & data$Z9065401 <= 650.0] <- 601.0
data$Z9065401[651.0 <= data$Z9065401 & data$Z9065401 <= 700.0] <- 651.0
data$Z9065401[701.0 <= data$Z9065401 & data$Z9065401 <= 750.0] <- 701.0
data$Z9065401[751.0 <= data$Z9065401 & data$Z9065401 <= 800.0] <- 751.0
data$Z9065401[801.0 <= data$Z9065401 & data$Z9065401 <= 850.0] <- 801.0
data$Z9065401[851.0 <= data$Z9065401 & data$Z9065401 <= 900.0] <- 851.0
data$Z9065401[901.0 <= data$Z9065401 & data$Z9065401 <= 950.0] <- 901.0
data$Z9065401[951.0 <= data$Z9065401 & data$Z9065401 <= 1000.0] <- 951.0
data$Z9065401[1001.0 <= data$Z9065401 & data$Z9065401 <= 1050.0] <- 1001.0
data$Z9065401[1051.0 <= data$Z9065401 & data$Z9065401 <= 1100.0] <- 1051.0
data$Z9065401[1101.0 <= data$Z9065401 & data$Z9065401 <= 1150.0] <- 1101.0
data$Z9065401[1151.0 <= data$Z9065401 & data$Z9065401 <= 1200.0] <- 1151.0
data$Z9065401[1201.0 <= data$Z9065401 & data$Z9065401 <= 1250.0] <- 1201.0
data$Z9065401[1251.0 <= data$Z9065401 & data$Z9065401 <= 1300.0] <- 1251.0
data$Z9065401 <- factor(data$Z9065401,
levels=c(0.0,51.0,101.0,151.0,201.0,251.0,301.0,351.0,401.0,451.0,501.0,551.0,601.0,651.0,701.0,751.0,801.0,851.0,901.0,951.0,1001.0,1051.0,1101.0,1151.0,1201.0,1251.0),
labels=c("0 TO 50: weeks",
"51 TO 100: weeks",
"101 TO 150: weeks",
"151 TO 200: weeks",
"201 TO 250: weeks",
"251 TO 300: weeks",
"301 TO 350: weeks",
"351 TO 400: weeks",
"401 TO 450: weeks",
"451 TO 500: weeks",
"501 TO 550: weeks",
"551 TO 600: weeks",
"601 TO 650: weeks",
"651 TO 700: weeks",
"701 TO 750: weeks",
"751 TO 800: weeks",
"801 TO 850: weeks",
"851 TO 900: weeks",
"901 TO 950: weeks",
"951 TO 1000: weeks",
"1001 TO 1050: weeks",
"1051 TO 1100: weeks",
"1101 TO 1150: weeks",
"1151 TO 1200: weeks",
"1201 TO 1250: weeks",
"1251 TO 1300: weeks"))
data$Z9068800[1.0 <= data$Z9068800 & data$Z9068800 <= 499.0] <- 1.0
data$Z9068800[500.0 <= data$Z9068800 & data$Z9068800 <= 999.0] <- 500.0
data$Z9068800[1000.0 <= data$Z9068800 & data$Z9068800 <= 1499.0] <- 1000.0
data$Z9068800[1500.0 <= data$Z9068800 & data$Z9068800 <= 1999.0] <- 1500.0
data$Z9068800[2000.0 <= data$Z9068800 & data$Z9068800 <= 2499.0] <- 2000.0
data$Z9068800[2500.0 <= data$Z9068800 & data$Z9068800 <= 2999.0] <- 2500.0
data$Z9068800[3000.0 <= data$Z9068800 & data$Z9068800 <= 3499.0] <- 3000.0
data$Z9068800[3500.0 <= data$Z9068800 & data$Z9068800 <= 3999.0] <- 3500.0
data$Z9068800[4000.0 <= data$Z9068800 & data$Z9068800 <= 4499.0] <- 4000.0
data$Z9068800[4500.0 <= data$Z9068800 & data$Z9068800 <= 4999.0] <- 4500.0
data$Z9068800[5000.0 <= data$Z9068800 & data$Z9068800 <= 9.9999999E7] <- 5000.0
data$Z9068800 <- factor(data$Z9068800,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9068900[1.0 <= data$Z9068900 & data$Z9068900 <= 499.0] <- 1.0
data$Z9068900[500.0 <= data$Z9068900 & data$Z9068900 <= 999.0] <- 500.0
data$Z9068900[1000.0 <= data$Z9068900 & data$Z9068900 <= 1499.0] <- 1000.0
data$Z9068900[1500.0 <= data$Z9068900 & data$Z9068900 <= 1999.0] <- 1500.0
data$Z9068900[2000.0 <= data$Z9068900 & data$Z9068900 <= 2499.0] <- 2000.0
data$Z9068900[2500.0 <= data$Z9068900 & data$Z9068900 <= 2999.0] <- 2500.0
data$Z9068900[3000.0 <= data$Z9068900 & data$Z9068900 <= 3499.0] <- 3000.0
data$Z9068900[3500.0 <= data$Z9068900 & data$Z9068900 <= 3999.0] <- 3500.0
data$Z9068900[4000.0 <= data$Z9068900 & data$Z9068900 <= 4499.0] <- 4000.0
data$Z9068900[4500.0 <= data$Z9068900 & data$Z9068900 <= 4999.0] <- 4500.0
data$Z9068900[5000.0 <= data$Z9068900 & data$Z9068900 <= 9.9999999E7] <- 5000.0
data$Z9068900 <- factor(data$Z9068900,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069000[1.0 <= data$Z9069000 & data$Z9069000 <= 499.0] <- 1.0
data$Z9069000[500.0 <= data$Z9069000 & data$Z9069000 <= 999.0] <- 500.0
data$Z9069000[1000.0 <= data$Z9069000 & data$Z9069000 <= 1499.0] <- 1000.0
data$Z9069000[1500.0 <= data$Z9069000 & data$Z9069000 <= 1999.0] <- 1500.0
data$Z9069000[2000.0 <= data$Z9069000 & data$Z9069000 <= 2499.0] <- 2000.0
data$Z9069000[2500.0 <= data$Z9069000 & data$Z9069000 <= 2999.0] <- 2500.0
data$Z9069000[3000.0 <= data$Z9069000 & data$Z9069000 <= 3499.0] <- 3000.0
data$Z9069000[3500.0 <= data$Z9069000 & data$Z9069000 <= 3999.0] <- 3500.0
data$Z9069000[4000.0 <= data$Z9069000 & data$Z9069000 <= 4499.0] <- 4000.0
data$Z9069000[4500.0 <= data$Z9069000 & data$Z9069000 <= 4999.0] <- 4500.0
data$Z9069000[5000.0 <= data$Z9069000 & data$Z9069000 <= 9.9999999E7] <- 5000.0
data$Z9069000 <- factor(data$Z9069000,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069100[1.0 <= data$Z9069100 & data$Z9069100 <= 499.0] <- 1.0
data$Z9069100[500.0 <= data$Z9069100 & data$Z9069100 <= 999.0] <- 500.0
data$Z9069100[1000.0 <= data$Z9069100 & data$Z9069100 <= 1499.0] <- 1000.0
data$Z9069100[1500.0 <= data$Z9069100 & data$Z9069100 <= 1999.0] <- 1500.0
data$Z9069100[2000.0 <= data$Z9069100 & data$Z9069100 <= 2499.0] <- 2000.0
data$Z9069100[2500.0 <= data$Z9069100 & data$Z9069100 <= 2999.0] <- 2500.0
data$Z9069100[3000.0 <= data$Z9069100 & data$Z9069100 <= 3499.0] <- 3000.0
data$Z9069100[3500.0 <= data$Z9069100 & data$Z9069100 <= 3999.0] <- 3500.0
data$Z9069100[4000.0 <= data$Z9069100 & data$Z9069100 <= 4499.0] <- 4000.0
data$Z9069100[4500.0 <= data$Z9069100 & data$Z9069100 <= 4999.0] <- 4500.0
data$Z9069100[5000.0 <= data$Z9069100 & data$Z9069100 <= 9.9999999E7] <- 5000.0
data$Z9069100 <- factor(data$Z9069100,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069200[1.0 <= data$Z9069200 & data$Z9069200 <= 499.0] <- 1.0
data$Z9069200[500.0 <= data$Z9069200 & data$Z9069200 <= 999.0] <- 500.0
data$Z9069200[1000.0 <= data$Z9069200 & data$Z9069200 <= 1499.0] <- 1000.0
data$Z9069200[1500.0 <= data$Z9069200 & data$Z9069200 <= 1999.0] <- 1500.0
data$Z9069200[2000.0 <= data$Z9069200 & data$Z9069200 <= 2499.0] <- 2000.0
data$Z9069200[2500.0 <= data$Z9069200 & data$Z9069200 <= 2999.0] <- 2500.0
data$Z9069200[3000.0 <= data$Z9069200 & data$Z9069200 <= 3499.0] <- 3000.0
data$Z9069200[3500.0 <= data$Z9069200 & data$Z9069200 <= 3999.0] <- 3500.0
data$Z9069200[4000.0 <= data$Z9069200 & data$Z9069200 <= 4499.0] <- 4000.0
data$Z9069200[4500.0 <= data$Z9069200 & data$Z9069200 <= 4999.0] <- 4500.0
data$Z9069200[5000.0 <= data$Z9069200 & data$Z9069200 <= 9.9999999E7] <- 5000.0
data$Z9069200 <- factor(data$Z9069200,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069300[1.0 <= data$Z9069300 & data$Z9069300 <= 499.0] <- 1.0
data$Z9069300[500.0 <= data$Z9069300 & data$Z9069300 <= 999.0] <- 500.0
data$Z9069300[1000.0 <= data$Z9069300 & data$Z9069300 <= 1499.0] <- 1000.0
data$Z9069300[1500.0 <= data$Z9069300 & data$Z9069300 <= 1999.0] <- 1500.0
data$Z9069300[2000.0 <= data$Z9069300 & data$Z9069300 <= 2499.0] <- 2000.0
data$Z9069300[2500.0 <= data$Z9069300 & data$Z9069300 <= 2999.0] <- 2500.0
data$Z9069300[3000.0 <= data$Z9069300 & data$Z9069300 <= 3499.0] <- 3000.0
data$Z9069300[3500.0 <= data$Z9069300 & data$Z9069300 <= 3999.0] <- 3500.0
data$Z9069300[4000.0 <= data$Z9069300 & data$Z9069300 <= 4499.0] <- 4000.0
data$Z9069300[4500.0 <= data$Z9069300 & data$Z9069300 <= 4999.0] <- 4500.0
data$Z9069300[5000.0 <= data$Z9069300 & data$Z9069300 <= 9.9999999E7] <- 5000.0
data$Z9069300 <- factor(data$Z9069300,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069400[1.0 <= data$Z9069400 & data$Z9069400 <= 499.0] <- 1.0
data$Z9069400[500.0 <= data$Z9069400 & data$Z9069400 <= 999.0] <- 500.0
data$Z9069400[1000.0 <= data$Z9069400 & data$Z9069400 <= 1499.0] <- 1000.0
data$Z9069400[1500.0 <= data$Z9069400 & data$Z9069400 <= 1999.0] <- 1500.0
data$Z9069400[2000.0 <= data$Z9069400 & data$Z9069400 <= 2499.0] <- 2000.0
data$Z9069400[2500.0 <= data$Z9069400 & data$Z9069400 <= 2999.0] <- 2500.0
data$Z9069400[3000.0 <= data$Z9069400 & data$Z9069400 <= 3499.0] <- 3000.0
data$Z9069400[3500.0 <= data$Z9069400 & data$Z9069400 <= 3999.0] <- 3500.0
data$Z9069400[4000.0 <= data$Z9069400 & data$Z9069400 <= 4499.0] <- 4000.0
data$Z9069400[4500.0 <= data$Z9069400 & data$Z9069400 <= 4999.0] <- 4500.0
data$Z9069400[5000.0 <= data$Z9069400 & data$Z9069400 <= 9.9999999E7] <- 5000.0
data$Z9069400 <- factor(data$Z9069400,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069500[1.0 <= data$Z9069500 & data$Z9069500 <= 499.0] <- 1.0
data$Z9069500[500.0 <= data$Z9069500 & data$Z9069500 <= 999.0] <- 500.0
data$Z9069500[1000.0 <= data$Z9069500 & data$Z9069500 <= 1499.0] <- 1000.0
data$Z9069500[1500.0 <= data$Z9069500 & data$Z9069500 <= 1999.0] <- 1500.0
data$Z9069500[2000.0 <= data$Z9069500 & data$Z9069500 <= 2499.0] <- 2000.0
data$Z9069500[2500.0 <= data$Z9069500 & data$Z9069500 <= 2999.0] <- 2500.0
data$Z9069500[3000.0 <= data$Z9069500 & data$Z9069500 <= 3499.0] <- 3000.0
data$Z9069500[3500.0 <= data$Z9069500 & data$Z9069500 <= 3999.0] <- 3500.0
data$Z9069500[4000.0 <= data$Z9069500 & data$Z9069500 <= 4499.0] <- 4000.0
data$Z9069500[4500.0 <= data$Z9069500 & data$Z9069500 <= 4999.0] <- 4500.0
data$Z9069500[5000.0 <= data$Z9069500 & data$Z9069500 <= 9.9999999E7] <- 5000.0
data$Z9069500 <- factor(data$Z9069500,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069600[1.0 <= data$Z9069600 & data$Z9069600 <= 499.0] <- 1.0
data$Z9069600[500.0 <= data$Z9069600 & data$Z9069600 <= 999.0] <- 500.0
data$Z9069600[1000.0 <= data$Z9069600 & data$Z9069600 <= 1499.0] <- 1000.0
data$Z9069600[1500.0 <= data$Z9069600 & data$Z9069600 <= 1999.0] <- 1500.0
data$Z9069600[2000.0 <= data$Z9069600 & data$Z9069600 <= 2499.0] <- 2000.0
data$Z9069600[2500.0 <= data$Z9069600 & data$Z9069600 <= 2999.0] <- 2500.0
data$Z9069600[3000.0 <= data$Z9069600 & data$Z9069600 <= 3499.0] <- 3000.0
data$Z9069600[3500.0 <= data$Z9069600 & data$Z9069600 <= 3999.0] <- 3500.0
data$Z9069600[4000.0 <= data$Z9069600 & data$Z9069600 <= 4499.0] <- 4000.0
data$Z9069600[4500.0 <= data$Z9069600 & data$Z9069600 <= 4999.0] <- 4500.0
data$Z9069600[5000.0 <= data$Z9069600 & data$Z9069600 <= 9.9999999E7] <- 5000.0
data$Z9069600 <- factor(data$Z9069600,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069700[1.0 <= data$Z9069700 & data$Z9069700 <= 499.0] <- 1.0
data$Z9069700[500.0 <= data$Z9069700 & data$Z9069700 <= 999.0] <- 500.0
data$Z9069700[1000.0 <= data$Z9069700 & data$Z9069700 <= 1499.0] <- 1000.0
data$Z9069700[1500.0 <= data$Z9069700 & data$Z9069700 <= 1999.0] <- 1500.0
data$Z9069700[2000.0 <= data$Z9069700 & data$Z9069700 <= 2499.0] <- 2000.0
data$Z9069700[2500.0 <= data$Z9069700 & data$Z9069700 <= 2999.0] <- 2500.0
data$Z9069700[3000.0 <= data$Z9069700 & data$Z9069700 <= 3499.0] <- 3000.0
data$Z9069700[3500.0 <= data$Z9069700 & data$Z9069700 <= 3999.0] <- 3500.0
data$Z9069700[4000.0 <= data$Z9069700 & data$Z9069700 <= 4499.0] <- 4000.0
data$Z9069700[4500.0 <= data$Z9069700 & data$Z9069700 <= 4999.0] <- 4500.0
data$Z9069700[5000.0 <= data$Z9069700 & data$Z9069700 <= 9.9999999E7] <- 5000.0
data$Z9069700 <- factor(data$Z9069700,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069701[1.0 <= data$Z9069701 & data$Z9069701 <= 499.0] <- 1.0
data$Z9069701[500.0 <= data$Z9069701 & data$Z9069701 <= 999.0] <- 500.0
data$Z9069701[1000.0 <= data$Z9069701 & data$Z9069701 <= 1499.0] <- 1000.0
data$Z9069701[1500.0 <= data$Z9069701 & data$Z9069701 <= 1999.0] <- 1500.0
data$Z9069701[2000.0 <= data$Z9069701 & data$Z9069701 <= 2499.0] <- 2000.0
data$Z9069701[2500.0 <= data$Z9069701 & data$Z9069701 <= 2999.0] <- 2500.0
data$Z9069701[3000.0 <= data$Z9069701 & data$Z9069701 <= 3499.0] <- 3000.0
data$Z9069701[3500.0 <= data$Z9069701 & data$Z9069701 <= 3999.0] <- 3500.0
data$Z9069701[4000.0 <= data$Z9069701 & data$Z9069701 <= 4499.0] <- 4000.0
data$Z9069701[4500.0 <= data$Z9069701 & data$Z9069701 <= 4999.0] <- 4500.0
data$Z9069701[5000.0 <= data$Z9069701 & data$Z9069701 <= 9.9999999E7] <- 5000.0
data$Z9069701 <- factor(data$Z9069701,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069702[1.0 <= data$Z9069702 & data$Z9069702 <= 499.0] <- 1.0
data$Z9069702[500.0 <= data$Z9069702 & data$Z9069702 <= 999.0] <- 500.0
data$Z9069702[1000.0 <= data$Z9069702 & data$Z9069702 <= 1499.0] <- 1000.0
data$Z9069702[1500.0 <= data$Z9069702 & data$Z9069702 <= 1999.0] <- 1500.0
data$Z9069702[2000.0 <= data$Z9069702 & data$Z9069702 <= 2499.0] <- 2000.0
data$Z9069702[2500.0 <= data$Z9069702 & data$Z9069702 <= 2999.0] <- 2500.0
data$Z9069702[3000.0 <= data$Z9069702 & data$Z9069702 <= 3499.0] <- 3000.0
data$Z9069702[3500.0 <= data$Z9069702 & data$Z9069702 <= 3999.0] <- 3500.0
data$Z9069702[4000.0 <= data$Z9069702 & data$Z9069702 <= 4499.0] <- 4000.0
data$Z9069702[4500.0 <= data$Z9069702 & data$Z9069702 <= 4999.0] <- 4500.0
data$Z9069702[5000.0 <= data$Z9069702 & data$Z9069702 <= 9.9999999E7] <- 5000.0
data$Z9069702 <- factor(data$Z9069702,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069703[1.0 <= data$Z9069703 & data$Z9069703 <= 499.0] <- 1.0
data$Z9069703[500.0 <= data$Z9069703 & data$Z9069703 <= 999.0] <- 500.0
data$Z9069703[1000.0 <= data$Z9069703 & data$Z9069703 <= 1499.0] <- 1000.0
data$Z9069703[1500.0 <= data$Z9069703 & data$Z9069703 <= 1999.0] <- 1500.0
data$Z9069703[2000.0 <= data$Z9069703 & data$Z9069703 <= 2499.0] <- 2000.0
data$Z9069703[2500.0 <= data$Z9069703 & data$Z9069703 <= 2999.0] <- 2500.0
data$Z9069703[3000.0 <= data$Z9069703 & data$Z9069703 <= 3499.0] <- 3000.0
data$Z9069703[3500.0 <= data$Z9069703 & data$Z9069703 <= 3999.0] <- 3500.0
data$Z9069703[4000.0 <= data$Z9069703 & data$Z9069703 <= 4499.0] <- 4000.0
data$Z9069703[4500.0 <= data$Z9069703 & data$Z9069703 <= 4999.0] <- 4500.0
data$Z9069703[5000.0 <= data$Z9069703 & data$Z9069703 <= 9.9999999E7] <- 5000.0
data$Z9069703 <- factor(data$Z9069703,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069704[1.0 <= data$Z9069704 & data$Z9069704 <= 499.0] <- 1.0
data$Z9069704[500.0 <= data$Z9069704 & data$Z9069704 <= 999.0] <- 500.0
data$Z9069704[1000.0 <= data$Z9069704 & data$Z9069704 <= 1499.0] <- 1000.0
data$Z9069704[1500.0 <= data$Z9069704 & data$Z9069704 <= 1999.0] <- 1500.0
data$Z9069704[2000.0 <= data$Z9069704 & data$Z9069704 <= 2499.0] <- 2000.0
data$Z9069704[2500.0 <= data$Z9069704 & data$Z9069704 <= 2999.0] <- 2500.0
data$Z9069704[3000.0 <= data$Z9069704 & data$Z9069704 <= 3499.0] <- 3000.0
data$Z9069704[3500.0 <= data$Z9069704 & data$Z9069704 <= 3999.0] <- 3500.0
data$Z9069704[4000.0 <= data$Z9069704 & data$Z9069704 <= 4499.0] <- 4000.0
data$Z9069704[4500.0 <= data$Z9069704 & data$Z9069704 <= 4999.0] <- 4500.0
data$Z9069704[5000.0 <= data$Z9069704 & data$Z9069704 <= 9.9999999E7] <- 5000.0
data$Z9069704 <- factor(data$Z9069704,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069705[1.0 <= data$Z9069705 & data$Z9069705 <= 499.0] <- 1.0
data$Z9069705[500.0 <= data$Z9069705 & data$Z9069705 <= 999.0] <- 500.0
data$Z9069705[1000.0 <= data$Z9069705 & data$Z9069705 <= 1499.0] <- 1000.0
data$Z9069705[1500.0 <= data$Z9069705 & data$Z9069705 <= 1999.0] <- 1500.0
data$Z9069705[2000.0 <= data$Z9069705 & data$Z9069705 <= 2499.0] <- 2000.0
data$Z9069705[2500.0 <= data$Z9069705 & data$Z9069705 <= 2999.0] <- 2500.0
data$Z9069705[3000.0 <= data$Z9069705 & data$Z9069705 <= 3499.0] <- 3000.0
data$Z9069705[3500.0 <= data$Z9069705 & data$Z9069705 <= 3999.0] <- 3500.0
data$Z9069705[4000.0 <= data$Z9069705 & data$Z9069705 <= 4499.0] <- 4000.0
data$Z9069705[4500.0 <= data$Z9069705 & data$Z9069705 <= 4999.0] <- 4500.0
data$Z9069705[5000.0 <= data$Z9069705 & data$Z9069705 <= 9.9999999E7] <- 5000.0
data$Z9069705 <- factor(data$Z9069705,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069706[1.0 <= data$Z9069706 & data$Z9069706 <= 499.0] <- 1.0
data$Z9069706[500.0 <= data$Z9069706 & data$Z9069706 <= 999.0] <- 500.0
data$Z9069706[1000.0 <= data$Z9069706 & data$Z9069706 <= 1499.0] <- 1000.0
data$Z9069706[1500.0 <= data$Z9069706 & data$Z9069706 <= 1999.0] <- 1500.0
data$Z9069706[2000.0 <= data$Z9069706 & data$Z9069706 <= 2499.0] <- 2000.0
data$Z9069706[2500.0 <= data$Z9069706 & data$Z9069706 <= 2999.0] <- 2500.0
data$Z9069706[3000.0 <= data$Z9069706 & data$Z9069706 <= 3499.0] <- 3000.0
data$Z9069706[3500.0 <= data$Z9069706 & data$Z9069706 <= 3999.0] <- 3500.0
data$Z9069706[4000.0 <= data$Z9069706 & data$Z9069706 <= 4499.0] <- 4000.0
data$Z9069706[4500.0 <= data$Z9069706 & data$Z9069706 <= 4999.0] <- 4500.0
data$Z9069706[5000.0 <= data$Z9069706 & data$Z9069706 <= 9.9999999E7] <- 5000.0
data$Z9069706 <- factor(data$Z9069706,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069707[1.0 <= data$Z9069707 & data$Z9069707 <= 499.0] <- 1.0
data$Z9069707[500.0 <= data$Z9069707 & data$Z9069707 <= 999.0] <- 500.0
data$Z9069707[1000.0 <= data$Z9069707 & data$Z9069707 <= 1499.0] <- 1000.0
data$Z9069707[1500.0 <= data$Z9069707 & data$Z9069707 <= 1999.0] <- 1500.0
data$Z9069707[2000.0 <= data$Z9069707 & data$Z9069707 <= 2499.0] <- 2000.0
data$Z9069707[2500.0 <= data$Z9069707 & data$Z9069707 <= 2999.0] <- 2500.0
data$Z9069707[3000.0 <= data$Z9069707 & data$Z9069707 <= 3499.0] <- 3000.0
data$Z9069707[3500.0 <= data$Z9069707 & data$Z9069707 <= 3999.0] <- 3500.0
data$Z9069707[4000.0 <= data$Z9069707 & data$Z9069707 <= 4499.0] <- 4000.0
data$Z9069707[4500.0 <= data$Z9069707 & data$Z9069707 <= 4999.0] <- 4500.0
data$Z9069707[5000.0 <= data$Z9069707 & data$Z9069707 <= 9.9999999E7] <- 5000.0
data$Z9069707 <- factor(data$Z9069707,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069708[1.0 <= data$Z9069708 & data$Z9069708 <= 499.0] <- 1.0
data$Z9069708[500.0 <= data$Z9069708 & data$Z9069708 <= 999.0] <- 500.0
data$Z9069708[1000.0 <= data$Z9069708 & data$Z9069708 <= 1499.0] <- 1000.0
data$Z9069708[1500.0 <= data$Z9069708 & data$Z9069708 <= 1999.0] <- 1500.0
data$Z9069708[2000.0 <= data$Z9069708 & data$Z9069708 <= 2499.0] <- 2000.0
data$Z9069708[2500.0 <= data$Z9069708 & data$Z9069708 <= 2999.0] <- 2500.0
data$Z9069708[3000.0 <= data$Z9069708 & data$Z9069708 <= 3499.0] <- 3000.0
data$Z9069708[3500.0 <= data$Z9069708 & data$Z9069708 <= 3999.0] <- 3500.0
data$Z9069708[4000.0 <= data$Z9069708 & data$Z9069708 <= 4499.0] <- 4000.0
data$Z9069708[4500.0 <= data$Z9069708 & data$Z9069708 <= 4999.0] <- 4500.0
data$Z9069708[5000.0 <= data$Z9069708 & data$Z9069708 <= 9.9999999E7] <- 5000.0
data$Z9069708 <- factor(data$Z9069708,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069709[1.0 <= data$Z9069709 & data$Z9069709 <= 499.0] <- 1.0
data$Z9069709[500.0 <= data$Z9069709 & data$Z9069709 <= 999.0] <- 500.0
data$Z9069709[1000.0 <= data$Z9069709 & data$Z9069709 <= 1499.0] <- 1000.0
data$Z9069709[1500.0 <= data$Z9069709 & data$Z9069709 <= 1999.0] <- 1500.0
data$Z9069709[2000.0 <= data$Z9069709 & data$Z9069709 <= 2499.0] <- 2000.0
data$Z9069709[2500.0 <= data$Z9069709 & data$Z9069709 <= 2999.0] <- 2500.0
data$Z9069709[3000.0 <= data$Z9069709 & data$Z9069709 <= 3499.0] <- 3000.0
data$Z9069709[3500.0 <= data$Z9069709 & data$Z9069709 <= 3999.0] <- 3500.0
data$Z9069709[4000.0 <= data$Z9069709 & data$Z9069709 <= 4499.0] <- 4000.0
data$Z9069709[4500.0 <= data$Z9069709 & data$Z9069709 <= 4999.0] <- 4500.0
data$Z9069709[5000.0 <= data$Z9069709 & data$Z9069709 <= 9.9999999E7] <- 5000.0
data$Z9069709 <- factor(data$Z9069709,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069710[1.0 <= data$Z9069710 & data$Z9069710 <= 499.0] <- 1.0
data$Z9069710[500.0 <= data$Z9069710 & data$Z9069710 <= 999.0] <- 500.0
data$Z9069710[1000.0 <= data$Z9069710 & data$Z9069710 <= 1499.0] <- 1000.0
data$Z9069710[1500.0 <= data$Z9069710 & data$Z9069710 <= 1999.0] <- 1500.0
data$Z9069710[2000.0 <= data$Z9069710 & data$Z9069710 <= 2499.0] <- 2000.0
data$Z9069710[2500.0 <= data$Z9069710 & data$Z9069710 <= 2999.0] <- 2500.0
data$Z9069710[3000.0 <= data$Z9069710 & data$Z9069710 <= 3499.0] <- 3000.0
data$Z9069710[3500.0 <= data$Z9069710 & data$Z9069710 <= 3999.0] <- 3500.0
data$Z9069710[4000.0 <= data$Z9069710 & data$Z9069710 <= 4499.0] <- 4000.0
data$Z9069710[4500.0 <= data$Z9069710 & data$Z9069710 <= 4999.0] <- 4500.0
data$Z9069710[5000.0 <= data$Z9069710 & data$Z9069710 <= 9.9999999E7] <- 5000.0
data$Z9069710 <- factor(data$Z9069710,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069711[1.0 <= data$Z9069711 & data$Z9069711 <= 499.0] <- 1.0
data$Z9069711[500.0 <= data$Z9069711 & data$Z9069711 <= 999.0] <- 500.0
data$Z9069711[1000.0 <= data$Z9069711 & data$Z9069711 <= 1499.0] <- 1000.0
data$Z9069711[1500.0 <= data$Z9069711 & data$Z9069711 <= 1999.0] <- 1500.0
data$Z9069711[2000.0 <= data$Z9069711 & data$Z9069711 <= 2499.0] <- 2000.0
data$Z9069711[2500.0 <= data$Z9069711 & data$Z9069711 <= 2999.0] <- 2500.0
data$Z9069711[3000.0 <= data$Z9069711 & data$Z9069711 <= 3499.0] <- 3000.0
data$Z9069711[3500.0 <= data$Z9069711 & data$Z9069711 <= 3999.0] <- 3500.0
data$Z9069711[4000.0 <= data$Z9069711 & data$Z9069711 <= 4499.0] <- 4000.0
data$Z9069711[4500.0 <= data$Z9069711 & data$Z9069711 <= 4999.0] <- 4500.0
data$Z9069711[5000.0 <= data$Z9069711 & data$Z9069711 <= 9.9999999E7] <- 5000.0
data$Z9069711 <- factor(data$Z9069711,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069712[1.0 <= data$Z9069712 & data$Z9069712 <= 499.0] <- 1.0
data$Z9069712[500.0 <= data$Z9069712 & data$Z9069712 <= 999.0] <- 500.0
data$Z9069712[1000.0 <= data$Z9069712 & data$Z9069712 <= 1499.0] <- 1000.0
data$Z9069712[1500.0 <= data$Z9069712 & data$Z9069712 <= 1999.0] <- 1500.0
data$Z9069712[2000.0 <= data$Z9069712 & data$Z9069712 <= 2499.0] <- 2000.0
data$Z9069712[2500.0 <= data$Z9069712 & data$Z9069712 <= 2999.0] <- 2500.0
data$Z9069712[3000.0 <= data$Z9069712 & data$Z9069712 <= 3499.0] <- 3000.0
data$Z9069712[3500.0 <= data$Z9069712 & data$Z9069712 <= 3999.0] <- 3500.0
data$Z9069712[4000.0 <= data$Z9069712 & data$Z9069712 <= 4499.0] <- 4000.0
data$Z9069712[4500.0 <= data$Z9069712 & data$Z9069712 <= 4999.0] <- 4500.0
data$Z9069712[5000.0 <= data$Z9069712 & data$Z9069712 <= 9.9999999E7] <- 5000.0
data$Z9069712 <- factor(data$Z9069712,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069713[1.0 <= data$Z9069713 & data$Z9069713 <= 499.0] <- 1.0
data$Z9069713[500.0 <= data$Z9069713 & data$Z9069713 <= 999.0] <- 500.0
data$Z9069713[1000.0 <= data$Z9069713 & data$Z9069713 <= 1499.0] <- 1000.0
data$Z9069713[1500.0 <= data$Z9069713 & data$Z9069713 <= 1999.0] <- 1500.0
data$Z9069713[2000.0 <= data$Z9069713 & data$Z9069713 <= 2499.0] <- 2000.0
data$Z9069713[2500.0 <= data$Z9069713 & data$Z9069713 <= 2999.0] <- 2500.0
data$Z9069713[3000.0 <= data$Z9069713 & data$Z9069713 <= 3499.0] <- 3000.0
data$Z9069713[3500.0 <= data$Z9069713 & data$Z9069713 <= 3999.0] <- 3500.0
data$Z9069713[4000.0 <= data$Z9069713 & data$Z9069713 <= 4499.0] <- 4000.0
data$Z9069713[4500.0 <= data$Z9069713 & data$Z9069713 <= 4999.0] <- 4500.0
data$Z9069713[5000.0 <= data$Z9069713 & data$Z9069713 <= 9.9999999E7] <- 5000.0
data$Z9069713 <- factor(data$Z9069713,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069800[1.0 <= data$Z9069800 & data$Z9069800 <= 499.0] <- 1.0
data$Z9069800[500.0 <= data$Z9069800 & data$Z9069800 <= 999.0] <- 500.0
data$Z9069800[1000.0 <= data$Z9069800 & data$Z9069800 <= 1499.0] <- 1000.0
data$Z9069800[1500.0 <= data$Z9069800 & data$Z9069800 <= 1999.0] <- 1500.0
data$Z9069800[2000.0 <= data$Z9069800 & data$Z9069800 <= 2499.0] <- 2000.0
data$Z9069800[2500.0 <= data$Z9069800 & data$Z9069800 <= 2999.0] <- 2500.0
data$Z9069800[3000.0 <= data$Z9069800 & data$Z9069800 <= 3499.0] <- 3000.0
data$Z9069800[3500.0 <= data$Z9069800 & data$Z9069800 <= 3999.0] <- 3500.0
data$Z9069800[4000.0 <= data$Z9069800 & data$Z9069800 <= 4499.0] <- 4000.0
data$Z9069800[4500.0 <= data$Z9069800 & data$Z9069800 <= 4999.0] <- 4500.0
data$Z9069800[5000.0 <= data$Z9069800 & data$Z9069800 <= 9.9999999E7] <- 5000.0
data$Z9069800 <- factor(data$Z9069800,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9069900[1.0 <= data$Z9069900 & data$Z9069900 <= 499.0] <- 1.0
data$Z9069900[500.0 <= data$Z9069900 & data$Z9069900 <= 999.0] <- 500.0
data$Z9069900[1000.0 <= data$Z9069900 & data$Z9069900 <= 1499.0] <- 1000.0
data$Z9069900[1500.0 <= data$Z9069900 & data$Z9069900 <= 1999.0] <- 1500.0
data$Z9069900[2000.0 <= data$Z9069900 & data$Z9069900 <= 2499.0] <- 2000.0
data$Z9069900[2500.0 <= data$Z9069900 & data$Z9069900 <= 2999.0] <- 2500.0
data$Z9069900[3000.0 <= data$Z9069900 & data$Z9069900 <= 3499.0] <- 3000.0
data$Z9069900[3500.0 <= data$Z9069900 & data$Z9069900 <= 3999.0] <- 3500.0
data$Z9069900[4000.0 <= data$Z9069900 & data$Z9069900 <= 4499.0] <- 4000.0
data$Z9069900[4500.0 <= data$Z9069900 & data$Z9069900 <= 4999.0] <- 4500.0
data$Z9069900[5000.0 <= data$Z9069900 & data$Z9069900 <= 9.9999999E7] <- 5000.0
data$Z9069900 <- factor(data$Z9069900,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070000[1.0 <= data$Z9070000 & data$Z9070000 <= 499.0] <- 1.0
data$Z9070000[500.0 <= data$Z9070000 & data$Z9070000 <= 999.0] <- 500.0
data$Z9070000[1000.0 <= data$Z9070000 & data$Z9070000 <= 1499.0] <- 1000.0
data$Z9070000[1500.0 <= data$Z9070000 & data$Z9070000 <= 1999.0] <- 1500.0
data$Z9070000[2000.0 <= data$Z9070000 & data$Z9070000 <= 2499.0] <- 2000.0
data$Z9070000[2500.0 <= data$Z9070000 & data$Z9070000 <= 2999.0] <- 2500.0
data$Z9070000[3000.0 <= data$Z9070000 & data$Z9070000 <= 3499.0] <- 3000.0
data$Z9070000[3500.0 <= data$Z9070000 & data$Z9070000 <= 3999.0] <- 3500.0
data$Z9070000[4000.0 <= data$Z9070000 & data$Z9070000 <= 4499.0] <- 4000.0
data$Z9070000[4500.0 <= data$Z9070000 & data$Z9070000 <= 4999.0] <- 4500.0
data$Z9070000[5000.0 <= data$Z9070000 & data$Z9070000 <= 9.9999999E7] <- 5000.0
data$Z9070000 <- factor(data$Z9070000,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070100[1.0 <= data$Z9070100 & data$Z9070100 <= 499.0] <- 1.0
data$Z9070100[500.0 <= data$Z9070100 & data$Z9070100 <= 999.0] <- 500.0
data$Z9070100[1000.0 <= data$Z9070100 & data$Z9070100 <= 1499.0] <- 1000.0
data$Z9070100[1500.0 <= data$Z9070100 & data$Z9070100 <= 1999.0] <- 1500.0
data$Z9070100[2000.0 <= data$Z9070100 & data$Z9070100 <= 2499.0] <- 2000.0
data$Z9070100[2500.0 <= data$Z9070100 & data$Z9070100 <= 2999.0] <- 2500.0
data$Z9070100[3000.0 <= data$Z9070100 & data$Z9070100 <= 3499.0] <- 3000.0
data$Z9070100[3500.0 <= data$Z9070100 & data$Z9070100 <= 3999.0] <- 3500.0
data$Z9070100[4000.0 <= data$Z9070100 & data$Z9070100 <= 4499.0] <- 4000.0
data$Z9070100[4500.0 <= data$Z9070100 & data$Z9070100 <= 4999.0] <- 4500.0
data$Z9070100[5000.0 <= data$Z9070100 & data$Z9070100 <= 9.9999999E7] <- 5000.0
data$Z9070100 <- factor(data$Z9070100,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070200[1.0 <= data$Z9070200 & data$Z9070200 <= 499.0] <- 1.0
data$Z9070200[500.0 <= data$Z9070200 & data$Z9070200 <= 999.0] <- 500.0
data$Z9070200[1000.0 <= data$Z9070200 & data$Z9070200 <= 1499.0] <- 1000.0
data$Z9070200[1500.0 <= data$Z9070200 & data$Z9070200 <= 1999.0] <- 1500.0
data$Z9070200[2000.0 <= data$Z9070200 & data$Z9070200 <= 2499.0] <- 2000.0
data$Z9070200[2500.0 <= data$Z9070200 & data$Z9070200 <= 2999.0] <- 2500.0
data$Z9070200[3000.0 <= data$Z9070200 & data$Z9070200 <= 3499.0] <- 3000.0
data$Z9070200[3500.0 <= data$Z9070200 & data$Z9070200 <= 3999.0] <- 3500.0
data$Z9070200[4000.0 <= data$Z9070200 & data$Z9070200 <= 4499.0] <- 4000.0
data$Z9070200[4500.0 <= data$Z9070200 & data$Z9070200 <= 4999.0] <- 4500.0
data$Z9070200[5000.0 <= data$Z9070200 & data$Z9070200 <= 9.9999999E7] <- 5000.0
data$Z9070200 <- factor(data$Z9070200,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070300[1.0 <= data$Z9070300 & data$Z9070300 <= 499.0] <- 1.0
data$Z9070300[500.0 <= data$Z9070300 & data$Z9070300 <= 999.0] <- 500.0
data$Z9070300[1000.0 <= data$Z9070300 & data$Z9070300 <= 1499.0] <- 1000.0
data$Z9070300[1500.0 <= data$Z9070300 & data$Z9070300 <= 1999.0] <- 1500.0
data$Z9070300[2000.0 <= data$Z9070300 & data$Z9070300 <= 2499.0] <- 2000.0
data$Z9070300[2500.0 <= data$Z9070300 & data$Z9070300 <= 2999.0] <- 2500.0
data$Z9070300[3000.0 <= data$Z9070300 & data$Z9070300 <= 3499.0] <- 3000.0
data$Z9070300[3500.0 <= data$Z9070300 & data$Z9070300 <= 3999.0] <- 3500.0
data$Z9070300[4000.0 <= data$Z9070300 & data$Z9070300 <= 4499.0] <- 4000.0
data$Z9070300[4500.0 <= data$Z9070300 & data$Z9070300 <= 4999.0] <- 4500.0
data$Z9070300[5000.0 <= data$Z9070300 & data$Z9070300 <= 9.9999999E7] <- 5000.0
data$Z9070300 <- factor(data$Z9070300,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070400[1.0 <= data$Z9070400 & data$Z9070400 <= 499.0] <- 1.0
data$Z9070400[500.0 <= data$Z9070400 & data$Z9070400 <= 999.0] <- 500.0
data$Z9070400[1000.0 <= data$Z9070400 & data$Z9070400 <= 1499.0] <- 1000.0
data$Z9070400[1500.0 <= data$Z9070400 & data$Z9070400 <= 1999.0] <- 1500.0
data$Z9070400[2000.0 <= data$Z9070400 & data$Z9070400 <= 2499.0] <- 2000.0
data$Z9070400[2500.0 <= data$Z9070400 & data$Z9070400 <= 2999.0] <- 2500.0
data$Z9070400[3000.0 <= data$Z9070400 & data$Z9070400 <= 3499.0] <- 3000.0
data$Z9070400[3500.0 <= data$Z9070400 & data$Z9070400 <= 3999.0] <- 3500.0
data$Z9070400[4000.0 <= data$Z9070400 & data$Z9070400 <= 4499.0] <- 4000.0
data$Z9070400[4500.0 <= data$Z9070400 & data$Z9070400 <= 4999.0] <- 4500.0
data$Z9070400[5000.0 <= data$Z9070400 & data$Z9070400 <= 9.9999999E7] <- 5000.0
data$Z9070400 <- factor(data$Z9070400,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070500[1.0 <= data$Z9070500 & data$Z9070500 <= 499.0] <- 1.0
data$Z9070500[500.0 <= data$Z9070500 & data$Z9070500 <= 999.0] <- 500.0
data$Z9070500[1000.0 <= data$Z9070500 & data$Z9070500 <= 1499.0] <- 1000.0
data$Z9070500[1500.0 <= data$Z9070500 & data$Z9070500 <= 1999.0] <- 1500.0
data$Z9070500[2000.0 <= data$Z9070500 & data$Z9070500 <= 2499.0] <- 2000.0
data$Z9070500[2500.0 <= data$Z9070500 & data$Z9070500 <= 2999.0] <- 2500.0
data$Z9070500[3000.0 <= data$Z9070500 & data$Z9070500 <= 3499.0] <- 3000.0
data$Z9070500[3500.0 <= data$Z9070500 & data$Z9070500 <= 3999.0] <- 3500.0
data$Z9070500[4000.0 <= data$Z9070500 & data$Z9070500 <= 4499.0] <- 4000.0
data$Z9070500[4500.0 <= data$Z9070500 & data$Z9070500 <= 4999.0] <- 4500.0
data$Z9070500[5000.0 <= data$Z9070500 & data$Z9070500 <= 9.9999999E7] <- 5000.0
data$Z9070500 <- factor(data$Z9070500,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070600[1.0 <= data$Z9070600 & data$Z9070600 <= 499.0] <- 1.0
data$Z9070600[500.0 <= data$Z9070600 & data$Z9070600 <= 999.0] <- 500.0
data$Z9070600[1000.0 <= data$Z9070600 & data$Z9070600 <= 1499.0] <- 1000.0
data$Z9070600[1500.0 <= data$Z9070600 & data$Z9070600 <= 1999.0] <- 1500.0
data$Z9070600[2000.0 <= data$Z9070600 & data$Z9070600 <= 2499.0] <- 2000.0
data$Z9070600[2500.0 <= data$Z9070600 & data$Z9070600 <= 2999.0] <- 2500.0
data$Z9070600[3000.0 <= data$Z9070600 & data$Z9070600 <= 3499.0] <- 3000.0
data$Z9070600[3500.0 <= data$Z9070600 & data$Z9070600 <= 3999.0] <- 3500.0
data$Z9070600[4000.0 <= data$Z9070600 & data$Z9070600 <= 4499.0] <- 4000.0
data$Z9070600[4500.0 <= data$Z9070600 & data$Z9070600 <= 4999.0] <- 4500.0
data$Z9070600[5000.0 <= data$Z9070600 & data$Z9070600 <= 9.9999999E7] <- 5000.0
data$Z9070600 <- factor(data$Z9070600,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070700[1.0 <= data$Z9070700 & data$Z9070700 <= 499.0] <- 1.0
data$Z9070700[500.0 <= data$Z9070700 & data$Z9070700 <= 999.0] <- 500.0
data$Z9070700[1000.0 <= data$Z9070700 & data$Z9070700 <= 1499.0] <- 1000.0
data$Z9070700[1500.0 <= data$Z9070700 & data$Z9070700 <= 1999.0] <- 1500.0
data$Z9070700[2000.0 <= data$Z9070700 & data$Z9070700 <= 2499.0] <- 2000.0
data$Z9070700[2500.0 <= data$Z9070700 & data$Z9070700 <= 2999.0] <- 2500.0
data$Z9070700[3000.0 <= data$Z9070700 & data$Z9070700 <= 3499.0] <- 3000.0
data$Z9070700[3500.0 <= data$Z9070700 & data$Z9070700 <= 3999.0] <- 3500.0
data$Z9070700[4000.0 <= data$Z9070700 & data$Z9070700 <= 4499.0] <- 4000.0
data$Z9070700[4500.0 <= data$Z9070700 & data$Z9070700 <= 4999.0] <- 4500.0
data$Z9070700[5000.0 <= data$Z9070700 & data$Z9070700 <= 9.9999999E7] <- 5000.0
data$Z9070700 <- factor(data$Z9070700,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070800[1.0 <= data$Z9070800 & data$Z9070800 <= 499.0] <- 1.0
data$Z9070800[500.0 <= data$Z9070800 & data$Z9070800 <= 999.0] <- 500.0
data$Z9070800[1000.0 <= data$Z9070800 & data$Z9070800 <= 1499.0] <- 1000.0
data$Z9070800[1500.0 <= data$Z9070800 & data$Z9070800 <= 1999.0] <- 1500.0
data$Z9070800[2000.0 <= data$Z9070800 & data$Z9070800 <= 2499.0] <- 2000.0
data$Z9070800[2500.0 <= data$Z9070800 & data$Z9070800 <= 2999.0] <- 2500.0
data$Z9070800[3000.0 <= data$Z9070800 & data$Z9070800 <= 3499.0] <- 3000.0
data$Z9070800[3500.0 <= data$Z9070800 & data$Z9070800 <= 3999.0] <- 3500.0
data$Z9070800[4000.0 <= data$Z9070800 & data$Z9070800 <= 4499.0] <- 4000.0
data$Z9070800[4500.0 <= data$Z9070800 & data$Z9070800 <= 4999.0] <- 4500.0
data$Z9070800[5000.0 <= data$Z9070800 & data$Z9070800 <= 9.9999999E7] <- 5000.0
data$Z9070800 <- factor(data$Z9070800,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9070900[1.0 <= data$Z9070900 & data$Z9070900 <= 499.0] <- 1.0
data$Z9070900[500.0 <= data$Z9070900 & data$Z9070900 <= 999.0] <- 500.0
data$Z9070900[1000.0 <= data$Z9070900 & data$Z9070900 <= 1499.0] <- 1000.0
data$Z9070900[1500.0 <= data$Z9070900 & data$Z9070900 <= 1999.0] <- 1500.0
data$Z9070900[2000.0 <= data$Z9070900 & data$Z9070900 <= 2499.0] <- 2000.0
data$Z9070900[2500.0 <= data$Z9070900 & data$Z9070900 <= 2999.0] <- 2500.0
data$Z9070900[3000.0 <= data$Z9070900 & data$Z9070900 <= 3499.0] <- 3000.0
data$Z9070900[3500.0 <= data$Z9070900 & data$Z9070900 <= 3999.0] <- 3500.0
data$Z9070900[4000.0 <= data$Z9070900 & data$Z9070900 <= 4499.0] <- 4000.0
data$Z9070900[4500.0 <= data$Z9070900 & data$Z9070900 <= 4999.0] <- 4500.0
data$Z9070900[5000.0 <= data$Z9070900 & data$Z9070900 <= 9.9999999E7] <- 5000.0
data$Z9070900 <- factor(data$Z9070900,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071000[1.0 <= data$Z9071000 & data$Z9071000 <= 499.0] <- 1.0
data$Z9071000[500.0 <= data$Z9071000 & data$Z9071000 <= 999.0] <- 500.0
data$Z9071000[1000.0 <= data$Z9071000 & data$Z9071000 <= 1499.0] <- 1000.0
data$Z9071000[1500.0 <= data$Z9071000 & data$Z9071000 <= 1999.0] <- 1500.0
data$Z9071000[2000.0 <= data$Z9071000 & data$Z9071000 <= 2499.0] <- 2000.0
data$Z9071000[2500.0 <= data$Z9071000 & data$Z9071000 <= 2999.0] <- 2500.0
data$Z9071000[3000.0 <= data$Z9071000 & data$Z9071000 <= 3499.0] <- 3000.0
data$Z9071000[3500.0 <= data$Z9071000 & data$Z9071000 <= 3999.0] <- 3500.0
data$Z9071000[4000.0 <= data$Z9071000 & data$Z9071000 <= 4499.0] <- 4000.0
data$Z9071000[4500.0 <= data$Z9071000 & data$Z9071000 <= 4999.0] <- 4500.0
data$Z9071000[5000.0 <= data$Z9071000 & data$Z9071000 <= 9.9999999E7] <- 5000.0
data$Z9071000 <- factor(data$Z9071000,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071100[1.0 <= data$Z9071100 & data$Z9071100 <= 499.0] <- 1.0
data$Z9071100[500.0 <= data$Z9071100 & data$Z9071100 <= 999.0] <- 500.0
data$Z9071100[1000.0 <= data$Z9071100 & data$Z9071100 <= 1499.0] <- 1000.0
data$Z9071100[1500.0 <= data$Z9071100 & data$Z9071100 <= 1999.0] <- 1500.0
data$Z9071100[2000.0 <= data$Z9071100 & data$Z9071100 <= 2499.0] <- 2000.0
data$Z9071100[2500.0 <= data$Z9071100 & data$Z9071100 <= 2999.0] <- 2500.0
data$Z9071100[3000.0 <= data$Z9071100 & data$Z9071100 <= 3499.0] <- 3000.0
data$Z9071100[3500.0 <= data$Z9071100 & data$Z9071100 <= 3999.0] <- 3500.0
data$Z9071100[4000.0 <= data$Z9071100 & data$Z9071100 <= 4499.0] <- 4000.0
data$Z9071100[4500.0 <= data$Z9071100 & data$Z9071100 <= 4999.0] <- 4500.0
data$Z9071100[5000.0 <= data$Z9071100 & data$Z9071100 <= 9.9999999E7] <- 5000.0
data$Z9071100 <- factor(data$Z9071100,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071200[1.0 <= data$Z9071200 & data$Z9071200 <= 499.0] <- 1.0
data$Z9071200[500.0 <= data$Z9071200 & data$Z9071200 <= 999.0] <- 500.0
data$Z9071200[1000.0 <= data$Z9071200 & data$Z9071200 <= 1499.0] <- 1000.0
data$Z9071200[1500.0 <= data$Z9071200 & data$Z9071200 <= 1999.0] <- 1500.0
data$Z9071200[2000.0 <= data$Z9071200 & data$Z9071200 <= 2499.0] <- 2000.0
data$Z9071200[2500.0 <= data$Z9071200 & data$Z9071200 <= 2999.0] <- 2500.0
data$Z9071200[3000.0 <= data$Z9071200 & data$Z9071200 <= 3499.0] <- 3000.0
data$Z9071200[3500.0 <= data$Z9071200 & data$Z9071200 <= 3999.0] <- 3500.0
data$Z9071200[4000.0 <= data$Z9071200 & data$Z9071200 <= 4499.0] <- 4000.0
data$Z9071200[4500.0 <= data$Z9071200 & data$Z9071200 <= 4999.0] <- 4500.0
data$Z9071200[5000.0 <= data$Z9071200 & data$Z9071200 <= 9.9999999E7] <- 5000.0
data$Z9071200 <- factor(data$Z9071200,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071300[1.0 <= data$Z9071300 & data$Z9071300 <= 499.0] <- 1.0
data$Z9071300[500.0 <= data$Z9071300 & data$Z9071300 <= 999.0] <- 500.0
data$Z9071300[1000.0 <= data$Z9071300 & data$Z9071300 <= 1499.0] <- 1000.0
data$Z9071300[1500.0 <= data$Z9071300 & data$Z9071300 <= 1999.0] <- 1500.0
data$Z9071300[2000.0 <= data$Z9071300 & data$Z9071300 <= 2499.0] <- 2000.0
data$Z9071300[2500.0 <= data$Z9071300 & data$Z9071300 <= 2999.0] <- 2500.0
data$Z9071300[3000.0 <= data$Z9071300 & data$Z9071300 <= 3499.0] <- 3000.0
data$Z9071300[3500.0 <= data$Z9071300 & data$Z9071300 <= 3999.0] <- 3500.0
data$Z9071300[4000.0 <= data$Z9071300 & data$Z9071300 <= 4499.0] <- 4000.0
data$Z9071300[4500.0 <= data$Z9071300 & data$Z9071300 <= 4999.0] <- 4500.0
data$Z9071300[5000.0 <= data$Z9071300 & data$Z9071300 <= 9.9999999E7] <- 5000.0
data$Z9071300 <- factor(data$Z9071300,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071400[1.0 <= data$Z9071400 & data$Z9071400 <= 499.0] <- 1.0
data$Z9071400[500.0 <= data$Z9071400 & data$Z9071400 <= 999.0] <- 500.0
data$Z9071400[1000.0 <= data$Z9071400 & data$Z9071400 <= 1499.0] <- 1000.0
data$Z9071400[1500.0 <= data$Z9071400 & data$Z9071400 <= 1999.0] <- 1500.0
data$Z9071400[2000.0 <= data$Z9071400 & data$Z9071400 <= 2499.0] <- 2000.0
data$Z9071400[2500.0 <= data$Z9071400 & data$Z9071400 <= 2999.0] <- 2500.0
data$Z9071400[3000.0 <= data$Z9071400 & data$Z9071400 <= 3499.0] <- 3000.0
data$Z9071400[3500.0 <= data$Z9071400 & data$Z9071400 <= 3999.0] <- 3500.0
data$Z9071400[4000.0 <= data$Z9071400 & data$Z9071400 <= 4499.0] <- 4000.0
data$Z9071400[4500.0 <= data$Z9071400 & data$Z9071400 <= 4999.0] <- 4500.0
data$Z9071400[5000.0 <= data$Z9071400 & data$Z9071400 <= 9.9999999E7] <- 5000.0
data$Z9071400 <- factor(data$Z9071400,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071500[1.0 <= data$Z9071500 & data$Z9071500 <= 499.0] <- 1.0
data$Z9071500[500.0 <= data$Z9071500 & data$Z9071500 <= 999.0] <- 500.0
data$Z9071500[1000.0 <= data$Z9071500 & data$Z9071500 <= 1499.0] <- 1000.0
data$Z9071500[1500.0 <= data$Z9071500 & data$Z9071500 <= 1999.0] <- 1500.0
data$Z9071500[2000.0 <= data$Z9071500 & data$Z9071500 <= 2499.0] <- 2000.0
data$Z9071500[2500.0 <= data$Z9071500 & data$Z9071500 <= 2999.0] <- 2500.0
data$Z9071500[3000.0 <= data$Z9071500 & data$Z9071500 <= 3499.0] <- 3000.0
data$Z9071500[3500.0 <= data$Z9071500 & data$Z9071500 <= 3999.0] <- 3500.0
data$Z9071500[4000.0 <= data$Z9071500 & data$Z9071500 <= 4499.0] <- 4000.0
data$Z9071500[4500.0 <= data$Z9071500 & data$Z9071500 <= 4999.0] <- 4500.0
data$Z9071500[5000.0 <= data$Z9071500 & data$Z9071500 <= 9.9999999E7] <- 5000.0
data$Z9071500 <- factor(data$Z9071500,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071600[1.0 <= data$Z9071600 & data$Z9071600 <= 499.0] <- 1.0
data$Z9071600[500.0 <= data$Z9071600 & data$Z9071600 <= 999.0] <- 500.0
data$Z9071600[1000.0 <= data$Z9071600 & data$Z9071600 <= 1499.0] <- 1000.0
data$Z9071600[1500.0 <= data$Z9071600 & data$Z9071600 <= 1999.0] <- 1500.0
data$Z9071600[2000.0 <= data$Z9071600 & data$Z9071600 <= 2499.0] <- 2000.0
data$Z9071600[2500.0 <= data$Z9071600 & data$Z9071600 <= 2999.0] <- 2500.0
data$Z9071600[3000.0 <= data$Z9071600 & data$Z9071600 <= 3499.0] <- 3000.0
data$Z9071600[3500.0 <= data$Z9071600 & data$Z9071600 <= 3999.0] <- 3500.0
data$Z9071600[4000.0 <= data$Z9071600 & data$Z9071600 <= 4499.0] <- 4000.0
data$Z9071600[4500.0 <= data$Z9071600 & data$Z9071600 <= 4999.0] <- 4500.0
data$Z9071600[5000.0 <= data$Z9071600 & data$Z9071600 <= 9.9999999E7] <- 5000.0
data$Z9071600 <- factor(data$Z9071600,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
data$Z9071700[1.0 <= data$Z9071700 & data$Z9071700 <= 499.0] <- 1.0
data$Z9071700[500.0 <= data$Z9071700 & data$Z9071700 <= 999.0] <- 500.0
data$Z9071700[1000.0 <= data$Z9071700 & data$Z9071700 <= 1499.0] <- 1000.0
data$Z9071700[1500.0 <= data$Z9071700 & data$Z9071700 <= 1999.0] <- 1500.0
data$Z9071700[2000.0 <= data$Z9071700 & data$Z9071700 <= 2499.0] <- 2000.0
data$Z9071700[2500.0 <= data$Z9071700 & data$Z9071700 <= 2999.0] <- 2500.0
data$Z9071700[3000.0 <= data$Z9071700 & data$Z9071700 <= 3499.0] <- 3000.0
data$Z9071700[3500.0 <= data$Z9071700 & data$Z9071700 <= 3999.0] <- 3500.0
data$Z9071700[4000.0 <= data$Z9071700 & data$Z9071700 <= 4499.0] <- 4000.0
data$Z9071700[4500.0 <= data$Z9071700 & data$Z9071700 <= 4999.0] <- 4500.0
data$Z9071700[5000.0 <= data$Z9071700 & data$Z9071700 <= 9.9999999E7] <- 5000.0
data$Z9071700 <- factor(data$Z9071700,
levels=c(0.0,1.0,500.0,1000.0,1500.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0),
labels=c("0",
"1 TO 499",
"500 TO 999",
"1000 TO 1499",
"1500 TO 1999",
"2000 TO 2499",
"2500 TO 2999",
"3000 TO 3499",
"3500 TO 3999",
"4000 TO 4499",
"4500 TO 4999",
"5000 TO 99999999: 5000+"))
return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
"SOMETHING STOLEN FROM SCH 1997",
"THREATENED TO BE HURT SCH 1997",
"FIGHT AT SCH 1997",
"LATE SCH WITHOUT EXCUSE 1997",
"# DAYS ABSENT FROM SCH? 1997",
"TEACHERS GOOD AG/DIS 1997",
"TEACHER INTEREST STUDENTS AG/DIS 1997",
"STUDENTS DISRUPT LEARNING AG/DIS 1997",
"STUDENTS GRADED FAIRLY AG/DIS 1997",
"CHEATING TESTS/ HOMEWRK AG/DIS 1997",
"DISCIPLINE IS FAIR AG/DIS 1997",
"FEEL SAFE AT SCH AG/DIS 1997",
"# DAYS SMOKED LAST 30 DAYS 1997",
"# DAYS ALCOHOL LAST 30 DAYS 1997",
"# DAY USE MARIJUANA LAST 30 DAYS 1997",
"KEY!SEX (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"CV_CENSUS_REGION 1997",
"CV_SAMPLE_TYPE 1997",
"CV_HGC_BIO_DAD 1997",
"CV_HGC_BIO_MOM 1997",
"KEY!RACE_ETHNICITY (SYMBOL) 1997",
"R EVER USE COC/DRUGS? 1998",
"R'S PARENTS DIVORCED IN LAST 5 YRS 2002",
"CV_CENSUS_REGION 2010",
"CV_HGC_EVER 2010",
"CV_HRLY_PAY L1 2010",
"CV_HRLY_PAY L2 2010",
"CV_HRLY_PAY L3 2010",
"CV_HRLY_PAY L4 2010",
"CV_HRLY_PAY L5 2010",
"CV_HRLY_PAY L6 2010",
"CV_HRLY_PAY L7 2010",
"CV_HRLY_PAY L8 2010",
"CV_HRLY_PAY L9 2010",
"CV_MARSTAT 2010",
"CV_BIO_CHILD_HH 2010",
"CV_CENSUS_REGION 2011",
"CV_HGC_EVER 2011",
"CV_HRLY_PAY L1 2011",
"CV_HRLY_PAY L2 2011",
"CV_HRLY_PAY L3 2011",
"CV_HRLY_PAY L4 2011",
"CV_HRLY_PAY L5 2011",
"CV_HRLY_PAY L6 2011",
"CV_HRLY_PAY L7 2011",
"CV_HRLY_PAY L8 2011",
"CV_HRLY_PAY L9 2011",
"CV_HRLY_PAY L10 2011",
"CV_HRLY_PAY L11 2011",
"CV_HRLY_PAY L12 2011",
"CV_HRLY_PAY L13 2011",
"CV_MARSTAT 2011",
"CV_BIO_CHILD_HH 2011",
"CV_CENSUS_REGION 2013",
"CV_HGC_EVER 2013",
"CV_HRLY_PAY L1 2013",
"CV_HRLY_PAY L2 2013",
"CV_HRLY_PAY L3 2013",
"CV_HRLY_PAY L4 2013",
"CV_HRLY_PAY L5 2013",
"CV_HRLY_PAY L6 2013",
"CV_HRLY_PAY L7 2013",
"CV_HRLY_PAY L8 2013",
"CV_HRLY_PAY L9 2013",
"CV_HRLY_PAY L10 2013",
"CV_MARSTAT 2013",
"CV_BIO_CHILD_HH 2013",
"CV_CENSUS_REGION 2015",
"CV_HGC_EVER 2015",
"CV_HRLY_PAY L1 2015",
"CV_HRLY_PAY L2 2015",
"CV_HRLY_PAY L3 2015",
"CV_HRLY_PAY L4 2015",
"CV_HRLY_PAY L5 2015",
"CV_HRLY_PAY L6 2015",
"CV_HRLY_PAY L7 2015",
"CV_HRLY_PAY L8 2015",
"CV_HRLY_PAY L9 2015",
"CV_HRLY_PAY L10 2015",
"CV_HRLY_PAY L11 2015",
"CV_HRLY_PAY L12 2015",
"CV_MARSTAT 2015",
"CV_BIO_CHILD_HH 2015",
"CV_BIO_CHILD_HH_U18 2015",
"CV_CENSUS_REGION 2017",
"CV_HGC_EVER 2017",
"CV_HRLY_PAY L1 2017",
"CV_HRLY_PAY L2 2017",
"CV_HRLY_PAY L3 2017",
"CV_HRLY_PAY L4 2017",
"CV_HRLY_PAY L5 2017",
"CV_HRLY_PAY L6 2017",
"CV_HRLY_PAY L7 2017",
"CV_HRLY_PAY L8 2017",
"CV_HRLY_PAY L9 2017",
"CV_HRLY_PAY L10 2017",
"CV_HRLY_PAY L11 2017",
"CV_HRLY_PAY L12 2017",
"CV_HRLY_PAY L13 2017",
"CV_HRLY_PAY L14 2017",
"CV_HRLY_PAY L15 2017",
"CV_MARSTAT 2017",
"CV_BIO_CHILD_HH 2017",
"CV_BIO_CHILD_HH_U18 2017",
"CV_CENSUS_REGION 2019",
"CV_HGC_EVER 2019",
"CV_HRLY_PAY L1 2019",
"CV_HRLY_PAY L2 2019",
"CV_HRLY_PAY L3 2019",
"CV_HRLY_PAY L4 2019",
"CV_HRLY_PAY L5 2019",
"CV_HRLY_PAY L6 2019",
"CV_HRLY_PAY L7 2019",
"CV_HRLY_PAY L8 2019",
"CV_HRLY_PAY L9 2019",
"CV_HRLY_PAY L10 2019",
"CV_HRLY_PAY L11 2019",
"CV_MARSTAT 2019",
"CV_BIO_CHILD_HH_U18 2019",
"CV_CENSUS_REGION 2021",
"CV_HGC_EVER 2021",
"CV_HRLY_PAY L1 2021",
"CV_HRLY_PAY L2 2021",
"CV_HRLY_PAY L3 2021",
"CV_HRLY_PAY L4 2021",
"CV_HRLY_PAY L5 2021",
"CV_HRLY_PAY L6 2021",
"CV_HRLY_PAY L7 2021",
"CV_HRLY_PAY L8 2021",
"CV_HRLY_PAY L9 2021",
"CV_HRLY_PAY L10 2021",
"CV_MARSTAT 2021",
"CV_BIO_CHILD_HH_U18 2021",
"CVC_WKSWK_ADULT_ALL",
"CVC_HOURS_YR_ALL L0",
"CVC_HOURS_YR_ALL L1",
"CVC_HOURS_YR_ALL L2",
"CVC_HOURS_YR_ALL L3",
"CVC_HOURS_YR_ALL L4",
"CVC_HOURS_YR_ALL L5",
"CVC_HOURS_YR_ALL L6",
"CVC_HOURS_YR_ALL L7",
"CVC_HOURS_YR_ALL L8",
"CVC_HOURS_YR_ALL L9",
"CVC_HOURS_YR_ALL L10",
"CVC_HOURS_YR_ALL L11",
"CVC_HOURS_YR_ALL L12",
"CVC_HOURS_YR_ALL L13",
"CVC_HOURS_YR_ALL L14",
"CVC_HOURS_YR_ALL L15",
"CVC_HOURS_YR_ALL L16",
"CVC_HOURS_YR_ALL L17",
"CVC_HOURS_YR_ALL L18",
"CVC_HOURS_YR_ALL L19",
"CVC_HOURS_YR_ALL L20",
"CVC_HOURS_YR_ALL L21",
"CVC_HOURS_YR_ALL L22",
"CVC_HOURS_YR_ALL L80",
"CVC_HOURS_YR_ALL L81",
"CVC_HOURS_YR_ALL L82",
"CVC_HOURS_YR_ALL L83",
"CVC_HOURS_YR_ALL L84",
"CVC_HOURS_YR_ALL L85",
"CVC_HOURS_YR_ALL L86",
"CVC_HOURS_YR_ALL L87",
"CVC_HOURS_YR_ALL L88",
"CVC_HOURS_YR_ALL L89",
"CVC_HOURS_YR_ALL L90",
"CVC_HOURS_YR_ALL L91",
"CVC_HOURS_YR_ALL L92",
"CVC_HOURS_YR_ALL L93",
"CVC_HOURS_YR_ALL L94",
"CVC_HOURS_YR_ALL L95",
"CVC_HOURS_YR_ALL L96",
"CVC_HOURS_YR_ALL L97",
"CVC_HOURS_YR_ALL L98",
"CVC_HOURS_YR_ALL L99",
"CVC_RND"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("PUBID_1997",
"YSCH-35900_1997",
"YSCH-36000_1997",
"YSCH-36100_1997",
"YSCH-36200_1997",
"YSCH-36300_1997",
"YSCH-36400_1997",
"YSCH-36500_1997",
"YSCH-36600_1997",
"YSCH-36700_1997",
"YSCH-36800_1997",
"YSCH-36900_1997",
"YSCH-37000_1997",
"YSAQ-361_1997",
"YSAQ-365_1997",
"YSAQ-371_1997",
"KEY_SEX_1997",
"KEY_BDATE_M_1997",
"KEY_BDATE_Y_1997",
"CV_CENSUS_REGION_1997",
"CV_SAMPLE_TYPE_1997",
"CV_HGC_BIO_DAD_1997",
"CV_HGC_BIO_MOM_1997",
"KEY_RACE_ETHNICITY_1997",
"YSAQ-372B_1998",
"YHEA-3000_2002",
"CV_CENSUS_REGION_2010",
"CV_HGC_EVER_EDT_2010",
"CV_HRLY_PAY.01_2010",
"CV_HRLY_PAY.02_2010",
"CV_HRLY_PAY.03_2010",
"CV_HRLY_PAY.04_2010",
"CV_HRLY_PAY.05_2010",
"CV_HRLY_PAY.06_2010",
"CV_HRLY_PAY.07_2010",
"CV_HRLY_PAY.08_2010",
"CV_HRLY_PAY.09_2010",
"CV_MARSTAT_2010",
"CV_BIO_CHILD_HH_2010",
"CV_CENSUS_REGION_2011",
"CV_HGC_EVER_EDT_2011",
"CV_HRLY_PAY.01_2011",
"CV_HRLY_PAY.02_2011",
"CV_HRLY_PAY.03_2011",
"CV_HRLY_PAY.04_2011",
"CV_HRLY_PAY.05_2011",
"CV_HRLY_PAY.06_2011",
"CV_HRLY_PAY.07_2011",
"CV_HRLY_PAY.08_2011",
"CV_HRLY_PAY.09_2011",
"CV_HRLY_PAY.10_2011",
"CV_HRLY_PAY.11_2011",
"CV_HRLY_PAY.12_2011",
"CV_HRLY_PAY.13_2011",
"CV_MARSTAT_2011",
"CV_BIO_CHILD_HH_2011",
"CV_CENSUS_REGION_2013",
"CV_HGC_EVER_EDT_2013",
"CV_HRLY_PAY.01_2013",
"CV_HRLY_PAY.02_2013",
"CV_HRLY_PAY.03_2013",
"CV_HRLY_PAY.04_2013",
"CV_HRLY_PAY.05_2013",
"CV_HRLY_PAY.06_2013",
"CV_HRLY_PAY.07_2013",
"CV_HRLY_PAY.08_2013",
"CV_HRLY_PAY.09_2013",
"CV_HRLY_PAY.10_2013",
"CV_MARSTAT_2013",
"CV_BIO_CHILD_HH_2013",
"CV_CENSUS_REGION_2015",
"CV_HGC_EVER_EDT_2015",
"CV_HRLY_PAY.01_2015",
"CV_HRLY_PAY.02_2015",
"CV_HRLY_PAY.03_2015",
"CV_HRLY_PAY.04_2015",
"CV_HRLY_PAY.05_2015",
"CV_HRLY_PAY.06_2015",
"CV_HRLY_PAY.07_2015",
"CV_HRLY_PAY.08_2015",
"CV_HRLY_PAY.09_2015",
"CV_HRLY_PAY.10_2015",
"CV_HRLY_PAY.11_2015",
"CV_HRLY_PAY.12_2015",
"CV_MARSTAT_2015",
"CV_BIO_CHILD_HH_2015",
"CV_BIO_CHILD_HH_U18_2015",
"CV_CENSUS_REGION_2017",
"CV_HGC_EVER_EDT_2017",
"CV_HRLY_PAY.01_2017",
"CV_HRLY_PAY.02_2017",
"CV_HRLY_PAY.03_2017",
"CV_HRLY_PAY.04_2017",
"CV_HRLY_PAY.05_2017",
"CV_HRLY_PAY.06_2017",
"CV_HRLY_PAY.07_2017",
"CV_HRLY_PAY.08_2017",
"CV_HRLY_PAY.09_2017",
"CV_HRLY_PAY.10_2017",
"CV_HRLY_PAY.11_2017",
"CV_HRLY_PAY.12_2017",
"CV_HRLY_PAY.13_2017",
"CV_HRLY_PAY.14_2017",
"CV_HRLY_PAY.15_2017",
"CV_MARSTAT_2017",
"CV_BIO_CHILD_HH_2017",
"CV_BIO_CHILD_HH_U18_2017",
"CV_CENSUS_REGION_2019",
"CV_HGC_EVER_EDT_2019",
"CV_HRLY_PAY.01_2019",
"CV_HRLY_PAY.02_2019",
"CV_HRLY_PAY.03_2019",
"CV_HRLY_PAY.04_2019",
"CV_HRLY_PAY.05_2019",
"CV_HRLY_PAY.06_2019",
"CV_HRLY_PAY.07_2019",
"CV_HRLY_PAY.08_2019",
"CV_HRLY_PAY.09_2019",
"CV_HRLY_PAY.10_2019",
"CV_HRLY_PAY.11_2019",
"CV_MARSTAT_2019",
"CV_BIO_CHILD_HH_U18_2019",
"CV_CENSUS_REGION_2021",
"CV_HGC_EVER_EDT_2021",
"CV_HRLY_PAY.01_2021",
"CV_HRLY_PAY.02_2021",
"CV_HRLY_PAY.03_2021",
"CV_HRLY_PAY.04_2021",
"CV_HRLY_PAY.05_2021",
"CV_HRLY_PAY.06_2021",
"CV_HRLY_PAY.07_2021",
"CV_HRLY_PAY.08_2021",
"CV_HRLY_PAY.09_2021",
"CV_HRLY_PAY.10_2021",
"CV_MARSTAT_2021",
"CV_BIO_CHILD_HH_U18_2021",
"CVC_WKSWK_ADULT2_ALL_XRND",
"CVC_HOURS_WK_YR_ALL.00_XRND",
"CVC_HOURS_WK_YR_ALL.01_XRND",
"CVC_HOURS_WK_YR_ALL.02_XRND",
"CVC_HOURS_WK_YR_ALL.03_XRND",
"CVC_HOURS_WK_YR_ALL.04_XRND",
"CVC_HOURS_WK_YR_ALL.05_XRND",
"CVC_HOURS_WK_YR_ALL.06_XRND",
"CVC_HOURS_WK_YR_ALL.07_XRND",
"CVC_HOURS_WK_YR_ALL.08_XRND",
"CVC_HOURS_WK_YR_ALL.09_XRND",
"CVC_HOURS_WK_YR_ALL.10_XRND",
"CVC_HOURS_WK_YR_ALL.11_XRND",
"CVC_HOURS_WK_YR_ALL.12_XRND",
"CVC_HOURS_WK_YR_ALL.13_XRND",
"CVC_HOURS_WK_YR_ALL.14_XRND",
"CVC_HOURS_WK_YR_ALL.15_XRND",
"CVC_HOURS_WK_YR_ALL.16_XRND",
"CVC_HOURS_WK_YR_ALL.17_XRND",
"CVC_HOURS_WK_YR_ALL.18_XRND",
"CVC_HOURS_WK_YR_ALL.19_XRND",
"CVC_HOURS_WK_YR_ALL.20_XRND",
"CVC_HOURS_WK_YR_ALL.21_XRND",
"CVC_HOURS_WK_YR_ALL.22_XRND",
"CVC_HOURS_WK_YR_ALL.80_XRND",
"CVC_HOURS_WK_YR_ALL.81_XRND",
"CVC_HOURS_WK_YR_ALL.82_XRND",
"CVC_HOURS_WK_YR_ALL.83_XRND",
"CVC_HOURS_WK_YR_ALL.84_XRND",
"CVC_HOURS_WK_YR_ALL.85_XRND",
"CVC_HOURS_WK_YR_ALL.86_XRND",
"CVC_HOURS_WK_YR_ALL.87_XRND",
"CVC_HOURS_WK_YR_ALL.88_XRND",
"CVC_HOURS_WK_YR_ALL.89_XRND",
"CVC_HOURS_WK_YR_ALL.90_XRND",
"CVC_HOURS_WK_YR_ALL.91_XRND",
"CVC_HOURS_WK_YR_ALL.92_XRND",
"CVC_HOURS_WK_YR_ALL.93_XRND",
"CVC_HOURS_WK_YR_ALL.94_XRND",
"CVC_HOURS_WK_YR_ALL.95_XRND",
"CVC_HOURS_WK_YR_ALL.96_XRND",
"CVC_HOURS_WK_YR_ALL.97_XRND",
"CVC_HOURS_WK_YR_ALL.98_XRND",
"CVC_HOURS_WK_YR_ALL.99_XRND",
"CVC_RND_XRND")
return(data)
}

new_data <- qnames(new_data)
# save as rds file
saveRDS(new_data, file = here("./data/NLS_data.rds"))
