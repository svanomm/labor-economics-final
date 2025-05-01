library(here)

new_data <- read.table(here('./data/20250417_NLSY97_Sample.dat'), sep=' ')
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
'R1235800',
'R1302400',
'R1302500',
'R1482600',
'R2191500',
'S1249300',
'U4943100',
'U4949600',
'U4951300',
'U4956900',
'Z9065401')


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
  data$U4956900 <- factor(data$U4956900,
levels=c(0.0,1.0,2.0),
labels=c("Rural",
"Urban",
"Unknown"))
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
"CV_SAMPLE_TYPE 1997",
"CV_HGC_BIO_DAD 1997",
"CV_HGC_BIO_MOM 1997",
"KEY!RACE_ETHNICITY (SYMBOL) 1997",
"R EVER USE COC/DRUGS? 1998",
"R'S PARENTS DIVORCED IN LAST 5 YRS 2002",
"CV_CENSUS_REGION 2021",
"CV_HGC_EVER 2021",
"CV_HRLY_PAY L1 2021",
"CV_URBAN-RURAL 2021",
"CVC_WKSWK_ADULT_ALL"
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
"CV_SAMPLE_TYPE_1997",
"CV_HGC_BIO_DAD_1997",
"CV_HGC_BIO_MOM_1997",
"KEY_RACE_ETHNICITY_1997",
"YSAQ-372B_1998",
"YHEA-3000_2002",
"CV_CENSUS_REGION_2021",
"CV_HGC_EVER_EDT_2021",
"CV_HRLY_PAY.01_2021",
"CV_URBAN-RURAL_2021",
"CVC_WKSWK_ADULT2_ALL_XRND")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
summary(categories)

#************************************************************************************************************

# save as rds file
saveRDS(new_data, file = here("./data/NLS_data.rds"))

