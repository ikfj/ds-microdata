############################################
##							##
##	CSI Seminar on Microdata Analysis	##
##				2019.12.09		##
##			Shinichi Hamamoto		##
##							##
############################################


##====================================================================
##========================================##
##	0. Use R for calculation		##
##========================================##

2+3         ###PUSH [Ctrl] + [R]
(3+5)*
2           ### comprete the program over line


##===================================================================
##========================================##
##	1. function	and Assignment		##
##========================================##

sqrt(81)		### square root function
log(100)		### natural log function
runif(20)		### uniform distributed random variables

hist( rnorm(1000) )
			### function in function
			### write histogram of 1000 normaldistributed random variables

x <- 5    		### input 5 into x object
x
x <- sqrt(2)  	### assign the result of function
x             	### overwrite x (previous input 5 disappeared) 
x^2
y<- sqrt(8)		
x * y 
z <- runif(10)	#### assign several values into one object


##===================================================================
##========================================##
##	2.Read data					##
##========================================##

	##	==============	##
	##	2.1  Import text data (NOT used in this class)
	##	==============	##
data1<-read.csv("zenshoH16enshudata_18.csv",header=F)
head(data1)

	##	==============	##
	##	2.2 Import Excel data into R 
	##	==============	
data2<-read.csv("zenshoH16enshudata_18.csv" ,header=T)

		# rewrite your path 
		# CAUTION  slash"\" can NOT be used rewrite all \ to / 
		# Add "header=T" if the excel data have the variable name at row 1


##===================================================================
##========================================##
##	3. Access the data			##
##========================================##

head(data2, 10)  		# Browse 10 cases of the data 
data2$house      		# Browse specific variable as a vector
data2[10,"house"]		# Access "house" value of the 10th case
data2[data2$house==1,"Income"]	# Browse Income values whose house value is 1

income1 <-data2[data2$house==1,"Income"]	# Assign the specific vector
income2 <-data2[data2$house==2,"Income"]	# Assign the specific vector 

#edit(data2)

##===================================================================
##========================================##
##	4. Descriptive statistics 		##
##========================================##

summary(data2$Income)
hist(data2$Income)

summary(income1)
summary(income2)
hist(income1)
hist(income2)

table(data2$house)
table1 <- ftable(data2$family, data2$house)
table1

prop.table(table1)
prop.table(table1, margin=1)		# margin 1:row
prop.table(table1, margin=2)		# margin 2:colum

##===================================================================
##========================================##
##	5.Data Handling				##
##========================================##

	##	===============	##
	##	5.1 create new variables in data
	##	===============	##

data2$engel <- data2$shokuryou / data2$consumme *100
summary(data2$engel)
hist(data2$engel)

data2$logincome <-log(data2$Income)	# "log" is log natural function
summary(data2$logincome)
hist(data2$logincome)

	##	===============	##
	##	5.2 recode the data values
	##	===============	##
data2$owner <- data2$house
data2[data2$house ==1, "owner"] <- 1
data2[data2$house ==2, "owner"] <- 0
table(data2$house,data2$owner)

data2$age_e <- data2$age
data2[data2$age==4 , "age_e"] <- 17
data2[data2$age==5 , "age_e"] <- 22
data2[data2$age==6 , "age_e"] <- 27
data2[data2$age==7 , "age_e"] <- 32
data2[data2$age==8 , "age_e"] <- 37
data2[data2$age==9 , "age_e"] <- 42
data2[data2$age==10, "age_e"] <- 47
data2[data2$age==11, "age_e"] <- 52
data2[data2$age==12, "age_e"] <- 57
data2[data2$age==13, "age_e"] <- 62
data2[data2$age==14, "age_e"] <- 67
data2[data2$age==15, "age_e"] <- 72
data2[data2$age==16, "age_e"] <- 77
data2[data2$age==17, "age_e"] <- 82
data2[data2$age==18, "age_e"] <- 87

# This recode program is also available 
#data2$age_e <- data2$age * 5 - 3
#
# or
#
#rec1<- 4:18
#rec2<- seq(17,87,len=15)
#for (i in 1:15){
#	data2[which(data2$age==rec1[i]), "age_e"] <- rec2[i]
#	}

table(data2$age,data2$age_e)


# Discriptive statistics of the new variables
summary(data2[,c("engel","logincome","age_e")])
table(data2$owner)

	##if you run the parallel computing, input all variables in c( ) function

##===================================================================
##========================================##
##	6.Regression				##
##========================================##

result1 <- lm(engel ~  logincome, data=data2)
summary(result1)

result2 <- lm(engel ~ owner + age_e + logincome, data=data2)
summary(result2)

data2$ELincome <- data2$Income / sqrt(data2$Nfamily)
result3 <- lm(engel ~ owner + age_e + ELincome, data=data2)
summary(result3)

