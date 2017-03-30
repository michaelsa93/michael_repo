read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")
set.seed(1234)
x<-rnorm(n=500,mean=4,sd=1)
y<-summary(x)
#without printing
# The order of arguments does not matter if the names are specified
#of tumors (from litter 2)
wt<-c(5,6,7)
ko<-c(8,9,11)
#try default t-test settings
t.test(wt,ko)
# Do standard 2-sample t-test
t.test(wt,ko,var.equal = T)
#save the results as a variable
wt.vs.ko=t.test(wt,ko,var.equal = T)
# what are the different parts of this data frame?
names(wt.vs.ko)
# Just print the p-value
wt.vs.ko$p.value
# what commands did we use?
history(max.show = Inf)
# Get the list of all the packages installed
library()
# Get all packages currently loaded in the R environment
search()
# Installing a new R package
install.packages("glmnet", repos= "cran or github url", dependencies = T)
# Variables in R
x<-3
x
print (x)
x+2
y<-4
x+y
x = 6
x+y
height<-1.80
weight<-100
bmi<-weight/(height)^2
#Exercise 3 Let’s do some math and logical
1+2
1-3
45/6
456*3.2
4^5
1>2
2>1
2<=2
2==1
2!=1
2!=1|2>1
x>y
#Booleans: all means all have to match parameters to be true any just means if any match its true
all(c(T,F,T))
any(c(T,F,T))
all(c(5>-1, 3>=1,1<1))
any(c(5>-1, 3>=1,1<1))
#Exercise 4 data types
x<-1
y<-2.1
z<-"metabolomics"
zz<-"metablomics is cool"
a1<-"1"
x+1
x+z
a1+1
as.integer(a1)+1
as.character(x)
typeof(x)
#creating a sequence from 5 to 13
(v<-5:13)
# Colon can be used also for non integers (still increments of 1)
(v<-6.6:12.6)
#If the final element specified does not belong to the sequence then its discarded
(v<-3.8:11.4)
#create vector with elements from 5-9 increasing by 0.4
(v<-seq(5,9, by = 0.4))
#create an all numerical vector
(s<-c(1,3,7:12))
#the non-character values are coerced to character type if one of the elements is a character
(s<-c('apple','red',5,T))
# Accessing vector elements using position
t<-c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat")
(u<-t[c(1,7,4)])
# Accessing vector elements using negative indexing
(x<-t[c(-7,-5)])
# Accessing vector elements using logical indexing
(v<-t[c(T,F,F,T,F,F,F)])
# Accessing vector elements using 0/1 indexing
(y<-t[c(0,0,0,0,0,0,1)])
# Select elements with a pattern
cpds<-c("xx_a","xx_b","xx_c","xx_d","yy_a", "yy_b")
grep("^xx",cpds)
cpds[-grep("^xx",cpds)]
length(cpds[-grep("^xx",cpds)])
cpds[grep("^xx",cpds)]
length(cpds[grep("^xx",cpds)])
# Vector1D 1type of data, matrix 2D 1 type of data, list 1D more flexible on type of data
# Create 2 vectors
v1<-c(1,4,7,3,8,15)
v2<-c(12,9,4,11,0,8)
# Vector adddition
(add.result<-v1+v2)
# Vector subtraction
(sub.result<-v1-v2)
# vector multiplication
(multi.result<-v1*v2)
# vector divison
(divi.result<-v1/v2)
# creating matrices
#byrow=true means that the values fill by row byrow = false means they fill columns first
# Elements are arranged sequentially by row
(M<-matrix(c(5:19), nrow=3, byrow = T))
# Elements are arranged sequentially by column
(N<-matrix(c(5:19), nrow=3,byrow=F))
# Define the column and row names
rownames<-c("row1","row2","row3")
colnames<-c("col1","col2","col3","col4","col5")
(P<-matrix(c(5:19), nrow = 3,byrow = T, dimnames=list(rownames, colnames)))
print(P)
# Access the element at 2nd column and 5th row
print(P[2,5])
# Rows are first and columns second in this annotation [x,y]
# Access only the 2nd row
print(P[2,])
# Subset 1:2 row 3:5 column
print(P[1:2, 3:5])
# Access only the 3rd column
print(P[,3])
# Matrix computations
# create 2 2x3 matrices
(matrix1<-matrix(c(3,9,-1,4,2,6),nrow=2))
(matrix2<-matrix(c(5,2,0,9,3,4),nrow = 2))
# Add the matrices
matrix1+matrix2
# Subtract the matrices
matrix1-matrix2
# Matrix algebra
# A (2x3) matrix A
(matrixA<-matrix(c(3,9,-1,4,2,6),nrow=2))
# A (3x4) matrix B
(matrixB<-matrix(c(5,2,0,9,3,4,5,6,7,11,3,4), nrow = 3))
# a result is a (2x4) matrix
matrixA %*% matrixB

#Exercise 7: Let's create lists
#creat a list containing heterogenous elements
(list_data <-list(c("Jan","Feb","Mar"),T,
                      matrix(c(3,9,5,1,-2,8), nrow=2),
                      list("green",12.3)))
# Create named list
(myinfo<-list(first="Lan",last="Nguyen",yearofbirth=1990, male=F))
list_data
# Name elements of list_data
names(list_data)<-c("1st Quarter", "Logical","A_Matrix","A Inner list")
list_data
# Get the second element of the list (my last name)
myinfo[2]
myinfo$last
# Get the same information using named element
myinfo$first
# Similarly we have:
list_data[3]
list_data$'A Inner list'
# Create the data frame
emp.data<-data.frame(emp_id=c(1:5),emp_name=c("Rick","Dan","Michelle","Ryan","Gary"),
salary=c(623.3,515.2,611.0,729.0,843.25),
start_date=as.Date(c("2012-01-01","2013-09-23","2014-11-15","2014-05-11","2015-03-27")),
stringsAsFactors = F)
# Print the data frame
print(emp.data)
# Get the structure of the data frame
str(emp.data)
# Print first 2 rows of emp.data
head(emp.data,2)
#print the summary (gives basic stats)
print(summary(emp.data))
# Extract specific columns
(result<-data.frame(emp.data$emp_name,emp.data$salary))
# Add the dept column
emp.data$dept<-c("IT","Operations","IT","HR","Finance")
print(emp.data)
# Create the second data frame
emp.newdata<-data.frame(emp_id=c(6:8),emp_name=c("Rasmi","Pranab","Tusar"),salary=c(578.0,722.5,632.8),start_date=as.Date(c("2013-05-21","2013-07-30","2014-06-17")),dept=c("IT","Operations","Finance"),stringsAsFactors = F)
# Bind the two data frames
(emp.finaldata<-rbind(emp.data,emp.newdata))

# Exercise 8: Let's create factors
month.abb
print(is.factor(month.abb))

# Apply the factor function
(factor_months<-factor(month.abb))
print(is.factor(factor_months))
#Print data frame function
head(data.frame)
default.stringsAsFactors()
# Ordering the levles of a factor
(months<-month.name[sample(1:12,10)])
(months<-factor(months))
(months<-factor(months,levels=month.name))
(months<-factor(months,levels=month.name,labels=month.abb))

#Control statments

#If Statements
traffic_light<-"green"
if(traffic_light=="green"){print("go.")}
# in the if statement the condition will be in {} but the conditions need to be a boolean (true or false)
# If-else statement
if(traffic_light=="green"){print("Go.")}else{print("Stay")}
traffic_light<-"yellow"
if(traffic_light=="green"){print("Go.")}else if(traffic_light=="yellow"){print("Get ready.")} else {print("stay")}
# good for duplicates/isotopes/adducts in post MS-FLO workflow
# Note the syntax: The if statements are followed by a boolean expression wrapped in parenthesis (adding white space is a good practice). The conditional block of code is inside curly braces.

# For Loops: a statement specifying iterations, which allows for a block of code to be executed repeatedly
for(i in 1:5){print(i^2)}
#can add variables in between 2 lines to change a variable above is simple formula below is slightly different
for(k in 1:5){
  i=k+4
  print(k^2)}
# not the curly braces syntax
#While loops: similar to for loops, but repeat the execution as long as the boolean condition supplied is true
i = 1
while(i <= 5){
  cat("i =", i, "\n")
  i=i+1
}
#cat can have multiple characters and combine them together but print cannot otherwise they are essentially the same
#people usually use for loops, while loops not so much
#*lines matter
#Next: halts the processof the current iteration and advances the looping index. Apply only to the innermost of nested loops.
for(i in 1:10) {
  if(i<=5){
    print("skip")
    next
  }
  cat(i, "is greater than 5.\n")
}
for(i in 1:10) {
  if(i<=5){
    print("skip")
    break
  }
  cat(i, "is greater than 5.\n")
}
#break command will stop the code after 1 loop

#Apply function
# apply is general can be used for any outcome, lapply will give list outcome, sapply will give vector outcome
# the function call for apply function is of the form apply(x, margin, fun,...)
#x is an array/matrix
#Margin is a vector giving the subscripts which the function will be applied over
#Fun is the function to be applied
set.seed(12345)
(X<-matrix(rnorm(30), nrow=5, ncol=6))
print(X)
apply(X,2,sum)
#applys a function
# 1 means column 2 means row
apply(X, 2, function(x) length(x[x<0]))
# function(x) is the input and length is the output this code says apply this function to matix X columns and have it do a function that counts all the values below 0
#Note: in matrix margin=1 indicates rows and Margin=2 indicates columns
myFun<-function(x,base=NULL) {
  if(!is.null(base)) x<-x+base
  return(length(x[x<0]))
}
apply(X,2,myFun)
apply(X,2,myFun, base=0.2)

#lapply:if we would like to repeatedly apply a function to a element of a list we use lapply
#examples of conventions: [[1]]=list [1]=vector
lapply(1:3,function(x)x^2)
unlist(lapply(1:3, function(x)x^2))
sapply(1:3,function(x)x^2)
sapply(1:3,function(x)x^2,simplify=F)
#sapply is the same as lapply but returns a "simplified" output aka vectors
#the functions sort and order are designed to be applied on vectors.
#sort returns a sorted vector
#order gives you the position of the lowest value
z.vec<-c(5,3,8,2,3.2)
sort(z.vec)
order(z.vec)
#default setting is lowest to highest (above)
#can change to reverse by the code below
order(z.vec,decreasing = T)
x<-matrix(c(2,1,1,3,.5,.3,.5,.2), ncol = 2)
#to sort the second column in decreasing order
x1<-x[order(x[,2], decreasing = T)]
#to sort the first column in the already partially sorted matrix
x2<-x1[order(x1[,1]),]
#If both columns are numeric, your negatives sort in the reverse order of positives
x[order(x[,1], -x[,2]),]
#if the values aren't known to be numeric, convert them to numeric before sorting
x[order(xtfrm(x[,1]), -xtfrm(x[,2])),]

#Missing Values are assigned special value of "NA"
z<-c(1:3,NA)
ind<-is.na(z)
#To remove missing values from a vector
print(z)
x<-z[!is.na(z)]

#How to import data
# current working directory is the location which R is currently pointing to
#if you run R commands in R console then the cmd is your home directory. If instead you are running R in from an R markdown or from an Rnotebook then the working directory is the location of the document
#To see the current working directory use:
getwd()
#to change working directory use setwd(path_name) with a specific path as na argument
setwd("path/to/directory")
# to specify the path to file on mswindows systems
# use the \ insead of the /
mydata<-read.table("path/to/filenam.csv",header=T, sep=",")

#.CSV Data import
#text files in table format can be read and saved to a selected variable using the function:
read.table()
#example is a comma delimited text file with a filename extension.csv these files use a comma as column separators
#If the 1st row in your file contains the variable id's then set the argument header = T to let R know these should be column names
#specify the separator for the columns in your data file with sep argument(default sep is a white space), but for comma-deliminated files set sep="," and for tab deliminated files sep="\t"
#you can optionally use row.names or col.names arguments to set the row and column names.
#use ?read.table to learn more about the function
#Saving data in R to a file
#text file:
#tab-delimited
write.table(mydata,"path/to/filename.txt", sep="\t")
#comma-delimited
write.table(mydata,"path/to/filename.csv", sep=",")
# comma-delimited
write.csv(mydata,"path/to/filename.csv"))
# *list is not a table

#excel spreadsheet
library(xslx)
write.xlsx(mydata, "path/to/filename.xlsx")

#read and inspect an example file
#after you type data type tab
data.raw<-read.csv("./data/mx 107155 _lung cancer tissue_summer course_08-2015_submit.csv",header = F,stringsAsFactors = F)
head(data.raw)
data.raw[20,10]
data.raw[1:5,8:12]
data.raw[,1]
data.raw[1,]
data.raw[10,]
cpd.names<-data.raw[,1]
cpd.names[1:20]
#write an if statement to get the names of all the iSTDs
for(i in cpd.names){
  if(length(grep('^z',i)>0)){
    print(i)
  }
}
#explanation: so cpd.names is the 1st column of data.raw.
#grep will only give you the value of the cell the length specifies the parameters of what to count
#so the command means search for the first column for all values that contain a z will be greater than 1 and if it has a z
#getting the row number and name of iSTDs
for(i in 1:length(cpd.names)){
  if(length(grep('^z',cpd.names[i])>0)){
    print(c(i,cpd.names[i]))
  }
#explanation: this just added the vector for position so know you know where each individual istd is in the raw.data
#lets get all the sample ids taht have a label "tumor"
data.raw[1, grep("tumor",data.raw[10,])]
#explanation: we want to look at the sample names and show me all the samples that contain the word tumor in row 10
#lets get all the columnst that has sample data
data.raw[1,grep("bbdsa",data.raw[1,])]


#ggplot2

library(ggplot2)
data("economics")
str(economics)
plot(unemploy/pop ~ date, data = economics, type =
       "l")
#~is the sign for regression
library(ggplot2)
ggplot(data = economics, aes(x = date, y = unemploy/pop)) + geom_line()
#code without specifying a graph type just means data won't be graphed
ggplot(data = economics, aes(x = date, y = unemploy/pop))
#remove grayscale
ggplot(data = economics, aes(x = date, y = unemploy/pop)) +
  geom_line() + theme_bw()
#new data
economics$month <-months(economics$date)
economics$year <- format(economics$date, format=
                           "%Y")
head(economics)
cat("Data type of economics$month:"
    , class(economics$month),
    "\n")
#Convert the character vector to a ordered factor vector
economics$month <- factor(economics$month, levels = month.name)
head(economics$month)
data2009 <- subset(economics, year == 2009)
data2014 <- subset(economics, year == 2014)
plot(unemploy/pop ~ as.numeric(month), data = data2009,
     ylim = c(0.025, 0.05), type =
       "l")
lines(unemploy/pop ~ as.numeric(month), data = data2014,
      col =
        "red")
legend("topleft"
       , c("2009"
           ,
           "2014"), title=
         "Year"
       ,
       col=c("black"
             ,
             "red"), lty = c(1,1))
# %in% means that the years in this case will be grouped together?
data2009_2014 <- subset(economics, year %in% c(2014, 2009))
ggplot(data = data2009_2014, aes(x = month, y = unemploy/pop)) +
  geom_line(aes(group = year, color = year))
#Angle axis names so they dont overlap
ggplot(data = data2009_2014, aes(x = month, y = unemploy/pop)) +
  geom_line(aes(group = year, color = year)) +
  theme(axis.text.x = element_text(angle = 45))
#theme functions make the graphs prettier
ggplot(data = economics, aes(x = month, y = unemploy/pop)) +
  geom_line(aes(group = year, color = year)) +
  theme(axis.text.x = element_text(angle = 45))

#example 2
data("diamonds")
str(diamonds)
head(diamonds)
hist(diamonds$price)
ggplot(diamonds, aes(x = price)) + geom_histogram()
# set.seed(12345) makes the sample reproducible
set.seed(12345)
#dsmall is just a smaller version of the diamond data
dsmall <- diamonds[sample(nrow(diamonds), 200), ]
colorMap <- data.frame(color = rainbow(length(unique(dsmall$color))))
rownames(colorMap) <- unique(dsmall$color)
plot(price ~ carat, data = dsmall, col = colorMap[dsmall$color,
                                                  "color"])
legend(x =
         'bottomright'
       , legend = rownames(colorMap),
       col = colorMap$color, pch = par("pch"), bty =
         'n'
       , xjust = 1)
#showing how much easier/better ggplot is
ggplot(data = dsmall, aes(x = carat, y = price, color = color)) +
  geom_point()
help.search("geom_", package = "ggplot2")
p1 <- ggplot(dsmall, aes(x = carat, y = price))
p1 + geom_point()
#have color sorted by diamonds color
p1+geom_point(aes(color=color))
#set shape by diamond cut
p1+geom_point(aes(shape=cut))
#set color and shape
p1 + geom_point(aes(shape = cut, color = color))
#variable vs fixed aesthetics
ggplot(data = dsmall, aes(x = carat, y = price)) +
  geom_point(aes(size = 2), color =
               "darkgreen")
ggplot(data = dsmall, aes(x = carat, y = price)) +
  geom_point(aes(fill = cut), size = 2, color =
               "black"
             , shape = 25)
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
ggplot(df2, aes(x = x, y = y)) +
  geom_point(aes(shape = z), size = 3,
             colour =
               "darkgreen"
             , fill =
               "orange") +
  scale_shape_identity()
#Data Transformation
ggplot(dsmall, aes(x = log(carat), y = log(price))) + geom_point()
dsmall2 <- diamonds[sample(nrow(diamonds), 100), ]
p2 <- ggplot(dsmall2, aes(x = log(carat), y = log(price)))
p2 + geom_text(aes(label = color))
p2 + geom_label()
p2 + geom_label(aes(label = color))
library(ggrepel)
p2 + geom_point() + geom_text_repel(aes(label=color), size = 3)
idx <- sample(c(TRUE, FALSE), nrow(dsmall2), replace = TRUE, prob = c(0.5, 0.5)


#Lecture 4
dat<-read.csv("Data/EconomistData.csv")
ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_jitter()
j1 <- ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_jitter(alpha = I(1/5))
j2 <- ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_jitter(alpha = I(1/50))
j3 <- ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_jitter(alpha = I(1/200))
library(gridExtra)
grid.arrange(j1, j2, j3, ncol = 3)
ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_boxplot()+geom_jitter()
h <- ggplot(data = diamonds, aes(x = carat)) + geom_histogram()
d <- ggplot(data = diamonds, aes(x = carat)) + geom_density()
grid.arrange(h, d, ncol = 2)
p <- ggplot(data = diamonds, aes(x = carat)) + xlim(0, 3)
h1 <- p + geom_histogram(binwidth = 0.5)
h2 <- p + geom_histogram(binwidth = 0.1)
h3 <- p + geom_histogram(binwidth = 0.05)
grid.arrange(h1, h2, h3, ncol = 3)
d1 <- p + geom_density(adjust = 5)
d2 <- p + geom_density(adjust = 1)
d3 <- p + geom_density(adjust = 1/5)
grid.arrange(d1, d2, d3, ncol = 3)
h <- p + geom_histogram(aes(fill = cut), position =
                          "dodge"
                        , bins = 10)
d <- p + geom_density(aes(color = cut))
grid.arrange(h, d, ncol = 2)
h <- p + geom_histogram(aes(fill = cut), position =
                          "stack")
d <- p + geom_density(aes(fill = cut), position =
                        "stack")
grid.arrange(h, d, ncol = 2)
b1 <- ggplot(diamonds, aes(x = color)) + geom_bar()
b2 <- ggplot(diamonds, aes(x = color)) + geom_bar(aes(weight = carat)) + ylab
grid.arrange(b1, b2, ncol = 2)
diamond.counts <- table(diamonds["color"])
df <- data.frame(diamond.counts)
colnames(df) <- c("color"
                  ,
                  "count")
df
ggplot(df, aes(x=color, y=count)) + geom_bar(stat=
                                               "identity")
p1 <- ggplot(dsmall, aes(x = carat, y = price))
p1 + geom_point() + scale_y_sqrt()
pEc <- ggplot(dat, aes(Percent.of.15plus.with.bank.account, SEDA.Current.level))
(pEc <- pEc + geom_point(aes(color = Region)) + scale_color_brewer(palette = "Set1"))



