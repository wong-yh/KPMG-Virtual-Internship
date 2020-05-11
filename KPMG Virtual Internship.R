library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)

# P.S. The first row "Note: The data and information in this document is ..." was deleted. 

df1<-read_excel("Data/KPMG_VI_New_raw_data_update_final.xlsx",sheet="Transactions")
df2<-read_excel("Data/KPMG_VI_New_raw_data_update_final.xlsx",sheet="NewCustomerList")
df3<-read_excel("Data/KPMG_VI_New_raw_data_update_final.xlsx",sheet="CustomerDemographic")
df4<-read_excel("Data/KPMG_VI_New_raw_data_update_final.xlsx",sheet="CustomerAddress")

#By going through the dataset, Data cleansing (Data Pre-treatment)
levels(factor(df3$gender))
df3 <- df3 %>% 
      mutate(gender = recode(gender, Femal = "F",
                                     Female = "F",
                                    Male = "M"))

df3$customer_id[!df3$customer_id %in% df4$customer_id]

#Convert the serial date to date format, Excel:January 1, 1900
df3$DOB<-as.Date(as.numeric(df3$DOB,na.rm=TRUE), origin = "1899-12-30")
df2$DOB<-as.Date(as.numeric(df2$DOB,na.rm=TRUE), origin = "1899-12-30")
df1$product_first_sold_date<-as.Date(as.numeric(df1$product_first_sold_date,na.rm=TRUE), origin = "1899-12-30")

levels(factor(df4$state))
df4 <- df4 %>%
       mutate(state= recode(state, "New South Wales" = "NSW", "Victoria" = "VIC"))
                           
##df2
levels(factor(df2$gender))
df3 <- df3 %>% 
  mutate(gender = recode(gender, Female = "F",
                                  Male = "M"))


#Combine CustomerDemographic with CustomerAddress by customer ID
df5<-merge(df3, df4,, by="customer_id", all=TRUE)
#It is noted that there are additional three new customer ID in df4
#df5: Orgingal Customer Information

#df6 Put customer information (involved) into transaction dataset
df6<-merge(df1, df5, by.x="customer_id", all.x =TRUE)

#df7 Sorting by customer ID, involving transaction
names(df6)
df7<- df6 %>% 
  select(customer_id,list_price,standard_cost,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  group_by(customer_id,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  summarise (freq=n(),total_revenue=sum(list_price), avg_spend=mean(list_price), median_spend= median(list_price))


#By considering the ordered status (valid transaction)
df6.1<-df6
df6.1<-df6[df6$order_status=="Approved",]

df7.1<- df6.1 %>% 
  select(customer_id,list_price,standard_cost,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  group_by(customer_id,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  summarise (freq=n(),total_revenue=sum(list_price),avg_spend=mean(list_price),median_spend= median(list_price))

#Approved transaction #6.1,7.1
#Per no. of transaction 
hist(df6.1$list_price)
df6.1$Age<-2020-year(df6.1$DOB)
summary(df6.1$Age)

hist(df6.1$list_price, breaks=100,xlab="Amount (AUD)", ylab=" No. of transaction", main= "Histogram of transaction amount in 2017 by each transaction")

#Age setting
df6.1$Age_range<-cut(df6.1$Age,breaks=seq(0,100,10), labels=c('0-10', '11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100'),right = TRUE )
barplot(table(df6.1$Age_range))
table(df6.1$Age_range)

##Per customer
hist(df7.1$total_revenue[!df7.1$total_revenue %in% boxplot.stats(df7.1$total_revenue)$out], breaks=50,xlim=c(0,14000),xlab="Amount (AUD)", ylab=" No. of customer", main= "Histogram of transaction amount in 2017 by each customer")
hist(df7.1$avg_spend, breaks=50,xlab="Amount (AUD)", ylab=" No. of customer", main= "Histogram of average transaction amount in 2017 by each customer")
hist(df7.1$median_spend, breaks=50,xlab="Amount (AUD)", ylab=" No. of customer", main= "Histogram of medain transaction amount in 2017 by each customer")

hist(df7.1$past_3_years_bike_related_purchases)


#Customer Information
barplot(table(df7.1$gender),xlab="Gender" ,ylab="No. of customer",main=" Customer's gender")
barplot(table(df7.1$state),xlab="State" ,ylab="No. of customer",main=" State") #NSW
barplot(table(df7.1$owns_car),xlab="Own car" ,ylab="No. of customer",main=" Ownership of car")
barplot(table(df7.1$wealth_segment),xlab="wealth_segment" ,ylab="No. of customer",main=" Customer's wealth segment") #Mass customer
barplot(table(df7.1$job_industry_category),xlab="Job Industry" ,ylab="No. of customer",main=" Customer's Job Industry",las=2,cex.names=0.6) # Manufacturing/Financial State

###
###
#age
df7.1$Age<-2020-year(df7.1$DOB)
summary(df7.1$Age)
cut(c(10,21),breaks=seq(0,40,10), labels=c('0-10', '11-20','21-30','31-40'),right = TRUE)
df7.1$Age_range<-cut(df7.1$Age,breaks=seq(0,100,10), labels=c('0-10', '11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100'),right = TRUE )
barplot(table(df7.1$Age_range))


#grouped (Per customer)
barplot(table(df7.1$wealth_segment, df7.1$Age_range),legend.text = levels(factor(df7.1$wealth_segment)),xlab="Age",ylab="No.of customer",las=2, main=" No. of customer by age group and wealth segment")
barplot(table(df7.1$state, df7.1$Age_range),legend.text = levels(factor(df7.1$state)),xlab="Age",ylab="No.of customer",las=2, main=" No. of customer by age group and state")
barplot(table(df7.1$wealth_segment, df7.1$state),legend.text = levels(factor(df7.1$wealth_segment)),xlab="State",ylab="No.of customer",las=2, main=" No. of customer by state and wealth segment")

#grouped (Per transaction)
barplot(table(df6.1$wealth_segment, df6.1$Age_range),legend.text = levels(factor(df6.1$wealth_segment)),xlab="Age",ylab="No.of transaction",las=2, main=" No. of transaction in 2017 by age group and wealth segment")
barplot(table(df6.1$state, df6.1$Age_range),legend.text = levels(factor(df6.1$state)),xlab="Age",ylab="No.of transaction",las=2, main=" No. of transaction in 2017 by age group and state")
barplot(table(df6.1$wealth_segment, df6.1$state),legend.text = levels(factor(df6.1$wealth_segment)),xlab="State",ylab="No.of transaction",las=2, main=" No. of transaction in 2017 by state and wealth segment")

levels(factor(df6.1$wealth_segment))
table(df6.1$state, df6.1$Age_range)

#NSW, Affluent customer (most customer)




#complicated  grouped bar chart
#Approach 1:
# revenue
# separate by state (stacked)
# show by age (x-axis)

dft<-df7.1 %>% select(total_revenue,state,Age_range) %>%
  group_by(state,Age_range)%>%
  summarise(value=sum(total_revenue)/12)


dftNSW<-dft[which(dft$state=="NSW"),]
dftQLD<-dft[which(dft$state=="QLD"),]
dftVIC<-dft[which(dft$state=="VIC"),]


#To find the age range that QLD doesn't have
dft[!(dft$Age_range %in% dftQLD$Age_range),]$Age_range

#approach
dft4<-merge(dftNSW[,-1],dftVIC[,-1], by="Age_range", all = TRUE)
dft4<-merge(dft4, dftQLD[,-1], by="Age_range", all = TRUE)
dft4<-dft4[-9,] #Without unknown age

rownames<-dft4$Age_range
dft4<-dft4[,-1]
colnames(dft4) <- c("NSW", "VIC","QLD")
rownames(dft4)<- rownames  #without unknown 
rownames(dft4)<- c("11-20" ,"21-30" ,"31-40" ,"41-50", "51-60" ,"61-70" ,"71-80" ,"81-90","Unknown"  )

barplot(t(dft4), beside=TRUE,xlab="Age" ,ylab="Amount(AUD)",main=" Monthly Average Transaction Amount by age group and state", 
        ylim=range(pretty(c(0, t(dft4)))),legend.text=c("NSW", "VIC","QLD"),
        cex.axis=0.6,cex.names=0.8, las=2, col=c("darkblue","red","black"))

#Wealth Segment 
#Step1
dfw<-df7.1 %>% select(total_revenue,wealth_segment,Age_range) %>%
  group_by(wealth_segment,Age_range)%>%
  summarise(value=sum(total_revenue)/12)

#Step2
levels(factor(dfw$wealth_segment))
dfwAC<-dfw[which(dfw$wealth_segment=="Affluent Customer"),]
dfwHN<-dfw[which(dfw$wealth_segment=="High Net Worth"),]
dfwMC<-dfw[which(dfw$wealth_segment=="Mass Customer"),]

dfw[!(dfw$Age_range %in% dfwHN$Age_range),]$Age_range

#Step3
dfw4<-merge(dfwAC[,-1],dfwHN[,-1], by="Age_range", all = TRUE)
dfw4<-merge(dfw4, dfwMC[,-1], by="Age_range", all = TRUE)
dfw4<-dfw4[-9,] #Without unknown age

#Step4
rownames<-dfw4$Age_range
dfw4<-dfw4[,-1]
colnames(dfw4) <- c("Affluent Customer" ,"High Net Worth","Mass Customer" )
rownames(dfw4)<- rownames  #without unknown 
rownames(dfw4)<- c("11-20" ,"21-30" ,"31-40" ,"41-50", "51-60" ,"61-70" ,"71-80" ,"81-90"  ,"Unknown")

#Step5
barplot(t(dfw4), beside=TRUE,xlab="Age" ,ylab="Amount(AUD)",main=" Monthly Average Transaction Amount by age group and wealth sector", 
        ylim=range(pretty(c(0, t(dft4)))),legend.text=c("Affluent Customer" ,"High Net Worth","Mass Customer" ),
        cex.axis=0.6,cex.names=0.8, las=2, col=c("darkblue","red","black"))

#past_3_years_bike_related_purchases

dfr<-df7.1 %>% select(past_3_years_bike_related_purchases,Age_range) %>%
  group_by(Age_range)%>%
  summarise(value=sum(past_3_years_bike_related_purchases))

rownames<-dfr$Age_range
dfr<-dfr[,-1]
dfr<-dfr[-9,]
rownames(dfr)<-c('11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90' )
table(dfr)

barplot(t(dfr),xlab="Age" ,ylab="No of purchase",main=" Past 3 years bike related purchases by age group", 
        ylim=range(pretty(c(0, t(dfr)))),
        cex.axis=0.6,cex.names=0.8, las=2, col=rainbow(8))

#job_industry_category
dfj<-df7.1 %>% select(past_3_years_bike_related_purchases,job_industry_category) %>%
  group_by(job_industry_category)%>%
  summarise(value=sum(past_3_years_bike_related_purchases))

rownames<-dfj$job_industry_category
dfj<-dfj[,-1]
dfj<-dfj[-11,]
rownames(dfj)<-c("Argiculture","Entertainment","Financial Services","Health" ,"IT" ,"Manufacturing","n/a","Property" ,"Retail" ,"Telecommunications" )
table(dfj)
rownames
barplot(t(dfj),xlab="Job Industry" ,ylab="No of purchase",main=" Past 3 years bike related purchases by job industry", 
        ylim=range(pretty(c(0, t(dfj)))),
        cex.axis=0.6,cex.names=0.6, las=2, col=blues9)

#job_industry_category +amount
dfj<-df7.1 %>% select(total_revenue,job_industry_category) %>%
  group_by(job_industry_category)%>%
  summarise(value=sum(total_revenue)/12)

rownames<-dfj$job_industry_category
dfj<-dfj[,-1]
dfj<-dfj[-11,]
rownames(dfj)<-c("Argiculture","Entertainment","Financial Services","Health" ,"IT" ,"Manufacturing","n/a","Property" ,"Retail" ,"Telecommunications" )

rownames
barplot(t(dfj),xlab="Job Industry" ,ylab="Amount(AUD)",main=" Monthly Average Transaction Amount by job industry", 
        ylim=range(pretty(c(0, t(dfj)))),
        cex.axis=0.6,cex.names=0.6, las=2, col=blues9)

##Product feature
dfp<- df6.1 %>%
      select(brand, product_line, product_class,product_size, list_price,standard_cost) %>%
      group_by(brand, product_line, product_class,product_size) %>%
      summarise(price=sum(list_price),cost= sum(standard_cost),profit=sum(list_price)-sum(standard_cost) ,median_price=median(list_price),median_cost=median(standard_cost), average_price=mean(list_price),average_cost=mean(standard_cost))

dfb<- df6.1 %>%
  select(brand,list_price, standard_cost) %>%
  group_by(brand) %>%
  summarise(price=sum(list_price),cost= sum(standard_cost),profit=sum(list_price)-sum(standard_cost), median_price=median(list_price),median_cost=median(standard_cost), average_price=mean(list_price),average_cost=mean(standard_cost))

####
#Step1
dfp1<-dfp %>% select(brand, product_line,price) %>%
  group_by(brand, product_line)%>%
  summarise(price=sum(price))

#Step2
levels(factor(dfp1$product_line))
dfp1MO<-dfp1[which(dfp1$product_line=="Mountain"),]
dfp1RO<-dfp1[which(dfp1$product_line=="Road"),]
dfp1ST<-dfp1[which(dfp1$product_line=="Standard"),]
dfp1TO<-dfp1[which(dfp1$product_line=="Touring"),]

#Checking
dfp1[!(dfp1$brand %in% dfp1MO$brand),]$brand
dfpp<-0
#Step3
dfpp<-merge(dfp1MO[,-2],dfp1RO[,-2], by="brand", all = TRUE)
dfpp<-merge(dfpp, dfp1ST[,-2], by="brand", all = TRUE)
dfpp<-merge(dfpp, dfp1TO[,-2], by="brand", all = TRUE)


#Step4
rownames<-dfpp$brand
dfpp<-dfpp[,-1]
colnames(dfpp) <- levels(factor(dfp1$product_line))
rownames(dfpp)<- rownames

#Step5
barplot(t(dfpp), beside=TRUE,xlab="Brand" ,ylab="Amount(AUD)",main="Transaction Amount in 2017 by brand and product line", 
        width = 20,ylim=c(0,3800000),
        cex.axis=0.5,cex.names=0.6, las=2, col=c("darkblue","red","green","orange"))

legend("topright", 
       legend =  colnames(dfpp),
       fill = 1:3, ncol = 4,
       cex = 0.5)

#By brand (Amount)
dfb1<- dfb[,c(1,2)]
dfb1$price<- dfb1$price/1000000
dfb1<-dfb1[-7,]
rownames<-dfb1$brand
dfb1<-dfb1$price
dfb1<-as.table(dfb1)
rownames(dfb1)<- rownames

barplot(t(dfb1),xlab="Brand" ,ylab="Amount(million AUD)",main="Transaction Amount in 2017 by brand", 
        ylim= c(0,5), 
        cex.axis=0.5,cex.names=0.6, las=2, col=c("purple"))

#By brand (Profit)
dfb2<- dfb[,c(1,4)]
dfb2$profit<- dfb2$profit/1000000
dfb2<-dfb2[-7,]
rownames<-dfb2$brand
dfb2<-dfb2$profit
dfb2<-as.table(dfb2)
rownames(dfb2)<- rownames

barplot(t(dfb2),xlab="Brand" ,ylab="Profit(million AUD)",main="Total Profit in 2017 by brand", 
        ylim= c(0,3), 
        cex.axis=0.5,cex.names=0.6, las=2, col=c("purple"))

