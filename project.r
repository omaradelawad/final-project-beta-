


data <- read.csv("C:\\programming\\R language\\DS project\\grc.csv" , header = TRUE) 

# i. Compare cash and credit totals. 

# extract the frequency of type method => the number of credit , the number of cash
paymentsmethods <- table(data$paymentType)  

# the lable of the two paymnets methods
lable = c("cash" , "credit") 

# the ratio of the payments method the method type  divided by the (sum of methods table count) *100 
ratio <-  round(   paymentsmethods/sum(paymentsmethods)*100 , 1)

# creating the pie chart
pie(paymentsmethods,labels = paste(names(paymentsmethods) , ratio , "%") , main = "cash vs credit" , col = c("navy", "orange"))



# ii. Compare each age and sum of total spending. 

# the sum of the total and ages 
ageByTotall <- tapply(data$total , data$age , sum)

sort(table(data$customer), decreasing = TRUE)
# sorting them
sortetTotal <- sort(ageByTotall ,decreasing = TRUE)

# bar chart 
barplot(
  sortetTotal , xlab  = "age" , ylab = "total spending" , col = c("navy" , "orange")
) 


# iii. Show each city total spending and arrange it by total descending. 
 
# extracting the cities and how much is the total payment for each city 
# alex => ???? 
# cairo => ??? 
# awsan => ??? 

totalCitySpending <- tapply(data$total, data$city, sum)

sortedCities <- sort(totalCitySpending, decreasing = TRUE)

barplot(sortedCities,
        xlab = "Cities",
        ylab = "Total Spending",
        main = "Total Spending per City",
        las = 2,  
        col = rainbow(length(sortedCities)))



hist(data$total, 
     main = "Distribution of Total Spending", 
     xlab = "Total Spending", 
     col = "orange", 
     border = "black")




# another way 
plot(
  sortetTotal, 
  xlab = "Age", 
  ylab = "Total Spending", 
  pch = 12,  # شكل النقاط
  col = "navy",
  main = "Total Spending by Age"
)


ageByTotal2 <- tapply(data$total , data$age , sum)

# ترتيب الإجماليات من الأعلى إلى الأدنى
sortedTotal <- sort(ageByTotal2 , decreasing = FALSE)

# رسم بياني
dotchart(sortetTotal, main = "Age vs Total Spending", 
         xlab = "Total Spending", ylab = "Age", 
         col = "navy")





boxplot(data$total,  
        main = "Distribution of Total Spending",  
       
        xlab = "Total Spending",  

        col = "navy")  


customers = table(data$customer) 
summ  = sum(customers) 

print(summ)








library(arules)


trans = data$items 

x <- apriori(trans , parameter = )

