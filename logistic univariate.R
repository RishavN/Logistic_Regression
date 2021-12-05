library(e1071)
library(dplyr)
setwd("C:\\Users\\Rishav\\Desktop")

bank = read.csv("bank-additional-full.csv",sep = ";")


#################################################
# creating a function for grouped frequency table
#################################################

gr_freq_uni = function(var,start,interval)
{
  freq = c()
  name = c()
  per  = c()
  end  = 0
  while(end<max(var)){
    
    end = start + interval
    len = length(var[var>=start & var<end])
    name = c(name,paste(start,end,sep = "-"))
    start = end
    freq = c(freq,len)
    per  = c(per,round((len/length(var))*100,3))
  }
  tab = as.data.frame(cbind(name,freq,per)) 
  return(tab)
}


# AGE
dim(bank) # 41188 customers     21 variables including y

summary(bank$age) 

length(unique(bank$age)) # 78 qnique age groups

hist(bank$age)

skewness(bank$age) # 0.7846 --> moderately skewes

g = gr_freq_uni(bank$age,10,10)
g
# age: age of the persons contacted (numeric continuous variable)

#The distribution of the variable is moderately skewed with a tail towards
#the right side.The minimum age of the of the person called is 17 and the maximum 
#age is 98. About 41.12 percent of the persons called were of the age group 30 - 40
#25 % of the persons are of age group 40-50, which means 65% of targetted people are of age group
#30-50 , the persons who probably has a job. 95% of the targetted people were between 20 - 60.






#______________________________________________________________________________________________________
#______________________________________________________________________________________________________


#JOB

table(bank$job)

barplot(table(bank$job),main = "Barplot of job")


# Most of persons contacted are administrative workers, blue-collar workers, technicians.
# Probably persons of these category are more interested in savings due to average salary.
# 25.30 % of the persons contacted are administrative workers. 22.46 % of the pesrons called
# were blue collar workers, and 16.37% were technicians.
# It is clear that the bank is targetting the section of economy with average income.
# Middle Class family tends to save more thus there is a greater chance of applying for a term deposite.


#______________________________________________________________________________________________________
#______________________________________________________________________________________________________


#MARITAL : Marital status of the contacted person (categotical variable)

# Out of all the persons contacted 
#  4612 were divorced
#  24928 were married
#  11568 were single
#  80 didnt share their contacts

# The huge amount of married people doesnt surprise us as most of the people 
# who were contacted are of the age group 30-60 (83.33 %). Married people tend to do more term deposit
# as they need to save for their future, retirements, students education, etc.

table(bank$marital)

barplot(table(bank$marital))




#_________________________________________________________________________________________

# EDUCATION : How far is the person educated (categorical variable)

table(bank$education)

barplot(table(bank$education))



# Most of the persons contated had a university degree (29.5%).
# Obviously persons with higher degree might have better jobs and thus will have better
# knowledge for making financial decisions.
# The high school educated persons were the next targetted segment (23.10%). 








#___________________________________________________________________________________________________


# DEFAULT : If the person has credit in default (binary variable)

table(bank$default)

barplot(table(bank$default))

# Most of the persons does not have credit in default
# However 8597 Persons didnt disclose theri default status
# Only 3 persons has credit in default





#________________________________________________________________________________________

# HOUSING : If the person has any house loan (binary variable)

table(bank$housing)

# Most of the contacted persons have a housing loan. Approximately  52.38% people has a
# housing loan. Few persons didnt want to disclose their status on housing loans, and the rest
# didnot have any house loan.




#___________________________________________________________________________________________________________

names(bank)
# LOAN: If the person has any personal loans (binary variable)

table(bank$loan)

# 82.42 % of the persons contacted doesnot have any personal loan
# Some of the persons didnot disclose their personal loan status (990 persons).






#___________________________________________________________________________________________________

# CONTACT : Mode of contact cellphone/telephone (binary variable)

table(bank$contact)

# More than half the persons were contacted in cellular phones.(26144 of 41188 persons)
# 





#__________________________________________________________________________________________________

names(bank)
# MONTH

table(bank$month)

# Most of the calls were made during may (13769 calls), followed by july (7174 calls)
# Most of the calls 
# march, april, may, june, july, aug, sept, oct, nov, dec

# Most of the calls were made in the summer (june to august)- 45.32%
# Followed by in the spring (march - may) - 41.14%
# So 86% of the calls were made during march to august. This seems to be the most active time for the
# bank. While during winter (dec - feb) no calls were made during january and february


# Summer -> June to August 45.32%
# Autumn -> Sept to November 13.08%
# Winter -> December to February 0.4%
# Spring -> March to May 41.14%







#____________________________________________________________________________________________________________



# day_of_week : on which day the customer were contacted (catgorical variable)

table(bank$day_of_week)

# The bank is closed on Saturdays and Sundays.
# All the phones were made uniformly on all days.



# ***
#_____________________________________________________________________________________________________________


names(bank)

# DURATION : call duration during last call (continuous variable) ***

hist(bank$duration)

gr_freq_uni(bank$duration,0,300)

summary(bank$duration)

length(bank$duration[bank$duration==0])



#________________________________________________________________________________________________________________

# CAMPAIGN : Nos. of times a person was called during the present campaign (term deposit)

table(bank$campaign)


# 42.83% of the people were contacted only once.
# 25.66% of the people were contacted twice during this campaign.
# The number of people reduced with the increase in number of contact.









#________________________________________________________________________________________________________________


# pdays : no. of days passed after the client was called for previous campaign {term deposit}(numeric discrete variable)
        # no. of days passed after the cliend was called for mutual fund

table(bank$pdays)

# Here 999 means that the person was never contacted before, nad 0 means that the person was called that day 
# itself.
# 39673 persons were never called before in any other campaign.
# there are 439 customers who were called 4 days before the present campaign.
# and 412 customers who were called 6 days before the present campaign

View(bank)





#_________________________________________________________________________________________________________________

#PREVIOUS :  no. of contacts performed before the current campaign (numeric discrete variable)
           # no, of contats done before term deosit
           # no. of contacts done for mutual fund, credit card, etc.

table(bank$previous)

# 35563 persons were not contacted 

39673-35563

View(bank[bank$previous !=0 & bank$pdays == 999,])

length(bank$previous[bank$previous!=0 & bank$pdays==999])

sum(bank$previous[bank$previous!=0 & bank$previous != 1 & bank$pdays==999])

sum(bank$previous[bank$previous!=0 & bank$previous != 1 & bank$previous != 2 & bank$pdays==999])


(414-214)*2



4561-4110

# there is an anomaly in pdays.
# in previous we can see that 35563 persons were not contacted in the previous campaign
# however in pdays there are 39673 persons under the value '999' ( which also means that in the previous campaign
# 37673 persons were not contacted at all) There is a mismatch of 4110 persons.
# when we cross checked and have seen that
# all the 35563 persons from the variable 'previous' are consisted in the 39673 persons of the variable 'pdays' 



#______________________________________________________________________________________________

names(bank)

#poutcome: outcome of previous marketing campaign (categorical)
         # did the person subscribe for mutual fund?
table(bank$poutcome)


# Out of 41188 persons 35563 persons were not contacted for the previous campaign
# Out of the rest  5625 persons 1373 persons said yes to the previous campaign.
# There was a 24% success rate for the campaign (for the persons who are called in this camaign 
# as well )

nrow(bank[bank$previous !=0 & bank$pdays == 999,])

#_____________________________________________________________________________________________


# emp.var.rate : employment variation rate - quarterly indicator (numeric continuous)

str(bank$emp.var.rate)

summary(bank$emp.var.rate)

barplot(table(bank$emp.var.rate))

hist(bank$emp.var.rate)


# The distribution is negatively skewed
# 41% of the persons have emp.var.rate les than 1.1










#___________________________________________________________________________________________________________________


# cons.price.idx: consumer price index - monthly indicator (numeric continuous variable)

summary(bank$cons.price.idx)

hist(bank$cons.price.idx)

skewness(bank$cons.price.idx) # -0.230

gr_freq_uni(bank$cons.price.idx,92,0.5)

# The range of the consumer price index is between 92.201 to 94.767
# Maximum of the consumers fall in 93.994 (7763)
# Infact 36.17% of the consumers fall in the range 93.5 - 94 c.p.i
# followed by 27.95 % of the consumers in 93. - 93.5









#_______________________________________________________________________________________________________________________


# cons.conf.idx: consumer confidence index - monthly indicator (numeric)

summary(bank$cons.conf.idx)

hist(bank$cons.conf.idx)

table(bank$cons.conf.idx)

# The range of this variavle is between -50.8 to -26.9
# Maximum no. of people fall under -36.4
# Almost 50% people fall under the range -46.2 to -41.8







#____________________________________________________________________________________________________________


#euribor3m: euribor 3 month rate - daily indicator (numeric)

# Euribor is the basic rate of interest used in lending between banks on the European Union interbank market.

summary(bank$euribor3m)

hist(bank$euribor3m)

skewness(bank$euribor3m)

gr_freq_uni(bank$euribor3m,0,1)


# 67.15% of the consumers had euribor rate for 3 months at 4-5 %
# While 23% got the rates 1-2 %
# the distribution is moderately negatively skewed

gr_freq_bi(bank$age,0,10)





#__________________________________________________________________________________________________________

# nr.employed: number of employees - quarterly indicator (numeric)

summary(bank$nr.employed)
hist(bank$nr.employed)

skewness(bank$nr.employed)


gr_freq_uni(bank$nr.employed,4950,50)

# The distribution is highly negatively skewed
# Maximu number of consumer lies in the group 5200-5250
# followed by 5150-5200 and then 5050-5100




























