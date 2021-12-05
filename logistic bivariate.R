
gr_freq_bi = function(var,start,interval)
{
  freq_yes = c()
  freq_no = c()
  group = c()
  per_yes  = c()
  per_no = c()
  end  = 0
  while(end<max(var)){
    
    end = start + interval
    len1 = length(var[var>=start & var<end & bank$y == "yes"])
    len2 = length(var[var>=start & var<end & bank$y == "no"])
    group = c(group,paste(start,end,sep = "-"))
    start = end
    freq_yes = c(freq_yes,len1)
    freq_no  = c(freq_no,len2)
    per_yes  = c(per_yes,round((len1/length(var))*100,4))
    per_no  = c(per_no,round((len2/length(var))*100,4))
  }
  tab = as.data.frame(cbind(group,freq_yes,freq_no,per_yes,per_no)) 
  return(tab)
}

#___________________________________________________________________________________________________
#___________________________________________________________________________________________________

#AGE

boxplot(bank$age~bank$y)

by(bank$age,bank$y,summary)

gr_freq_bi(bank$age,10,10)

t.test(bank$age~bank$y)


# Out of 5594 persons (13% of population) belonging to the group 20-30 who were contacted for term deposit
# 888 persons (2.15%) said yes and 4706 persons (11.42%) said no,

# Out of 16938 persons (41.12%) belonging to the age group 30-40, who were contacted
# 1715(4.16%) said yes and 15223 (36.95%) said no.

# Out of 10526 persons (25.55%) belonging to the age group 40-50, who were contacted for term deposit
# 834 persons (2.02% ) said yes and 9692 persons (23.53%) said no.

# Out of 6862 presons (16.66%) belonging to the age group 50-60, who were contacted
# 697 persons(1.69%) said yes and 6165 presons (1.14%) said no.


# When we drew the summary of the persons who responded positively(yes) and compared it to the 
# summary of the persons who responded no. we saw similar results in every section. Even the boxplots
# looked similar

# However after doing a t-test the results were clear.
# There is an association between the variables age and y.
# the t-value is 4.779 std error away from 0 on the left side.
# Thus we reject the null hypothesis and accept the alternative hypothesis, which is The true diference 
# in mean is not equal to 0



#____________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________


#JOB

table(bank$y,bank$job)

barplot(table(bank$y,bank$job))

propr(bank$job)

woe_cat(bank$job)           # IV = 0.1886 (medium predictive power)

#                  no   yes Total
# admin.        87.03 12.97 10422
# blue-collar   93.11  6.89  9254
# entrepreneur  91.48  8.52  1456
# housemaid     90.00 10.00  1060
# management    88.78 11.22  2924
# retired       74.77 25.23  1720 **
# self-employed 89.51 10.49  1421
# services      91.86  8.14  3969
# student       68.57 31.43   875 **
# technician    89.17 10.83  6743
# unemployed    85.80 14.20  1014
# unknown       88.79 11.21   330

# Only 875 students were contacted. 31.43% of them agreed to apply for term deposite.
# out of 1720 retired persons 25.23% agreed to do a term deposit. Due to retirement these people will
# best interest rates (for being senior citizens). Plus the amount of money for retirement can be well
# uses to do a term deposite.


# Lets perform the chi square test

c = chisq.test(bank$y,bank$job)
c$p.value
c
# The Null Hypothesis is (according to bhaskar) both y and job are random
# i.e. there is no influence of job on y
# which is very close to zero and less than the tabulated value of chi square
# So we reject Null Hypothesis. And we can say that there is an association
# between y and job





#________________________________________________________________________________________


# MARRIAGE

by(bank$marital,bank$y,summary)

propr(bank$marital)

barplot(table(bank$y,bank$marital))

woe_cat(bank$marital)     # IV = 0.0281 (weak)

# Out of 4612 divorced persons 10.32% said yes, similarly out of 24928 married persons
# 10% said yes, while single and unknown 14% and 15% said yes respectively
# Most of the yes came from married persons (2532 yes) followed by single persons (1620)



chisq.test(bank$marital,bank$y)

# The p-value from chi squared test came very close to 0. 
# there is an association between the marital and y




#_______________________________________________________________________________________________


# EDUCATION 

barplot(table(bank$y,bank$education))

propr(bank$education)

chisq.test(bank$education,bank$y)

woe_cat(bank$education)        # IV = 0.0486 (weak)

# Most of the calls were made to the persons having university degree. The most number
# of yes came from them aswell (1670 persons i.e. 13% of them said yes)
# Approximately 10% of all the high school passed persons applied for term deposit (1031 persons)
# Also 22% of the illeterate persons did term deposit. However that is not very significant
# because only 18 persons were approached with the offer and 4 of them said yes. These persons
# are either retired or an entreprenure and one was self employed

# There is associction between y and education








#__________________________________________________________________________________________

# DEFAULT

table(bank$y,bank$default)

propr(bank$default)

woe_cat(bank$default)      # IV = 0.1277 (weak)

# 12% of the ersons who does not have any credit in default took loan
# while 443 of 8154 (5.15%) of the people who didnt disclose their default status took loan
# persons who have default in credit didnt apply for the loan


chisq.test(bank$y,bank$default)

#There is a relationship between y and default status





#___________________________________________________________________________________________________


# HOUSING

table(bank$housing,bank$y)

propr(bank$housing)

woe_cat(bank$housing)  # IV = 0.0014 not offen use

# Most of the persons who were contacted already had a housing loan (53% of all the people, 21576 persons)
# Out of those persons 2507 applied for term deposit (11%). Out of 883 persons who
# didnot disclose their housing loan status 107 persons applied for term deposit. 
# Out of 18k people who didn't have any housing loan 2k applied for term deposit.

# When we perform chi squared test we found out that

chisq.test(bank$y,bank$housing)

# the p-value is 0.05829, which means we accep the Null Hypothesis.
# So we can say that there is no association between y and housing.






#__________________________________________________________________________________________

# LOAN

table(bank$y,bank$loan)

propr(bank$loan)
 
woe_cat(bank$loan)    # IV = 0000 not often use

# 10% of customers from all the segment (i.e. with personal loan, without and unknown) have 
# agreed to apply for term deposit. Since persons without personal loans were more than without
# personal loan, The no. of application for term deposit came from them.

# When we did chi squared test we observed

chisq.test(bank$y,bank$loan)

# The p-value is 0.5787
# So we accept the Null Hypothesis. There is no association between the two variables.






#______________________________________________________________________________________________________


# CONTACT

table(bank$y,bank$contact)

propr(bank$contact)

woe_cat(bank$contact)        # IV = 0.2516 (medium)

# Approximately 15% of the people with cellular phones agreed to apply in the term deposit
# while only 5 % of the people with cellphone invested in a term deposite.

chisq.test(bank$y,bank$contact)$p.value

# When we did the chi squared test we found out that the p-vallue is very cloase to 0
# Thus we reject the null hypothesis
# We can conclude that there is a relationship between contact and y






#_________________________________________________________________________________________________________

# MONTH

table(bank$y,bank$month)

propr(bank$month)

woe_cat(bank$month)  # IV = 0.4855 (High)

# Out of all the persons who were called during spring (16947) 10% of them applied for term deposit
# Out of all the persons who were called during summer (18670)  9% of them applied for term deposit
# Out of all the persons who were called during autumn (5389) 18.31% of them applied to term deposit
# Out of all the persons who were called during winter (182) 48.9% of them applird to term deposit
# However we should keep in mind that during winter, December was the only month when calls were made.
# Out of those 182 people 89 people said yes.

# Spring -> 10%
# Summer -> 9.97%
# Autums -> 18.31%
# Winter -> 48.90%

# 



table(bank$y)
(276+886+539)/4640

# Out of all the persons who said yes 33.36% were said during spring
# Out of all the persons who said yes 40.00% were said during summer
# Out of all the persons who said yes 21.27% were said during autumn
# Out of all the persons who said yes 01.90% were said during winter

chisq.test(bank$month,bank$y)


# The p-value for chi squared test is very close to 0
# So we conclude that there is an association between the two variables


# Spring -> 33.36%
# Summer -> 40%
# Autumn -> 21.27%
# Winter -> 1.9%

names(bank)



#________________________________________________________________________________________________________



# day_of_week


table(bank$y,bank$day_of_week)

# All the responses are equally distributed among all the days

chisq.test(bank$y,bank$day_of_week)

# However there is an association between y and day_of_week

woe_cat(bank$day_of_week)     # IV 0.0065 (no use)


#_________________________________________________________________________________________________________

#CAMPAIGN

propr(bank$campaign)

table(bank$y,bank$campaign)

chisq.test(bank$campaign,bank$y)

woe_cat(bank$campaign)

#__________________________________________________________________________________________________________

# pdays

table(bank$y,bank$pdays)

# Approximately 78% of the persons out of 36000 persons (contacted for the first time) said no.
# on the other hand if a person was contacted previously the the no of no is lesser than the number of yes
# So we thought of converting 999 into 0 (i.e absence of past calls) and all the other into 1 (i.e. previous
# calls were present)

bank$pdays = ifelse(bank$pdays == 999, 0,1)
table(bank$pdays)

table(bank$y,bank$pdays)

# People who were contacted more than once, 63% of them invested in term deposit
# While people who were contacted for the first time, 78% of them said no.


woe_cat(bank$pdays)  # Iv = 0.5513 (high)




#_________________________________________________________________________________________________________________________________


names(bank)
# prevous


woe_cat(bank$previous)






#_________________________________________________________________________________________________________________________


# POUTCOME

table(bank$y,bank$poutcome)

barplot(table(bank$y,bank$poutcome))

chisq.test(bank$poutcome,bank$y)

woe_cat(bank$poutcome)   # IV = 0.5477 (High)

# Out of all the persons who didnt aply for the previous campaign 14% applied in the present campaign
# Out of all those people who aplied for the previous campaing 65% applied in this campaign as well
# This proves that the decision of the bank to contact their previous customers was right.
# 32% of the persons who applied for the term deposit were contacted previously
# chi squared test suggests that there is an association between the two variables

# However there is an anoma

(605+894)/(605+894+3141)

names(bank)







#_______________________________________________________________________________________________________________

# emp.var.rate

table(bank$y,bank$emp.var.rate)

t.test(bank$emp.var.rate~bank$y)



# rate of saying yes is more in the group of people having emp.var.rate less than 1.1
# t-test suggests that there is an association between the two variables






#_______________________________________________________________________________________________________________


# cons.price.idx: consumer price index - monthly indicator (numeric)


by(bank$cons.price.idx,bank$y,summary)



gr_freq_bi(bank$cons.price.idx,92,0.5)


t.test(bank$cons.price.idx~bank$y )$p.value

#The p-valueis very less than 0.05
# so we can safely conclude that the two variables are associated






#_________________________________________________________________________________________________________

# cons.con.idx :  consumer confidence index

table(bank$y,bank$cons.conf.idx)

t.test(bank$cons.conf.idx~bank$y)


propr(bank$cons.conf.idx)

# The p-value is very close to 0
# which means there is a significant difference in mean
# thus we can say that there is association between y and confidence rate



#_______________________________________________________________________________________________________________


# euribor3m

gr_freq_bi(bank$euribor3m,0,1)

t.test(bank$euribor3m~bank$y)

# the rate influences the decision
# there is a huge percent of yes when the rate is very less

# after doing t-test we have seen that there is an association between the variables.....




#__________________________________________________________________________________________________________

# nr.employed: number of employees - quarterly indicator (numeric)

gr_freq_bi(bank$nr.employed,4950,50)

t.test(bank$nr.employed~bank$y)

# More the nr.employed increase the precentage of yes for a ertain group decreased
# however on a total the maximun percent of yes came from the group 5200-5250

# kub bhako kore accept hoyeche ( according to bhaskar)
# the p-value is close to 0 so there is a significant difference in the mean.
# thus there is an association between themmmmmmmmmmmmmmmmmm



