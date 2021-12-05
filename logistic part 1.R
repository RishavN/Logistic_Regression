library(dplyr)

setwd("C:\\Users\\Rishav\\Desktop")

bank = read.csv("bank-additional-full.csv",sep = ";")

View(bank)

str(bank)

####
table(bank$job)  # job is nominal variable (categorical)
                 # 12 factors
                 #  * admin    * blue-collar   * entrepreneur   * housemaid    * management     
                 #  * retired  * self-employed * services       * student      * technician
                 #  * unemployed * UNKNOWN (330 obs)


####
table(bank$marital) # marital is nominal (categorical)
                    # 4 factors
                    # * divorced     * married     * single    * UNKNOWN  (80 obs)


####
table(bank$education) # education is ordinal (categorical)
                      # 8 factors
                      # * basic.4y(class 4)   * basic.6y(class 6)     * basic.9y(class 9)
                      # * illiterate          * professional course   * university degree
                      # * high school         * UNKNOWN (1731)


####
table(bank$default)   # binary variable  [if the person has a credit default] (categorical)
                      # * no    * yes    * UNLNOWN (8597)

####
table(bank$housing)   # binary variable  [if the person has a housing loan] (categorical)

                      # * no     * yes    *UNKNOWN (990)


####
table(bank$loan)      # binary variable  [if the person has any personal loan]  (categorical)
                      # * no     * yes    *UNKNOWN (990)


####
table(bank$contact)   # binary variable            (categorical)
                      # * cellular    * telephone   


####
table(bank$month)     # nominal         (categorical)
                      # - c(jan,feb)

####
table(bank$day_of_week) # nominal       (categorical)
                        # -c(sun,sat)


##################################
summary(bank$duration)  #numeric continuous   

nb = select(bank,y,duration)

View(nb)

nrow(nb[nb$y=="yes",])

summary(nb[nb$y=="yes",])

summary(nb[nb$y=="no",])

hist(nb$duration[nb$y=="no"])

hist(nb$duration[nb$y=="yes"])


####################################
table(bank$campaign) #  nominal
                     #  no. of contacts during this campaign
                     #

barplot(table(bank$campaign))

length(bank$campaign[bank$campaign<=5])/nrow(bank) # 43% people in campaign 1
                                                   # 68% people in camp 1 & 2
                                                   # 92% within camp 5

bank[bank$campaign>=15,]

###############################

table(bank$pdays) # days passed after contating the person from the pevious campaign
                  # 999 -> not contacted previously
                  # 999 -> 39673


###############################
table(bank$previous) # no. of contacts brfore this campaign foe a single client
                     # ordinal variable
                     # 0 -> 35563 86%


#######################
table(bank$poutcome) # outcome for previous campaigns
                     # ordinal
                     # * failure     * nonexistent (86%)    * success

p = chisq.test(factor(bank$poutcome),bank$y)
p$p.value
#######################
summary(bank$emp.var.rate) # emloyment variation rate 
                           # numeric discrete variable


table(bank$emp.var.rate)
barplot(table(bank$emp.var.rate))


##########################

table(bank$cons.price.idx) # numeric discrete variable

barplot(table(bank$cons.price.idx)) # numeric discrete variable)


##############################

table(bank$cons.conf.idx)

barplot(table(bank$cons.conf.idx)) # numeric discrete variable


#####################################

summary(bank$euribor3m)  # numeric continuous variable
hist(bank$euribor3m)

#################################

summary(bank$nr.employed)  # discrete
table(bank$nr.employed)
barplot(table(bank$nr.employed))

##################################

table(bank$y) #categorical binary


########################################################
#assumptions
########################################################

#1) multicollinearity
#2) linearity with log(odds)

############################################################
setwd("C:\\Users\\Rishav\\Desktop")

bank = read.csv("bank-additional-full.csv",sep = ";")



y = factor(bank$y,level = c("no","yes"),labels = c(0,1))  # changing the factors yes and no into 0 and 1


bank$y = y                                                # imputing value of y in the dataset

#View(bank)


ch = c()      # creating a function for chi sq test

chi_test = function(variable)     # functio to check chi sq test
{
  c = chisq.test(select(bank,variable),bank$y)
  return(c$p.value)
}


bank2 = names(bank[,-c(1,11,12,13,14,16,17,18,19,20,21)])

for (var in bank2) {
  
  ch = c(ch,chi_test(var))
  
}



category_pval =as.data.frame(ch,row.names = bank2)
print(category_pval)

#chi_test(bank$job)

d = chisq.test(bank$y,bank$month)
d
d$p.value

table(bank$y,bank$month)

276/(270+276)  # mar      50.54%
539/(539+2093) # april    20.47%
886/(886+12883)# may      06.43%
559/(559+4759) # jun      10.51%
649/(649+6525) # jul      09.04%
655/(655+5523) # aug      10.60%
256/(256+314)  # sep      44.91%
315/(315+403)  # oct      43.87%
416/(416+3685) # nov      10.14%
89/(89+93)     # dec      48.90%

###########################################################################
#
###########################################################################




#  JOB , MARR, EDU, DEFAULT , HOUSING, LOAN, CONTACT, DOW, 
table(bank$pdays)
# 

#########################################################################################

t_age = t.test(bank$age~bank$y)
t_cons.conf.idx = t.test(bank$cons.conf.idx~bank$y)
t_cons.conf.idx$p.value
cont_pval = as.data.frame(c(t_cons.conf.idx$p.value,t_age$p.value),row.names = c("cons.conf.idx",'age'))
print(cont_pval)


############################################################################################