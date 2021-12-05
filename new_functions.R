
#######################################
#######################################
# Group Frequency table (Univariate)
#######################################
#######################################


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
    per  = c(per,round(len/length(var)*100,4))
  }
  tab = as.data.frame(cbind(name,freq,per)) 
  return(tab)
}
#gr_freq_uni(bank$age,15,15)





###############################################
###############################################
# Group Frequency Table (Bivariate)
###############################################
###############################################


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
    tot_len = length(bank$age[var >= start & bank$age < end])
    group = c(group,paste(start,end,sep = "-"))
    start = end
    freq_yes = c(freq_yes,len1)
    freq_no  = c(freq_no,len2)
    per_yes  = c(per_yes,round((len1/tot_len)*100,4))
    per_no  = c(per_no,round((len2/tot_len)*100,4))
  }
  tab = data.frame(freq_yes,freq_no,per_yes,per_no,row.names = group) 
  return(tab)
}






############################################
############################################
# Proportion Table (Bivariate)
############################################
############################################



propr = function(var){
  t=table(bank$y,var)
  
  n=names(t[1,])
  n1=c(names(t[,2]),"Total","tot_y","tot_n")
  part=matrix(NA,ncol=length(n1),nrow = length(n),dimnames =list(n,n1))
  for(j in n){
    part[j,"yes"]  = round(t[2,j]*100/sum(t[1:2,j]),2)
    part[j,"no"]   = round(t[1,j]*100/sum(t[1:2,j]),2)
    part[j,"Total"]= sum(t[1:2,j])
    part[j,"tot_y"]= t[2,j]
    part[j,"tot_n"]= t[1,j]
  }
  return(as.data.frame(part))
}






################################################
################################################
# Weight of Evidence (categorical Variable)
################################################
################################################


woe_cat = function(var){
  t = table(bank$y,var)
  
  t_no = length(bank$y[bank$y=="no"])
  
  t_yes = length(bank$y[bank$y=="yes"])
  
  woe = c()
  per_good = c()
  per_bad = c()
  IV = c()
  for (i in 1:ncol(t)) {
    per_good = c(per_good,round((t[2,i]/t_yes),4))
    per_bad = c(per_bad,round((t[1,i]/t_no),4))
    p=ifelse(per_good[i] > 0 & per_bad[i] > 0,log((t[2,i]/t_yes)/(t[1,i]/t_no)),0 )
    woe=c(woe,p)
    IV = c(IV,round((per_good[i]-per_bad[i])*woe[i],4))
  }
  woe2 = data.frame(per_good,per_bad,woe,IV,row.names = names(t[1,]))
  return(list(IV_table = woe2,IV=sum(IV)))
  
}
#ifelse()
#woe_cat(bank$education)

###############################################
###############################################
# WOE (numeric variable)
###############################################
###############################################

# Run bivariate table fn first

woe_num = function(var,start,interval){
  t = gr_freq_bi(var,start,interval)
  
  t_no = length(bank$y[bank$y=="no"])
  
  t_yes = length(bank$y[bank$y=="yes"])
  per_good = c()
  per_bad = c()
  woe = c()
  IV = c()
  for (i in 1:nrow(t)) {
    per_good = c(per_good,round((t[i,1]/t_yes),4))
    per_bad = c(per_bad,round((t[i,2]/t_no),4))
    p=ifelse(per_good[i] > 0 & per_bad[i] > 0,round(log((t[i,1]/t_yes)/(t[i,2]/t_no)),4),0)
    woe=c(woe,p)
    IV = c(IV,round((per_good[i]-per_bad[i])*woe[i],4))
  }
  woe2 = data.frame(per_good,per_bad,woe,IV,row.names = row.names(t))
  return(list(IV_table=woe2,IV = sum(IV)))
  
}




n=c()
iv=c()
for(i in names(bank[,-21])){
  if(typeof(bank[,i])=="character"){
    w=woe_cat(bank[,i])
    n=c(n,i)
    iv=c(iv,w$IV)
  }
}

iv = data.frame(iv,row.names = n)
iv
n
iv=iv$iv
names(bank)
bank[,"job"]
table(bank$pdays)
summary(bank$cons.price.idx)
(we=woe_num(bank$cons.price.idx,92.2,.2))



iv=c(iv,we$IV)
n=c(n,"emp.var.rate")
iv
woe_cat(bank$cons.price.idx)
table(bank$cons.price.idx)
