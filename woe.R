# WEIGHT OF EVIDENCE


t = table(bank$y,bank$job)

t_no = length(bank$y[bank$y=="no"])

t_yes = length(bank$y[bank$y=="yes"])

woe = c()
for (i in 1:ncol(t)) {
  woe = c(woe,log((t[2,i]/t_yes)/(t[1,i]/t_no)))
}
woe

table(bank$job)





woe_cat = function(var){
  t = table(bank$y,var)
  
  t_no = length(bank$y[bank$y=="no"])
  
  t_yes = length(bank$y[bank$y=="yes"])
  
  woe = c()
  for (i in 1:ncol(t)) {
    woe = c(woe,round(log((t[2,i]/t_yes)/(t[1,i]/t_no)),4))
  }
  woe2 = as.data.frame(cbind(names(t[1,]),woe))
  return(woe2)
  
}
woe_cat(bank$job)
woe_cat(bank$marital)





gr_freq_bi2 = function(var,start,interval)
{
  freq_yes = c()
  freq_no = c()
  group = c()
  end  = 0
  while(end<max(var)){
    
    end = start + interval
    len1 = length(var[var>=start & var<end & bank$y == "yes"])
    len2 = length(var[var>=start & var<end & bank$y == "no"])
    group = c(group,paste(start,end,sep = "-"))
    start = end
    freq_yes = c(freq_yes,len1)
    freq_no  = c(freq_no,len2)
    
  }
  tab =data.frame(group,freq_yes,freq_no) 
  return(tab)
}



woe_num = function(var,start,interval){
  t = gr_freq_bi2(var,start,interval)
  
  t_no = length(bank$y[bank$y=="no"])
  
  t_yes = length(bank$y[bank$y=="yes"])
  
  woe = c()
  for (i in 1:nrow(t)) {
    woe = c(woe,round(log((t[i,2]/t_yes)/(t[i,3]/t_no)),4))
  }
  woe2 = data.frame(woe,row.names = t[,1])
  return(woe2)
  
}

