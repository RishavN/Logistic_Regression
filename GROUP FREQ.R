
gr_freq_uni = function(var,start,interval)
{
  freq = c()
  name = c()
  per  = c()
  end  = 0
  while(end<max(var)){
    
    end = start + interval
    len = length(var[var>=start & var<end])
    #tot_len = length(bank$age[var >= start & bank$age < end])
    name = c(name,paste(start,end,sep = "-"))
    start = end
    freq = c(freq,len)
    per  = c(per,round(len/length(var)*100,4))#tot_len*100,4))
  }
  tab = as.data.frame(cbind(name,freq,per)) 
  return(tab)
}

gr_freq_uni(bank$age,10,10)



####################################################################################################
#...................................................................................................
#///////////////////////////////////////////////////////////////////////////////////////////////////
#...................................................................................................
####################################################################################################


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
  tab = as.data.frame(cbind(group,freq_yes,freq_no,per_yes,per_no)) 
  return(tab)
}

gr_freq_bi(bank$age,10,10)




########################################################################################
#
#########################################################################################

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

table(bank$y,bank$job)
propr(bank$job)
