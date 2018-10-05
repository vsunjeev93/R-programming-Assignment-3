rankall<-function(out,num='best'){outcome<-read.csv('outcome-of-care-measures.csv',colClasses='character')
sf<-split(outcome,outcome$State)
# function to order lists in ascending order of mortality rate

order_ha<-function(s){f<-as.data.frame(s)
  ord_out1<-f[order(as.numeric(f$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),f$Hospital.Name),]
  ord_out<-ord_out1[!is.na(as.numeric(ord_out1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
}
order_hf<-function(s){f<-as.data.frame(s)
  ord_out1<-f[order(as.numeric(f$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),f$Hospital.Name),]
  ord_out<-ord_out1[!is.na(as.numeric(ord_out1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
}
order_p<-function(s){f<-as.data.frame(s)
  ord_out1<-f[order(as.numeric(f$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),f$Hospital.Name),]
  ord_out<-ord_out1[!is.na(as.numeric(ord_out1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
}
getHospital_best<-function(ordf){hospi<-ordf$Hospital.Name[1]}
getHospital_worst<-function(ordf){hospi<-tail(ordf$Hospital.Name,1)}
getHospital_num<-function(ordf){hospi<-ordf$Hospital.Name[num]}
if(out=='heart attack'){ordered_frame<-lapply(sf,order_ha)
if(num=='best'){k<-lapply(ordered_frame,getHospital_best)}
if(num=='worst'){k<-lapply(ordered_frame,getHospital_worst)}
if(class(num)=='numeric'){k<-lapply(ordered_frame,getHospital_num)}}


if(out=='heart failure'){ordered_frame<-lapply(sf,order_hf)
if(num=='best'){k<-lapply(ordered_frame,getHospital_best)}
if(num=='worst'){k<-lapply(ordered_frame,getHospital_worst)}
if(class(num)=='numeric'){k<-lapply(ordered_frame,getHospital_num)}}


if(out=='pneumonia'){ordered_frame<-lapply(sf,order_p)
if(num=='best'){k<-lapply(ordered_frame,getHospital_best)}
if(num=='worst'){k<-lapply(ordered_frame,getHospital_worst)}
if(class(num)=='numeric'){k<-lapply(ordered_frame,getHospital_num)}}
return (k)
}
