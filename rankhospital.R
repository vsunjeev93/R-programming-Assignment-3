rankhospital<-function(state,out,num='best'){
  if(max(outcome$State==state)==0){stop("invalid state")}
  if(out!='heart attack' & out!='pneumonia' & out!='heart failure'){stop('invalid outcome')}
  outcome<-read.csv('outcome-of-care-measures.csv',colClasses='character')
  state_outcome=subset(outcome,State==state)
  if (out=='heart attack'){ord_out1<-state_outcome[order(as.numeric(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_outcome$Hospital.Name),]
  ord_out<-ord_out1[!is.na(as.numeric(ord_out$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
  if (num=='best'){k<-ord_out$Hospital.Name[1]}
  if (num=='worst'){k<-tail(ord_out$Hospital.Name,1)}
  if (class(num)=='numeric'){k<-ord_out$Hospital.Name[num]}
  }
  if (out=='heart failure'){ord_out1<-state_outcome[order(as.numeric(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),state_outcome$Hospital.Name),]
  ord_out<-ord_out1[!is.na(as.numeric(ord_out$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
  if (num=='best'){k<-ord_out$Hospital.Name[1]}
  if (num=='worst'){k<-tail(ord_out$Hospital.Name,1)}
  if (class(num)=='numeric'){k<-ord_out$Hospital.Name[num]}
  }
  if (out=='pneumonia'){ord_out1<-state_outcome[order(as.numeric(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),state_outcome$Hospital.Name),]
  ord_out<-ord_out1[!is.na(as.numeric(ord_out$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
  if (num=='best'){k<-ord_out$Hospital.Name[1]}
  if (num=='worst'){k<-tail(ord_out$Hospital.Name,1)}
  if (class(num)=='numeric'){k<-ord_out$Hospital.Name[num]}
  }
  return (k)
  }