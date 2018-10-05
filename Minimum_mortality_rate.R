best<-function(state,out){
  outcome<-read.csv('outcome-of-care-measures.csv',colClasses='character')
  if (out=='heart attack'){
    keyword='Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    state_outcome=subset(outcome,State==state)
    min_outcome=min(na.omit(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    hospi_in_state<-subset(state_outcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min_outcome)
    hospital<- (min(hospi_in_state$Hospital.Name))
    } else if (out== 'pneumonia'){
   keyword='Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
   state_outcome=subset(outcome,State==state)
   min_outcome=min(na.omit(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
   hospi_in_state<-subset(state_outcome,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min_outcome)
   hospital<-min(hospi_in_state$Hospital.Name)} else if(out=='heart failure'){
   keyword='Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
   state_outcome=subset(outcome,State==state)
   min_outcome=min(na.omit(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
   hospi_in_state<-subset(state_outcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min_outcome)
   hospital<- min(hospi_in_state$Hospital.Name)}
  
  if(max(outcome$State==state)==0){stop("invalid state")}
  if(out!='heart attack' & out!='pneumonia' & out!='heart failure'){stop('invalid outcome')}
  return (hospital)
    
    
}