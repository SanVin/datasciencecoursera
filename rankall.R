rankall<-function(cond,num="best")
{
  con<-file("C:/Users/Sanjay/Desktop/Data Science/hosdata/outcome-of-care-measures.csv","r")
  outcome<-read.csv(con,colClasses = "character")
  x<-switch(cond,"heart attack"=outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
            "heart failure"=outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
            "pneumonia"=outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,stop("invalid outcome"))
  
  state<-outcome$State
  state<-unique(sort(state))
  hospital<-vector()
  
  
  for(i in state)
  {
    hname<-vector()
    hstat<-vector()
    
    for(j in 1:4706)
    {
      if(i==outcome$State[j] && x[j]!="Not Available" && is.na(x[j])==FALSE)
      {    hname<-c(hname,outcome$Hospital.Name[j])
           hstat<-c(hstat,x[j])      
      }  
      
    }
    hnameind<-order(hname)
    hn<-hname[hnameind]
    hs<-hstat[hnameind]
    hsind<-order(as.numeric(hs))
    hs1<-hs[hsind]
    hn1<-hn[hsind]
    if(num=="best")
    hospital<-c(hospital,hn1[1])
    
    else if(num=="worst")
      hospital<-c(hospital,tail(hn1,n=1))
    
    else if(num<=as.numeric(length(hn1)))
      hospital<-c(hospital,hn1[num])
    
    else
      hospital<-c(hospital,"<NA>")
    
  }
  df<-data.frame(hospital,state)
  df
  
}
