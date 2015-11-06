best<-function(state,cond){
  
  con<-file("C:/Users/Sanjay/Desktop/Data Science/hosdata/outcome-of-care-measures.csv","r")
  outcome<-read.csv(con,colClasses = "character")
  
  flag=FALSE
  for(i in outcome$State)
  {
    if(i==state)
    {
      flag=TRUE
      break
    }
  }
  if(flag==FALSE)
    stop("invalid state")
  
  
 
  x<-switch(cond,"heart attack"=outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
            "heart failure"=outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
            "pneumonia"=outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,stop("invalid outcome"))
         
  hname<-vector()
  hstat<-vector()
  
  for(i in 1:4706)
  {
    if(state==outcome$State[i] && x[i]!="Not Available" && is.na(x[i])==FALSE)
    {    hname<-c(hname,outcome$Hospital.Name[i])
         hstat<-c(hstat,x[i])      
    }  
    
  }
 nhstat<-as.numeric(hstat)  
 m<-min(nhstat)
 good<-nhstat==m
 hn<-sort(hname[good])
 hn[1]
}
