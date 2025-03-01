SimDat_analyser<-function(simdat){
  
  #simdat<-Results[[1]]$etable_Pad_result
  
  Loadstep<-unique(simdat$Loadstep)
  timesteps<-unique(simdat$Time)
  TLStxt<-paste("T",simdat$Time,"L",simdat$Loadstep,"S",simdat$Substep,sep = "")
  tlsdataframe<-data.frame(TLStxt=TLStxt
                           ,T=simdat$Time
                           ,Temp=simdat$Temp
                           ,L=simdat$Loadstep
                           ,S=simdat$Substep)
  maxsubsteps<-NULL
  maxsubstepsIndex<-NULL
  
  LS_Min<-min(Loadstep)
  LS_Max<-max(Loadstep)
  
  counter<-0
  for (i in Loadstep){
    counter<-counter+1
    sub1<-(subset(tlsdataframe, L==i))
    dimmi<-dim(sub1)[1]
    maxsubsteps[counter]<-sub1[dimmi,]$S
    maxsubstepsIndex[counter]<-which(tlsdataframe$L==i & tlsdataframe$S==maxsubsteps[counter])[1]
  }
  SimTimes_wo_Substeps<-tlsdataframe$T[maxsubstepsIndex]
  SimTimes<-as.numeric(unique(simdat$Time))
  
  ret<-list(SimTimes_wo_Substeps=SimTimes_wo_Substeps
            ,SimTimes=SimTimes
            ,tlsdataframe=tlsdataframe
            ,Loadsteps=Loadstep
            ,maxsubsteps=maxsubsteps
            ,maxsubstepsIndex=maxsubstepsIndex
            ,timesteps=timesteps
  )
  return(ret)
}
