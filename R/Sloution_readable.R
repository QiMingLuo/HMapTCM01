Sloution_readable<-function(x,CP_content){
  x=ifelse(x<0.5,0,1)
  Gp_index    =  Global_data3
  RevScore_df =  DatRevScore()
  # print(dim(RevScore_df))
  RevScore_df =  RevScore_df[,which(colnames(RevScore_df) %in% unique( Gp_index[which( Gp_index$Save_Comb==1),] $group_name))]
  # print(dim(RevScore_df))
  Sav_HODC = na.omit(colnames(RevScore_df )[which(x ==1)])
  # print(paste0 (Sav_HODC,"_","/n"))
  # Sav_HODC = na.omit(colnames(revrate_df_p )[which(x ==1)])
  SumContent = sum(CP_content[which(CP_content$ChineseName %in% Sav_HODC) , 2] )
  CompName = paste0( Sav_HODC[order(CP_content[which(CP_content$ChineseName %in% Sav_HODC),2])],collapse = "_")
  # print(    CompName)
  df = data.frame(CompName = CompName,SumContent = SumContent,CompNum = sum(x))
  return ( df)
}
