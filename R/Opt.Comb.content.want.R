Opt.Comb.content.want.Tot.fun<-function(x ){
  y <- numeric(3)
  x=ifelse(x<0.5,0,1)
  Gp_index    =  Global_data3
  c1          =  unique(Gp_index[which(Gp_index$Wanted == 1),]$group_name)
  c2          =  unique(Gp_index[which(Gp_index$Wanted == 2),]$group_name)
  CP_content  =  DatCPcontent()
  RevScore_df =  DatRevScore()
  RevScore_df =  RevScore_df[,which(colnames(RevScore_df) %in% unique( Gp_index[which( Gp_index$Save_Comb==1),] $group_name))]
  # print(dim(RevScore_df ))
  # print(colnames( RevScore_df))
  revrate_df =   RevScore_df
  revrate_df$Pheno  = rownames(RevScore_df)
  Sav_HODC = make.names( na.omit(colnames(RevScore_df )[which(x ==1)]))
  revrate_df_sav = RevScore_df [,
                                which(colnames(RevScore_df ) %in% Sav_HODC )]
  if((!nrow(revrate_df_sav) == 0) | is.null(revrate_df_sav)) {
    revrate_df_sav = as.data.frame(revrate_df_sav[,order(colnames(revrate_df_sav))])
    # print(paste0(colnames(revrate_df_sav),"_"))
    df_sav  = data.frame(ChineseName = colnames( revrate_df_sav ))
    contnet_df = CP_content[which(CP_content$ChineseName %in% Sav_HODC) , ]
    contnet_df = contnet_df[order( contnet_df$ChineseName),]
    df_sav$"ChineseName" =make.names(df_sav$"ChineseName")
    df_sav_content = dplyr::left_join(df_sav,contnet_df,by = "ChineseName")
    df_sav_content[is.na(df_sav_content)] = min(contnet_df$`含量（%）`)
    df_ok = t(revrate_df_sav) * ( df_sav_content $`含量（%）`/100)
    revrate_df_sav$Sum = rowSums( t(df_ok), na.rm = T)
    SumContent = sum(CP_content[which(CP_content$ChineseName %in% Sav_HODC) , 2] )
    wanted_num = sum(ifelse(Sav_HODC%in% c1,1,0)) + 0.3* sum(ifelse(Sav_HODC%in% c2,1,0))
    y[1]  = -wanted_num
    # print(y[1])
    revrate_df$Pheno = rownames(revrate_df_sav)
    y[2]  = factoextra::get_dist(rbind(
      Comb_Rev [which(revrate_df$Pheno %in% Pos3_Inter )],
      revrate_df_sav[which(revrate_df$Pheno %in% Pos3_Inter ),]$Sum), method = "spearman")
    # print(y[2])
    y[3]  = -SumContent/sum(x)
  }else{
    y[1] = 0
    y[2]  =  10
    y[3]  = 0
  }
  return (y)
}
