get_OptComb = function(inter ,CP_content, RevScore_df,GroupIndex){
  RevScore_df <-   RevScore_df[,-which(colnames( RevScore_df) %in% unique(GroupIndex$group_name[which(GroupIndex$Pos_Treat == "Pos")]))]
  wanted_df = data.frame(
    Names = c(unique(GroupIndex$group_name[which(GroupIndex $Wanted == 1)]),
              unique(GroupIndex$group_name[which(GroupIndex $Wanted == 2)])
    ),
    Level = c(rep(1,length(unique(GroupIndex$group_name[which(GroupIndex $Wanted == 1)]))),
              rep(2,length(unique(GroupIndex$group_name[which(GroupIndex $Wanted == 2)])))
    )
  )
  CP_content$ChineseName = make.names(  CP_content$ChineseName )
  Comb_Rev  <-  RevScore_df[,which(colnames(RevScore_df) == unique(GroupIndex$group_name[GroupIndex$Pos_Treat == "Comb"]))]
  assign("Comb_Rev", Comb_Rev ,envir = .GlobalEnv)
  # cat("Comb_Rev Done \n")
  Pos3_Inter = inter[,1]
  assign("Pos3_Inter", Pos3_Inter ,envir = .GlobalEnv)
  set.seed(2024)
  # cat("Begin-Opt\n")
  Opt.Comb.content.want.Tot<-mco::nsga2(Opt.Comb.content.want.Tot.fun  ,
                                        idim=ncol(RevScore_df), odim=3,
                                        generations=10, popsize=100,
                                        lower.bounds = c(rep(0,
                                                             ncol(RevScore_df)
                                        )),
                                        upper.bounds =  c(rep(1,
                                                              ncol(RevScore_df)
                                        )))
  # cat("Done-Opt\n")
  df_solu = do.call(rbind,lapply(1:100, function(i) Sloution_readable(Opt.Comb.content.want.Tot$par[i,],CP_content)))
  # cat("Explain\n")
  df_solu$Sim = Opt.Comb.content.want.Tot$value[,2]
  df_solu$WantedScore = Opt.Comb.content.want.Tot$value[,1]
  df_solu$AveContent = Opt.Comb.content.want.Tot$value[,3]
  df_solu$pareto.optimal = Opt.Comb.content.want.Tot$pareto.optimal
  return(df_solu=df_solu)
}
