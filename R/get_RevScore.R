
get_RevScore = function(Batch_limma_HTC,GroupIndex){
  group_list = GroupIndex$group_name
  Index_Treat = unique(group_list )[-which(unique(group_list ) %in% c("C","M"))]
  par_CM = Batch_limma_HTC
  par_CM  = par_CM [,which(group_list %in% c("C","M"))]
  group_CM_2 <- as.factor(group_list[which(group_list %in% c("C","M"))])
  design_CM_2 <- model.matrix(~0+group_CM_2)
  colnames(design_CM_2) = levels(factor(group_CM_2))
  rownames(design_CM_2) = colnames(group_CM_2)
  norm_CM_2 <- voom(par_CM , design_CM_2, plot = TRUE)
  fit <- lmFit(norm_CM_2, design_CM_2, method = 'ls')
  contrast <- makeContrasts('C-M', levels = design_CM_2)
  fit2 <- contrasts.fit(fit, contrast);fit2 <- eBayes(fit2)
  diff_C_M <- topTable(fit2, number = Inf, adjust.method = 'fdr')
  diff_C_M= diff_C_M[order( rownames(diff_C_M)),]

  temp_FC_list = list()
  for (i in 1:length(Index_Treat)) {
    par_MT  = Batch_limma_HTC [,which(group_list %in% c("M",Index_Treat[i]))]
    par_MT [  par_MT <0]  = 0
    colnames(  par_MT ) =  make.names(colnames(  par_MT ))
    group_MT_2 <- as.factor(make.names(group_list[which(group_list %in% c("M",Index_Treat[i]))])
    )
    design_MT_2 <- model.matrix(~0+group_MT_2)
    colnames(design_MT_2) = levels(factor(group_MT_2))
    rownames(design_MT_2) = colnames(group_MT_2)
    norm_MT_2 <- voom(par_MT , design_MT_2, plot = F)
    fit <- lmFit(norm_MT_2, design_MT_2, method = 'ls')
    g_con = paste0("M-",make.names(Index_Treat[i]))
    contrast <- makeContrasts(  contrasts=g_con, levels = design_MT_2)
    fit2 <- contrasts.fit(fit, contrast);fit2 <- eBayes(fit2)
    diff_MT <- topTable(fit2, number = Inf, adjust.method = 'fdr')
    diff_MT= diff_MT[order( rownames( diff_MT)),]
    temp_FC = diff_MT$logFC
    temp_FC_list[[i]] =   temp_FC
    cat(paste0("This is ",i," of ",length(Index_Treat),"\n"))
    i = i+1
  }
  FC_df = do.call(cbind,temp_FC_list)
  colnames(FC_df ) = Index_Treat
  rownames(FC_df ) =  rownames( diff_MT)
  RevScore_df = -FC_df /diff_C_M $logFC
  RevScore_df  = as.data.frame(RevScore_df )
  return( RevScore_df = RevScore_df )
}
