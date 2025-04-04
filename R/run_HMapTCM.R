run_HMapTCM <- function(){
  shiny::shinyApp(ui = dashboardPage(
    dashboardHeader(title = "高内涵优化组方系统"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("去除批次效应", tabName = "BATHTC", icon = icon("graduation-cap")),
        menuItem("计算逆转分数", tabName = "RevSco", icon = icon("grav")),
        menuItem("智能组方优化", tabName = "CombOpt", icon = icon("th")),
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search...")
      )
    ),

    dashboardBody(
      tabItems(
        tabItem(tabName = "BATHTC",
                h2("根据实验批次与分组信息对原始数据进行去批次化"),
                fileInput("BAT","upload Batch Info xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUIBAT"),

                fileInput("HTC","upload High Content data xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUIHTC"),
                tableOutput("RemHTC"),
                downloadButton("download", "Download Batcheffect removed table.xlsx")
        ),
        tabItem(tabName = "RevSco",
                h2("根据组别计算变化倍数并形成逆转分数，同时提取交集"),
                fileInput("Group_Index","upload Group_Index xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUIGroup_Index"),
                fileInput("NewHTC","upload removed batch effect High Content data xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUINewHTC"),
                tableOutput("previewDatNewHTC"),
                tableOutput("previewRevSco_Out"),
                tableOutput("previewPosInter_Out"),
                downloadButton("downloadRevSco", "Download Reverse Score table.xlsx"),
                downloadButton("downloadPosInter", "Download Pos Interact table.xlsx")

        ),
        # Second tab content
        tabItem(tabName = "CombOpt",
                h2("预测组合"),
                fileInput("Inter","upload Pos Interact  xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUIInter"),
                fileInput("CPcontent","upload CPcontent xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUICPcontent"),
                fileInput("RevScore","upload RevScore xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUIRevScore"),
                fileInput("Group_Index_New","upload Group_Index xlsx file",accept = ".xlsx"),
                uiOutput("dropdownUIGroup_Index_New"),
                tableOutput("previewComb_Out"),
                downloadButton("downloadComb", "Download Predicted Combination table.xlsx"),
                tableOutput("previewDatCPcontent ")


        )
      )
    )
  ),
  server = function(input, output,session) {




    ###########################################
    WorkbookRevScore <- eventReactive(input$RevScore, {
      loadWorkbook(input$RevScore$datapath)
    })
    SheetsRevScore <- eventReactive(WorkbookRevScore(), {
      names(WorkbookRevScore())
    })
    output$dropdownUIRevScore <- renderUI({
      req(SheetsRevScore())
      selectInput("sheetRevScore", "Choose a  RevScore  sheet", SheetsRevScore())
    })
    DatRevScore <<- eventReactive(input$sheetRevScore, {
      read.xlsx(WorkbookRevScore(), sheet = input$sheetRevScore,colNames = T,rowNames = T)
    })
    output$previewDatRevScore <-
      renderTable( {DatRevScore()    })

    ###########################################
    WorkbookCPcontent <- eventReactive(input$CPcontent, {
      loadWorkbook(input$CPcontent$datapath)
    })
    SheetsCPcontent <- eventReactive(WorkbookCPcontent(), {
      names(WorkbookCPcontent())
    })
    output$dropdownUICPcontent <- renderUI({
      req(SheetsCPcontent())
      selectInput("sheetCPcontent", "Choose a  CPcontentact  sheet", SheetsCPcontent())
    })

    DatCPcontent <<- eventReactive(input$sheetCPcontent, {
      read.xlsx(WorkbookCPcontent(), sheet = input$sheetCPcontent,colNames = T,rowNames = T)
    })
    # print(head(DatCPcontent))
    output$previewDatCPcontent <-
      renderTable( {DatCPcontent()    })

    ###########################################

    WorkbookInter <- eventReactive(input$Inter, {
      loadWorkbook(input$Inter$datapath)
    })
    SheetsInter <- eventReactive(WorkbookInter(), {
      names(WorkbookInter())
    })
    output$dropdownUIInter <- renderUI({
      req(SheetsInter())
      selectInput("sheetInter", "Choose a  Pos Interact  sheet", SheetsInter())
    })
    DatInter <- eventReactive(input$sheetInter, {
      read.xlsx(WorkbookInter(), sheet = input$sheetInter,colNames = T,rowNames = T)
    })
    output$previewDatInter <-
      renderTable( {DatInter()    })
    ###########################################
    WorkbookGroup_Index_New <- eventReactive(input$Group_Index_New, {
      loadWorkbook(input$Group_Index_New$datapath)
    })
    SheetsGroup_Index_New <- eventReactive(WorkbookGroup_Index_New(), {
      names(WorkbookGroup_Index_New())
    })
    output$dropdownUIGroup_Index_New <- renderUI({
      req(SheetsGroup_Index_New())
      selectInput("sheetGroup_Index_New", "Choose a  Group_Index_New  sheet", SheetsGroup_Index_New())
    })
    DatGroup_Index_New <- eventReactive(input$sheetGroup_Index_New, {
      read.xlsx(WorkbookGroup_Index_New(), sheet = input$sheetGroup_Index_New,colNames = T,rowNames = T)
    })

    DatGroupIndexNew <- eventReactive(input$sheetGroup_Index_New, {
      read.xlsx(WorkbookGroup_Index_New(), sheet = input$sheetGroup_Index_New,colNames = T,rowNames = T)
    })

    output$previewDatGroup_Index_New <-
      renderTable( {DatGroup_Index_New()    })

    ##################################
    # 准备组合优化前数据
    # downloadButton("downloadPreCalculation", "Download Pre-Calculation Rda"),
    # fileInput("PreCalculation","upload Pre-Calculation Rda file",accept = ".rda"),
    observeEvent(input$RevScore,{
      # 如果提前存在要清除
      if(exists("Global_data3",envir = .GlobalEnv)){
        rm("Global_data3",envir = .GlobalEnv)
      }
      showNotification("Env Clean")
    }
    )

    observeEvent(DatGroup_Index_New(),{
      assign("Global_data1",DatCPcontent(),envir = .GlobalEnv)
      showNotification("Cpcontent loaded")
      assign("Global_data2",DatRevScore(),envir = .GlobalEnv)
      showNotification("RevScore loaded")
      assign("Global_data3",DatGroupIndexNew(),envir = .GlobalEnv)
      showNotification("DatGroup_Index_New loaded")
    }
    )

    ##################################
    # 计算最优组合
    Comb_Out <- eventReactive(  input$sheetGroup_Index_New, {
      cat("Comb_Start\n")
      Sav_HODC <- DatInter()
      RevScore_df <- DatRevScore()
      RevScore_df$Pheno <- rownames(DatRevScore())
      assign("RevScore_df",RevScore_df)
      CP_content<- DatCPcontent()
      assign("CP_content",CP_content)
      cat("Begin\n")
      Comb_Out <- get_OptComb (DatInter() ,DatCPcontent(), DatRevScore(),DatGroup_Index_New())
      cat("done\n")
      Comb_Out
    })
    output$previewComb_Out <-
      renderTable( {Comb_Out()    })


    output$downloadComb <- downloadHandler(
      filename = function() {
        paste0(input$HTC,"Comb.xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx( as.data.frame(    as.data.frame (Comb_Out ())) , file,rowNames = TRUE)
      }
    )
    ##################################

    ##################################


    WorkbookGroup_Index <- eventReactive(input$Group_Index, {
      loadWorkbook(input$Group_Index$datapath)
    })
    SheetsGroup_Index <- eventReactive(WorkbookGroup_Index(), {
      names(WorkbookGroup_Index())
    })
    output$dropdownUIGroup_Index <- renderUI({
      req(SheetsGroup_Index())
      selectInput("sheetGroup_Index", "Choose a Control And Model sheet", SheetsGroup_Index())
    })
    DatGroup_Index <- eventReactive(input$sheetGroup_Index, {
      read.xlsx(WorkbookGroup_Index(), sheet = input$sheetGroup_Index,colNames = T,rowNames = T)
    })
    output$previewDatGroup_Index <-
      renderTable( {DatGroup_Index()    })

    ###############################################
    WorkbookNewHTC <- eventReactive(input$NewHTC, {
      loadWorkbook(input$NewHTC$datapath)
    })

    SheetsNewHTC <- eventReactive(WorkbookNewHTC(), {
      names(WorkbookNewHTC())
    })

    output$dropdownUINewHTC <- renderUI({
      req(SheetsNewHTC())
      selectInput("sheetNewHTC", "Choose a Control And Model sheet", SheetsNewHTC())
    })

    DatNewHTC <- eventReactive(input$sheetNewHTC, {
      read.xlsx(WorkbookNewHTC(), sheet = input$sheetNewHTC,rowNames = T,colNames = T)
    })

    output$previewDatNewHTC <-
      renderTable( {DatNewHTC()    })

    RevSco_Out <- eventReactive(  input$sheetNewHTC, {
      RevSco_Out <-get_RevScore(DatNewHTC(),DatGroup_Index())
      RevSco_Out
    })

    output$previewRevSco_Out <-
      renderTable( { RevSco_Out()    })

    PosInter_Out <- eventReactive( input$sheetNewHTC, {
      # PosInter_Out <-get_PosInter(DatGroup_Index() ,RevSco_Out())
      Names  <-rownames( RevSco_Out() )
      Pos_Treat <-  unique(DatGroup_Index() $group_name[which(DatGroup_Index() $Pos_Treat == "Pos")])
      Pos_df  <- RevSco_Out() [,which(colnames( RevSco_Out()  ) %in% Pos_Treat)]
      Pos_l   <-  lapply(1:length(Pos_Treat), function(i) t(ifelse(Pos_df[,i]>0,1,0 )))
      Pos_01  <-base::do.call( "rbind",args= Pos_l)
      # length(rownames( RevSco_Out() ))
      # nrow(Pos_01 )
      # lapply(1:nrow(Pos_01 ), function(i) {rownames( RevSco_Out() )[which(Pos_01 [i,] == 1)]})
      Pos_rev_l <- vector("list", nrow(Pos_01 ))
      # lapply(1:nrow(Pos_01 ), function(i) Pos_rev_l[[i]] <- as.vector( Names[which(Pos_01 [i,] == 1)]))
      # 加进去不行，就删除试试

      for (i in 1:nrow(Pos_01 )) {
        Pos_rev_l[[i]] <- as.vector( Names)
        Pos_rev_l[[i]] [which(Pos_01 [i,] == 0)] <-NA
        # Pos_rev_l[[i]] [which(Pos_01 [i,] == 0)] <- NULL
        # 只导出一个是可以的
      }
      Pos_rev_inter =  na.omit(Pos_rev_l[[1]] )
      for (i in 2:nrow(Pos_01 )) {
        Pos_rev_inter = intersect( Pos_rev_inter,na.omit(Pos_rev_l[[i]]))
        # Pos_rev_l[[i]] <- as.vector( Names)

        # 只导出一个是可以的
      }
      Pos_rev_inter
      # Pos_rev_l = na.omit(Pos_rev_l )
      # <- Pos_rev_l[!is.na(Pos_rev_l)]
      # inter <- VennDiagram::get.venn.partitions(x= Pos_rev_l, force.unique = T )
      # default method not implemented for type 'list'
      # for (i in 1:nrow(inter))  {inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')}
      # inter
      # 123\=]

      # inter <- VennDiagram::get.venn.partitions(Pos_rev_l )
      # Pos_rev_l
      # 这一句有问题 <Anonymous>: 参数值意味着不同的行数: 122, 116, 115
      # rowSums( Pos_01)这里有问题
      # Pos_rev  <- lapply(1:nrow(Pos_01 ), function(i) rownames( RevSco_Out() )[which(Pos_01 [i,] == 1)])
      # Pos_rev
    })
    output$previewPosInter_Out <-
      renderTable( { PosInter_Out()    })

    # "downloadRevSco"
    output$downloadRevSco <- downloadHandler(
      filename = function() {
        paste0(input$HTC,"-  RevSco_Out .xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx( as.data.frame(   RevSco_Out ()) , file,rowNames = TRUE)
      }
    )

    output$downloadPosInter <- downloadHandler(
      filename = function() {
        paste0(input$HTC,"-  PosInter_Out .xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx( as.data.frame(    as.data.frame (PosInter_Out ())) , file,rowNames = TRUE)
      }
    )

    #####################################################
    WorkbookBAT <- eventReactive(input$BAT, {
      loadWorkbook(input$BAT$datapath)
    })

    SheetsBAT <- eventReactive(WorkbookBAT(), {
      names(WorkbookBAT())
    })

    output$dropdownUIBAT <- renderUI({
      req(SheetsBAT())
      selectInput("sheetBAT", "Choose a Batch Infor sheet", SheetsBAT())
    })

    DatBAT <- eventReactive(input$sheetBAT, {
      read.xlsx(WorkbookBAT(), sheet = input$sheetBAT)
    })

    T1BAT <- eventReactive(input$sheetBAT, {
      df = as.data.frame(table( DatBAT ()[,1]))
      colnames( df) = c("Date-CellWell","Rep"  )
      df
    })


    T2BAT <- eventReactive(input$sheetBAT, {
      df = as.data.frame(table( DatBAT ()[,2]))
      colnames( df) = c("Group","Rep"  )
      df
    })
    # colnames(  T2BAT()) = c("GroupInfor","Rep"  )

    output$previewT1BAT <-
      renderTable( {T1BAT()    })
    output$previewT2BAT <-
      renderTable( {T2BAT()    })


    # design <- model.matrix(~DatBAT()[,2])
    # batch  <- DatBAT()[,1]
    # BAT_design是有的
    BAT_design <- eventReactive(input$sheetBAT, {
      model.matrix(~DatBAT()[,2])
      # DatBAT()[,1]
    })
    output$BAT_design <-
      renderTable( {
        # reactive(
        BAT_design()
        # )
      })

    # batch可以不用额外弄
    # BAT_batch <- eventReactive(input$sheetBAT, {
    #   df = DatBAT()[,1]
    #   df
    # })

    # 致谢代码来自https://cloud.tencent.com/developer/ask/sof/537030
    WorkbookHTC <- eventReactive(input$HTC, {
      loadWorkbook(input$HTC$datapath)
    })

    SheetsHTC <- eventReactive(WorkbookHTC(), {
      names(WorkbookHTC())
    })

    output$dropdownUIHTC <- renderUI({
      req(SheetsHTC())
      selectInput("sheetHTC", "Choose a  High Content data sheet", SheetsHTC())
    })

    DatHTC <- eventReactive(input$sheetHTC, {
      read.xlsx(WorkbookHTC(), sheet = input$sheetHTC,rowNames = T)
    })
    #
    # output$contents <- renderTable({
    #   req(Dat())
    #   Dat()
    # })
    PreHTC <- eventReactive(input$sheetHTC, {
      head(  DatHTC ())
    })
    output$files <- renderTable(input$HTC)
    output$Group <- renderTable(input$Group)


    RemHTC <- eventReactive(  input$sheetHTC, {
      # head(  DatHTC ())
      #
      # design <- model.matrix(~DatBAT()[,2])
      # batch  <- DatBAT()[,1]
      # RemHTC <- limma::removeBatchEffect(DatHTC(),batch = batch,design = design)
      # RemHTC

      # design <- model.matrix(~DatBAT()[,2])
      # batch  <- DatBAT()[,1]
      # design
      RemHTC <- limma::removeBatchEffect(DatHTC(),batch = as.vector( DatBAT()[,1]) ,design = model.matrix(~DatBAT()[,2]))
      RemHTC
    })
    ################################################

    # RevScore_df =get_RevScore(Batch_limma_HTC,GroupIndex)
    # inter= get_PosInter (GroupIndex,RevScore_df )
    # df_solu = get_OptComb (inter ,CP_content, RevScore_df,GroupIndex)
    #

    # observeEvent(RemHTC(), {
    #   message("Please Wait Calculation is done")
    # })
    #
    # 看看这个运算没有
    output$RemHTC<-
      renderTable( {
        # reactive(
        RemHTC()
        # )
      })
    output$previewHTC <-
      renderTable( {
        # reactive(
        PreHTC()
        # )
      })
    # design <- limma::model.matrix(~DatBAT()[,2])
    # batch  <- DatBAT()[,1]
    # DatHTC()
    #"D:/Experiments/2024-chenling/R-output/240729-limma-HTC.xlsx"
    # Batch_limma_HTC <- removeBatchEffect(BatchEffect_HTC,batch = batch,design = design)

    # =
    # output$preview <- renderTable({
    #   head(Dat())
    # })
    # output$preview <-
    #   renderTable( {
    #     eventReactive(input$sheetHTC, {
    #     head(DatHTC())
    #     }
    #     )
    #     })
    # 预览还没搞好，不加eventReactive，有报错，加了之后直接不显示了

    # data <- reactive({
    #   req(input$file)
    #
    # })
    # output$download <- downloadHandler(
    #   filename = function() {
    #     paste0(input$dataset, ".xlsx")
    #   },
    #   content = function(file) {
    #     openxlsx::write.xlsx(data(), file)
    #   }
    # )

    # output$head <- renderTable({
    #   head(input$HTC)
    # })

    # Error in as.data.frame.default: cannot coerce class ‘"packageIQR"’ to a data.frame
    output$download <- downloadHandler(
      filename = function() {
        paste0(input$HTC,"-changed.xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx( as.data.frame( RemHTC()) , file,rowNames = TRUE)
      }
    )

    # output$distPlot <- renderPlot({
    #   # generate bins based on input$bins from ui.R
    #   x    <- faithful[, 2]
    #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    #   # draw the histogram with the specified number of bins
    #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
  }
  )
}
