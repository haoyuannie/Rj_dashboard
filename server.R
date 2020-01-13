

server <- function(input, output, session) {
  
  setwd("C:/R_Shiny_Dashboard/DB_0106")
  source("./Spatial_Reproductive_Number.R", local=TRUE)
  
  # test start  ------------------------------------------------------------------
  
  # output$contents = renderTable({
  #   # input$file1 will be NULL initially. After the user selects
  #   # and uploads a file, head of that data file by default,
  #   # or all rows if selected, will be shown.
  #   
  #   req(input$csv)
  #   
  #   # when reading semicolon separated files,
  #   # having a comma separator causes `read.csv` to error
  #   tryCatch(
  #     {
  #       df <- read.csv(input$csv$datapath,
  #                      header = input$header,
  #                      sep = input$sep,
  #                      quote = input$quote)
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  #   
  #   if(input$disp == "head") {
  #     return(head(df))
  #   }
  #   else {
  #     return(df)
  #   }
  #   
  # })
  
  
  # output$textt <- renderText({
  #   
  #   # input$file1 will be NULL initially. After the user selects
  #   # and uploads a file, head of that data file by default,
  #   # or all rows if selected, will be shown.
  #   
  #   req(input$csv)
  #   
  #   # when reading semicolon separated files,
  #   # having a comma separator causes `read.csv` to error
  #   tryCatch(
  #     {
  #       df <- read.csv(input$csv$datapath,
  #                      header = input$header,
  #                      sep = input$sep,
  #                      quote = input$quote)
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  #   
  #   as.character(nrow(df))
  # })  
  
  # test end ------------------------------------------------------------
  
  
  # 1. Descriptive ----------------------------------------
  # ___1.1 Total count ----------------------------------------------------------
  output$disease_count = renderInfoBox({
    
    source("./read_file.R", local = TRUE)
    
    valueBox(as.character(nrow(df)),
             "Total count", 
             icon = icon("street-view"), color = "yellow")
  })
  
  # ___1.2 Epi_curve ----------------------------------------------------------------------------

  output$epi_curve = renderPlot({
    
    source("./read_file.R", local = TRUE)

    # Plotting
    plot.epi(t = df$date)

  })
  
  
  # ___1.3 Spatial pattern ----------------------------------------------------------------------
  
  output$Sp_point=  renderPlot({
    
    source("./read_file.R", local = TRUE)
    
    # Plotting
    base <- readRDS("./Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    plot.points(x1 = df$long, x2= df$lat, crs_pts = NULL,
                base_map = base, bnd = bnd)
    
  })
  
  output$Sp_hexagon = renderPlot({
    
    source("./read_file.R", local = TRUE)
    
    base <- readRDS("Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    plot.hex(x1 = df$long, x2= df$lat, base_map = base, nbin = input$Sp_hexagon_slider, 
             bnd = bnd)
    
  })
  
  output$Sp_kde = renderPlot({
    
    source("./read_file.R", local = TRUE)
    
    base <- readRDS("Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    plot.kde(x1 = df$long, x2= df$lat, base_map = base, ngrid = input$Sp_kde_slider,
             bnd = bnd)
    
  })
  
  # 2. Rj -------------------------------------------------
  
  # ___ Action --------------------------------------------
  
  #test start -----------------------
  
  # aa = eventReactive(input$rj_go, { #like a function
  #   #1+1
  #   if (input$sp_adjust == TRUE) {
  #     1
  #   } else {
  #     0
  #   }
  #   
  # })
  # 
  # output$action_test = renderPrint({
  #   print(aa())
  # })
  
  #test end ------------------------
  
  
  # ___2.1 Summary of Rj ----------------------------------------
  action_rj_cnt = eventReactive(input$rj_go, {
    
    source("./read_file.R", local = TRUE)
    source("./cal_Rj.R", local = TRUE)
    
    if (input$sp_adjust == TRUE) {
      res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
                         lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
      res_adj$Rj
    } else {
      res <- calc.Rj(t = df$date, 
                     lpdf_GI = lpdf_GI, adj.sp = F)
      res$Rj
      #summary(res) # %>% print()
    }
    
  })
  
  output$rj_q1 = renderText ({
    summary(action_rj_cnt())[2] %>% round(digits = 3) %>% as.character()
  }) 
  
  output$rj_q3 = renderText ({
    summary(action_rj_cnt())[5] %>% round(digits = 3) %>% as.character()
  })
  
  output$rj_mean = renderText ({
    summary(action_rj_cnt())[4] %>% round(digits = 3) %>% as.character()
  })
  
  # HERE ________________________________________________  
  
  output$rj_box_min = renderValueBox({
    
    #source("./read_file.R", local = TRUE)
    
    valueBox(summary(action_rj_cnt())[1] %>% round(digits = 3) %>% as.character(),
             "Min Rj",
             icon = icon("exclamation-circle"), color = "blue")
  })
  
  output$rj_box_max = renderValueBox({
    
    #source("./read_file.R", local = TRUE)
    
    valueBox(summary(action_rj_cnt())[6] %>% round(digits = 3) %>% as.character(),
             "Max Rj",
             icon = icon("exclamation-circle"), color = "red")
  })
  
  output$rj_box_med = renderValueBox({
    
    #source("./read_file.R", local = TRUE)
    
    valueBox(summary(action_rj_cnt())[3] %>% round(digits = 3) %>% as.character(),
             "Median Rj",
             icon = icon("exclamation-circle"), color = "yellow")
  })
  
  
  # ___2.2 Rj_time ------------------------------------------------
  action_rj_time = eventReactive(input$rj_go, {
    
    source("./read_file.R", local = TRUE)
    source("./cal_Rj.R", local = TRUE)
    
    if (input$sp_adjust == TRUE) {
      res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
                         lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
      df$Rj_adj <- res_adj$Rj
      
      res <- calc.Rj(t = df$date,
                     lpdf_GI = lpdf_GI, adj.sp = F)
      df$Rj <- res$Rj
      
      plot.Rt(t = df$date, Rjs = list("Non-adjusted"=df$Rj,"Adjusted"=df$Rj_adj)) 
    } else {
      res <- calc.Rj(t = df$date,
                     lpdf_GI = lpdf_GI, adj.sp = F)
      df$Rj <- res$Rj
      
      plot.Rt(t = df$date, Rjs = list("Non-adjusted"=df$Rj))
    }
    ##
    
  })
  
  output$Rj_time = renderPlot ({
    action_rj_time()
  })
  
  
  # ___2.3 Spatial pattern --------------------

  action_rj_pnt = eventReactive(input$rj_go, {
    
    source("./read_file.R", local = TRUE)
    source("./cal_Rj.R", local = TRUE)
    
    # Plot
    #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
    #sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
    
    base <- readRDS("Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    
    if (input$sp_adjust == TRUE) {
      
      res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
                         lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
      df$Rj_adj <- res_adj$Rj
      #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
      sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
      
      plot.points(x1 = sdf$long, x2= sdf$lat,
                  Rj = sdf$Rj_adj, base_map = base, bnd = bnd)
    
      } else {
        
      res <- calc.Rj(t = df$date, 
                       lpdf_GI = lpdf_GI, adj.sp = F)
      df$Rj <- res$Rj
      #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
      sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
          
      plot.points(x1 = sdf$long, x2= sdf$lat,
                  Rj = sdf$Rj, base_map = base, bnd = bnd)
    }
    
    
  })
  
  action_rj_hexa = eventReactive(input$rj_go, {
    
    source("./read_file.R", local = TRUE)
    source("./cal_Rj.R", local = TRUE)
    
    # Plot
    #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
    #sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
    
    base <- readRDS("Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    
    if (input$sp_adjust == TRUE) {
      
      res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
                         lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
      df$Rj_adj <- res_adj$Rj
      #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
      sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
      
      plot.hex(x1 = sdf$long, x2= sdf$lat, Rj = sdf$Rj_adj, 
               nbin = input$Rj_hexagon_slider, base_map = base, bnd = bnd)
      
    } else {
      
      res <- calc.Rj(t = df$date, 
                     lpdf_GI = lpdf_GI, adj.sp = F)
      df$Rj <- res$Rj
      #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
      sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
      
      plot.hex(x1 = sdf$long, x2= sdf$lat, Rj = sdf$Rj, 
               nbin = input$Rj_hexagon_slider, base_map = base, bnd = bnd)
      
    }
    
  })
  
  output$Rj_pnt = renderPlot ({
    action_rj_pnt()
  }) 
  
  output$Rj_hexagon = renderPlot ({
    action_rj_hexa()
  }) 
  

  # ___2.4 Spatio-temporal ---------------------------------------
  
  action_rj_anim_pnt = eventReactive(input$rj_go, {
    
    source("./read_file.R", local = TRUE)
    source("./cal_Rj.R", local = TRUE)
    
    base <- readRDS("Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    
    if (input$sp_adjust == TRUE) {
      
      res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
                         lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
      df$Rj_adj <- res_adj$Rj

      # Plotting
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      anim_save("outfile.gif", animate.points(t = df$date, x1 = df$long, x2= df$lat,
                                              Rj = df$Rj_adj,dt = 14, base_map = base,bnd = bnd)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
      
      
    } else {
      
      res <- calc.Rj(t = df$date, 
                     lpdf_GI = lpdf_GI, adj.sp = F)
      df$Rj <- res$Rj
      
      # Plotting
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      anim_save("outfile.gif", animate.points(t = df$date, x1 = df$long, x2= df$lat,
                                              Rj = df$Rj, dt = 14, base_map = base,bnd = bnd)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
    } # else
    
  })
  
  
  output$Rj_spt_ptrn_point = renderImage({
    action_rj_anim_pnt()
  }, deleteFile = TRUE)
  
  
  action_rj_anim_hexa = eventReactive(input$rj_go, {
    
    source("./read_file.R", local = TRUE)
    source("./cal_Rj.R", local = TRUE)
    
    base <- readRDS("Taiwan_town_sf.rds") #optional
    bnd <- c(120.1,22.9,120.4,23.1) #optional
    
    
    if (input$sp_adjust == TRUE) {
      
      res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
                         lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
      df$Rj_adj <- res_adj$Rj
      
      # Plotting
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      anim_save("outfile.gif", animate.hex(t = df$date, x1 = df$long, x2= df$lat,
                                           Rj = df$Rj_adj, dt = 14, base_map = base,bnd = bnd)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
      
      
    } else {
      
      res <- calc.Rj(t = df$date, 
                     lpdf_GI = lpdf_GI, adj.sp = F)
      df$Rj <- res$Rj
      
      # Plotting
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      anim_save("outfile.gif", animate.hex(t = df$date, x1 = df$long, x2= df$lat,
                                           Rj = df$Rj, dt = 14, base_map = base,bnd = bnd)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
    } # else
    
    
  })
  
  
  output$Rj_spt_ptrn_hexagon = renderImage({
    action_rj_anim_hexa()
  }, deleteFile = TRUE)
  
  
  
  
  
  
  # OK!!
  # output$Rj_spt_ptrn_point = renderImage({
  # 
  #   source("./read_file.R", local = TRUE)
  #   source("./cal_Rj.R", local = TRUE)
  #   
  #   res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
  #                      lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
  #   res <- calc.Rj(t = df$date,
  #                  lpdf_GI = lpdf_GI, adj.sp = F)
  # 
  #   df$Rj <- res$Rj
  #   df$Rj_adj <- res_adj$Rj
  #   
  #   base <- readRDS("Taiwan_town_sf.rds") #optional
  #   bnd <- c(120.1,22.9,120.4,23.1) #optional
  #   
  #   
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   outfile <- tempfile(fileext='.gif')
  #   
  #   # now make the animation
  #   anim_save("outfile.gif", animate.points(t = df$date, x1 = df$long, x2= df$lat,
  #                                           Rj = df$Rj_adj,dt = 14, base_map = base,bnd = bnd)) # New
  #   
  #   # Return a list containing the filename
  #   list(src = "outfile.gif",
  #        contentType = 'image/gif'
  #        # width = 400,
  #        # height = 300,
  #        # alt = "This is alternate text"
  #   )
  #   
  # }, deleteFile = TRUE)
  

  

  # _______ Don't need at the moment______ -----------------
  
  # ___Spatial pattern ------------------------------------
  
  # output$textt2 <- renderText({
  #   
  #   source("./read_file.R", local = TRUE)
  #   source("./cal_Rj.R", local = TRUE)
  #   
  #   
  # 
  #   as.character(summary(res_adj$Rj))
  #   
  # })
  # 
  # 
  # output$Rj_pnt = renderPlot({
  # 
  #   source("./read_file.R", local = TRUE)
  #   source("./cal_Rj.R", local = TRUE)
  #   
  #   # Plot
  #   #sdf <- subset(df,date > as.Date("2007-09-15") & date < as.Date("2007-10-15"))
  #   sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
  #   
  #   base <- readRDS("Taiwan_town_sf.rds") #optional
  #   bnd <- c(120.1,22.9,120.4,23.1) #optional
  # 
  #   plot.points(x1 = sdf$long, x2= sdf$lat,
  #               Rj = sdf$Rj_adj, base_map = base, bnd = bnd)
  # 
  # 
  # })
  # 
  # output$Rj_hexagon = renderPlot({
  #   
  #   source("./read_file.R", local = TRUE)
  #   source("./cal_Rj.R", local = TRUE)
  #   
  #   # Plot
  #   sdf <- subset(df,date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
  #   
  #   
  #   base <- readRDS("Taiwan_town_sf.rds") #optional
  #   bnd <- c(120.1,22.9,120.4,23.1) #optional
  #   
  #   plot.hex(x1 = sdf$long, x2= sdf$lat, Rj = sdf$Rj, 
  #            nbin = input$Rj_hexagon_slider, base_map = base, bnd = bnd)
  #   
  # })
  

  
  
}

#shinyApp(ui, server) # Need this line if ui and server codes are written in a single script
