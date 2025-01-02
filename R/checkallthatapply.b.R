CATAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "CATAClass",
    inherit = CATABase,
    private = list(
      .init = function() {
        showdesc=self$options$showdescription
        if (showdesc){
          self$results$description$setContent(
            '<html>
                <head>
                <title>Food and Beverage Product formulation</title>
                <style>
                body {
                  font-family: Arial, sans-serif;
                  background-color: transparent; /* Transparent background */
                    margin: 0;
                  padding: 0;
                }
              
              .content {
                margin: 20px;
                padding: 20px;
                background-color: transparent;
              }
              
              h2 {
                color: #6B9DE8; /* Blue */
              }
              
              p {
                line-height: 1.6;
              }
              </style>
                </head>
                <body>
                <div class="content">
                <h2>Introduction to food experimental design</h2>
                <p>Food experimental design introduces a comprehensive approach to experimental design in food and beverage product formulation. This powerful tool aids gastronomy professionals, nutritionists, and high-performance athletes in creating and optimizing nutritional content for a variety of products.</p>
                
                <h2>Components of Experimental Design</h2>
                <p>The module specializes in 2k factorial designs for experimental design, particularly beneficial for screening in formulation studies. This includes the screening of many ingredients, facilitating efficient experimentation in the development and optimization of food and beverage products.</p>
                
                <h2>How to Use Experimental Design</h2>
                <p>When using experimental design, professionals start by identifying primary sensations and nutritional goals. This method ensures a standardized and effective approach to food experimental design.</p>
                
                </div>
                </body>
                </html>')}
      },      
        .run = function() {
        
          if (is.null(self$options$cons) || is.null(self$options$produ) || is.null(self$options$attr)){return()}
          
          atr_names=self$options$attr
          atr=self$data[atr_names]
          natr=dim(atr)[2] # n of atributes
          nobs=dim(atr)[1] # n of observations
          
          if (natr<2 || nobs<2){return()}
          
          #yield=private$.getyieldGUI(data=self$data,options=self$options)
          #responseName=yield$responseName
          #responsedata=yield$responsedata
          #rvars=length(responseName)
          
          # get inputs
          cons_names=self$options$cons
          consu=self$data[cons_names]
          pro_names=self$options$produ
          produ=self$data[pro_names]
          lik_names=self$options$lik
          lik=self$data[lik_names]
          
          atrNames=private$.namingissue(atr_names) # solve naming issues
          conNames=private$.namingissue(cons_names) # solve naming issues
          proNames=private$.namingissue(pro_names) # solve naming issues
          likNames=private$.namingissue(lik_names) # solve naming issues
          
          # Convert attributes
          atr2= data.frame(matrix(nrow = dim(atr)[1], ncol = dim(atr)[2]))
          for (k in 1:natr){atr2[k]=as.numeric(as.character(unlist(atr[k])))}
          colnames(atr2)=atrNames
          
          # Convert liking
          lik2=as.character(unlist(lik))
          lik3= data.frame(matrix(nrow = dim(atr)[1], ncol = 1))
          lik3[1]=as.numeric(ifelse(lik2 == "" | lik2 == "NaN", NA, lik2))
          colnames(lik3)=likNames
          
          # Convert product
          produ2=as.character(unlist(produ))
          nonum <- produ2[is.na(as.numeric(produ2))]
          proref <- unique(nonum) #ok done (Ideal o Control)
          produ3=data.frame(matrix(nrow = dim(atr)[1], ncol = 1))
          produ3[1]=produ2
          colnames(produ3)=proNames
          
          # Convert consumers
          consu2=as.character(unlist(consu))
          consu3=data.frame(matrix(nrow = dim(atr)[1], ncol = 1))
          consu3[1]=as.numeric(consu2)
          colnames(consu3)=conNames
          
          # get dataframe
          data1=cbind(consu3,produ3,lik3,atr2)

          # Reshape all dataset
          formula <- as.formula(paste("cbind(", paste(atrNames, collapse = ", "), ") ~", conNames))
          percentagesraw <- stats::aggregate(formula,data = data1,FUN = function(x) sum(x == 1)/length(x)*100)
          
          # Save info for plotting
          image3 <- self$results$attricheck
          image3$setState(percentagesraw)
          
          image2 <- self$results$consucheck
          image2$setState(percentagesraw)
          
          self$results$heatmapdata$setState(conNames)
          image1 <- self$results$heatmap
          image1$setState(percentagesraw)

          # Reshape all dataset
          formula2 <- as.formula(paste("cbind(", paste(atrNames, collapse = ", "), ") ~", conNames, "+", proNames))
          percentagesraw2 <- stats::aggregate(formula2,data = data1,FUN = function(x) sum(x == 1)/length(x)*100)
          percentages2=percentagesraw2[,-c(1,2)]
          samp=cbind(rowMeans(percentages2))
          assessor2=cbind(percentagesraw2[,c(1,2)],samp)
          
          product <- reshape(assessor2,timevar = proNames,idvar = conNames,direction = "wide")
          
          private$.productable(product)  
          
          # Cochran Q
          Qres=data.frame(Attribute = atrNames,
                          Q = rep(0,natr),
                          p_value = rep(0,natr),
                          MRD = rep(0,natr),
                          stringsAsFactors = FALSE)
          
          alfa=self$options$alphaval # alpha val in %
          for (k in 1:natr){
            data3=cbind(data1[,c(1,2)],data1[atrNames[k]])
            data_matrix <- reshape(data3,timevar =proNames,idvar=conNames,direction = "wide")
            #data_matrix <- data_matrix[, !grepl("Ideal", colnames(data_matrix))]
            data_matrix <- data_matrix[, !grepl(proref, colnames(data_matrix))]
            data_matrix <- data_matrix[,-1] # consumers
          
            Q=private$.CochranQ(data_matrix,alfa/100)
            Qres$Q[k]=Q[1]
            Qres$p_value[k]=Q[2]
            Qres$MRD[k]=Q[3]
          }
          
          table2 <- self$results$attrCochranQ
          for (xk in 1:natr){
            if (Qres$p_value[xk]>alfa/100){res1="Exclude"}else{res1="Ok"}
            table2$setRow(rowNo=xk, values=list(
              var=atr_names[xk],
              attrQ=Qres$Q[xk],
              attrQp=Qres$p_value[xk],
              commQ=res1))}
          
          formula3 <- as.formula(paste("cbind(", paste(atrNames, collapse = ", "), ") ~", proNames))
          countraw <- stats::aggregate(formula3,data = data1,FUN = function(x) sum(x == 1))
          countraw1=countraw[,-1] # Contingency table
          test1 <- chisq.test(countraw1)  # Independence test result
          
          # show contigency table + result table
          private$.contingencytable(countraw,test1)
          
          if (is.null(self$options$lik)){return()} # no liking input
          
          output=private$.musthaveattr(data1,atrNames,consumer_col=conNames,product_col =proNames,liking_col=likNames,proref=proref)
          plotdata=private$.prepare_plotdata(output)
          
          image4 <- self$results$musthaveplot
          image4$setState(plotdata)
          
          resER=private$.elicitation_rates(data1,atrNames,product_col =proNames,ideal_label=proref)

          image5 <- self$results$elicitationplots
          image5$setState(resER)

          #------------------------
          # Check Status
          #------------------------
          #object1=image1$state
          #object2=image2$state
          #object3=image3$state
          #object4=image4$state
          #object5=image5$state
          #xh1=length(serialize(object1,connection=NULL))
          #xh2=length(serialize(object2,connection=NULL))
          #xh3=length(serialize(object3,connection=NULL))
          #xh4=length(serialize(object4,connection=NULL))
          #xh5=length(serialize(object5,connection=NULL))
          #self$results$debugger$setContent(c(xh1,xh2,xh3,xh4,xh5)) #ok
          
      },
      .namingissue=function(xnames){
        xnames <- make.names(xnames)
        xnames <- gsub("\\.","",xnames) # clean
        return(xnames)
      },
      .heatmap=function(image1,...) {
        
        if (is.null(image1$state) || is.null(self$options$cons) || is.null(self$options$produ) || is.null(self$options$attr)){return(FALSE)}
        
        percentagesraw <- image1$state
        ConsuName=self$results$heatmapdata$state
        
        data_long <- reshape(
          percentagesraw,
          varying = list(names(percentagesraw)[2:ncol(percentagesraw)]),
          v.names = "Percentage",                   
          timevar = "Attribute",                   
          times = names(percentagesraw)[2:ncol(percentagesraw)],        
          direction = "long"                        
        )
        
        data_long <- data_long[order(data_long[[ConsuName]]), ]
        rownames(data_long) <- NULL 
        
        heatmap_plot=ggplot2::ggplot(data_long, ggplot2::aes(x = Attribute, y = factor(Consumer), fill = Percentage)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_gradientn(colors = c("blue", "lightblue", "yellow", "orange", "red"),name = "Percentage") +
          ggplot2::labs(x = "Attributes", y = "Consumers") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        print(heatmap_plot) # show plot
        TRUE
      },
      .consucheck=function(image2,...) {
        
        if (is.null(image2$state) || is.null(self$options$cons) || is.null(self$options$produ) || is.null(self$options$attr)){return(FALSE)}
        
        percentagesraw <- image2$state
        alfa=self$options$alphaval # alpha val
        
        consumers=percentagesraw[,1]
        percentages=percentagesraw[,-1]
        
        checkasse=as.data.frame(cbind(consumers,rowMeans(percentages)))
        colnames(checkasse)=c("Consumers","Percentage")
        
        total_assessors=length(consumers)
        proportions=mean(rowMeans(percentages)/100)
        
        alpha <- alfa/100
        lower_bounds <- proportions - qnorm(1 - alpha / 2) * sqrt((proportions * (1 - proportions)) / total_assessors) # CI (95%) binomial approach
        upper_bounds <- proportions + qnorm(1 - alpha / 2) * sqrt((proportions * (1 - proportions)) / total_assessors) # CI (95%) binomial approach
        
        if (lower_bounds<0){lower_bounds=0}
        ymax=ceiling(max(checkasse[,2])/5)*5
        
        df_sorted <- checkasse[order(checkasse$Percentage,decreasing=TRUE),]
        df_sorted$Consumers <- factor(df_sorted$Consumers, levels = df_sorted$Consumers)
        
        bar_plot <- ggplot2::ggplot(df_sorted, ggplot2::aes(x = Consumers, y = Percentage)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::geom_hline(yintercept = 100*lower_bounds, color = "red", linetype = "solid", size = 1) +
          ggplot2::geom_hline(yintercept = 100*upper_bounds, color = "red", linetype = "solid", size = 1) +
          ggplot2::scale_y_continuous( breaks = seq(0, 100, by = 5),limits = c(0, ymax)) +
          ggplot2::labs(x = "Consumers", y = "% Checked") +
          ggplot2::theme_minimal()+
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) # rotate 90 degree
        
        print(bar_plot) # show plot
        TRUE
      },  
      .attricheck=function(image3,...) {
        
        if (is.null(image3$state) || is.null(self$options$cons) || is.null(self$options$produ) || is.null(self$options$attr)){return(FALSE)}
        
        percentagesraw <- image3$state
        alfa=self$options$alphaval # alpha val
        percentages=percentagesraw[,-1]
        
        att=cbind(colMeans(percentages))
        ymax=ceiling(max(att)/5)*5
        
        checkatt=as.data.frame(cbind(rownames(att),att))
        rownames(checkatt)=NULL
        colnames(checkatt)=c("Attributes","Percentage")
        checkatt$Percentage <- as.numeric(checkatt$Percentage)
        
        total_att=length(att)
        proportions1=mean(colMeans(percentages)/100)
        
        alpha <- alfa/100
        lower_bounds1 <- proportions1 - qnorm(1 - alpha / 2) * sqrt((proportions1 * (1 - proportions1)) / total_att)
        upper_bounds1 <- proportions1 + qnorm(1 - alpha / 2) * sqrt((proportions1 * (1 - proportions1)) / total_att)
        
        if (lower_bounds1<0){lower_bounds1=0}
        
        df_attsorted <- checkatt[order(checkatt$Percentage,decreasing=TRUE),]
        df_attsorted$Attributes <- factor(df_attsorted$Attributes, levels = df_attsorted$Attributes)
        
        bar_plot1 <- ggplot2::ggplot(df_attsorted, ggplot2::aes(x = Attributes, y = Percentage)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::geom_hline(yintercept = 100*lower_bounds1, color = "red", linetype = "solid", size = 1) +
          ggplot2::geom_hline(yintercept = 100*upper_bounds1, color = "red", linetype = "solid", size = 1) +
          ggplot2::scale_y_continuous( breaks = seq(0, 100, by = 5),limits = c(0, ymax)) +
          ggplot2::labs(x = "Attributes", y = "% Checked") +
          ggplot2::theme_minimal()+
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) # rotate 90 degree
        
        print(bar_plot1)
        TRUE
      },
      .productable=function(product){
        
        table0 <- self$results$productable
        nname=colnames(product)
        nname <- gsub("^samp\\.", "", colnames(product))
        nname[1]="Consumer ID"
        num_rows <- nrow(product)
        num_cols <- ncol(product)
        
        # Fill columns
        table0$addColumn(name=paste0("v",1), title=nname[1], type='integer')
        for (i in 2:num_cols){table0$addColumn(name=paste0("v",i), title=nname[i], type='number')}
        
        # Fill Rows
        for (i in 1:num_rows){
          row_values <- c(sapply(1:num_cols, function(j) product[i,j]))
          col_names <- c(paste0("v",1:num_cols))
          names(row_values) <- col_names
          
          row=as.list(row_values)
          table0$addRow(rowKey=i,row)
        }
      },
      .CochranQ=function(dmatr,alpha=0.05){
        n <- nrow(dmatr)
        k <- ncol(dmatr)
        
        row_sum <- rowSums(dmatr)    # x_i
        col_sum <- colSums(dmatr)    # x_j
        
        T1 <- sum(dmatr)  # T 
        C1 <- sum(col_sum^2)  
        R1 <- sum(row_sum^2)        
        
        Q=((k-1)*(k*C1-T1^2))/(k*T1-R1)
        
        C2=k*(k-1)/2 # C (Sheskin)
        alpha_adj=alpha/(2*C2) # nondirectional alpha_adj (Sheskin)
        z_adj=qnorm(1-alpha_adj)
        
        MRD <- z_adj*sqrt(2*((k*T1-R1)/(n^2*k*(k-1)))) # Minimum required distance (CDc)
        gdl=k-1
        pvalue=1-pchisq(q=Q,df=gdl)
        outp=c(Q,pvalue,MRD)
        return(outp)
      },
      .contingencytable=function(conting,test1){
        
        table3 <- self$results$contingencytable
        nname=colnames(conting)
        #nname <- gsub("^samp\\.", "", colnames(product))
        #nname[1]="Consumer ID"
        num_rows <- nrow(conting)
        num_cols <- ncol(conting)
        
        # Fill columns
        table3$addColumn(name=paste0("v",1), title=nname[1], type='text')
        for (i in 2:num_cols){table3$addColumn(name=paste0("v",i), title=nname[i], type='integer')}
        
        # Fill Rows
        for (i in 1:num_rows){
          row_values <- c(sapply(1:num_cols, function(j) conting[i,j]))
          col_names <- c(paste0("v",1:num_cols))
          names(row_values) <- col_names
          
          row=as.list(row_values)
          table3$addRow(rowKey=i,row)
        }
        X2 <- round(test1$statistic,2)
        pval <- ifelse(test1$p.value < 0.001, "<.001", format(round(test1$p.value,3), scientific = FALSE))
        formatted_text <- paste0("\u03C7\u00B2 = ", round(X2, 2), ", df: ", test1$parameter,", p-value: ", pval)
        #formatted_expression <- bquote(chi^2 == .(round(X2,2)) ~ p[italic(value)] == .(pval))
        table3$setNote('Test for independence: ',formatted_text)
      },
      .musthaveattr=function(data, attributes, consumer_col = "Consumer", product_col = "Product", liking_col = "Liking",proref="Ideal") {
        
        ideal_data <- data[data[[product_col]] == proref, ]
        results <- list()
        column_sums <- list()
        percentages <- list()
        liking_means <- list()
        mean_drops <- list()
        
        for (attr in attributes) {
          unique_consumers <- unique(data[[consumer_col]])
          result <- data.frame(
            Consumer = unique_consumers,
            Group1 = numeric(length(unique_consumers)),
            Group2 = numeric(length(unique_consumers)),
            Group3 = numeric(length(unique_consumers)),
            Group4 = numeric(length(unique_consumers)),
            lik1 = numeric(length(unique_consumers)),
            lik2 = numeric(length(unique_consumers)),
            lik3 = numeric(length(unique_consumers)),
            lik4 = numeric(length(unique_consumers))
          )
          
          for (i in seq_along(result$Consumer)) {
            consumer <- result$Consumer[i]
            
            consumer_data <- data[data[[consumer_col]] == consumer & data[[product_col]] != proref, ]
            ideal_value <- ideal_data[ideal_data[[consumer_col]] == consumer, attr]
            
            result$Group1[i] <- sum(consumer_data[[attr]] == 1 & ideal_value == 1, na.rm = TRUE)
            result$lik1[i] <- sum(consumer_data[[liking_col]][consumer_data[[attr]] == 1 & ideal_value == 1], na.rm = TRUE)
            result$Group2[i] <- sum(consumer_data[[attr]] == 0 & ideal_value == 1, na.rm = TRUE)
            result$lik2[i] <- sum(consumer_data[[liking_col]][consumer_data[[attr]] == 0 & ideal_value == 1], na.rm = TRUE)
            result$Group3[i] <- sum(consumer_data[[attr]] == 1 & ideal_value == 0, na.rm = TRUE) 
            result$lik3[i] <- sum(consumer_data[[liking_col]][consumer_data[[attr]] == 1 & ideal_value == 0], na.rm = TRUE)
            result$Group4[i] <- sum(consumer_data[[attr]] == 0 & ideal_value == 0, na.rm = TRUE)
            result$lik4[i] <- sum(consumer_data[[liking_col]][consumer_data[[attr]] == 0 & ideal_value == 0], na.rm = TRUE)
          }
          
          results[[attr]] <- result
          column_sums[[attr]] <- colSums(result[, -1], na.rm = TRUE)
          
          total_frequency <- sum(column_sums[[attr]][c("Group1", "Group2", "Group3", "Group4")], na.rm = TRUE)
          percentages[[attr]] <- (column_sums[[attr]][c("Group1", "Group2", "Group3", "Group4")] / total_frequency) * 100
          
          liking_means[[attr]] <- column_sums[[attr]][c("lik1", "lik2", "lik3", "lik4")] / 
            column_sums[[attr]][c("Group1", "Group2", "Group3", "Group4")]
          
          mean_drops[[attr]] <- list(
            md1 = liking_means[[attr]]["lik1"] - liking_means[[attr]]["lik2"],
            md2 = liking_means[[attr]]["lik3"] - liking_means[[attr]]["lik4"]
          )
        }
        return(list(results = results,column_sums = column_sums,percentages = percentages,liking_means = liking_means,mean_drops = mean_drops))
      },
      .prepare_plotdata=function(output) {
        attributes <- names(output$mean_drops)
        plot_data <- data.frame(Attribute = character(),Percentage = numeric(), MeanDrop = numeric(),Type = character())
        
        for (attr in attributes) {
          percentages <- output$percentages[[attr]]
          mean_drops <- output$mean_drops[[attr]]
          
          plot_data <- rbind(plot_data, data.frame(
            Attribute = attr,
            Percentage = percentages["Group2"],
            MeanDrop = mean_drops$md1,
            Type = "md1"
          ))
          
          plot_data <- rbind(plot_data, data.frame(
            Attribute = attr,
            Percentage = percentages["Group3"],
            MeanDrop = mean_drops$md2,
            Type = "md2"
          ))
        }
        
        return(plot_data)
      },
      .musthaveplot=function(image4,...) {
        
        if (is.null(image4$state) || is.null(self$options$cons) || is.null(self$options$produ) || is.null(self$options$lik) || is.null(self$options$attr)){return(FALSE)}
        
        plot_data<- image4$state
        
        scatter_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Percentage, y = MeanDrop, label = Attribute)) +
          ggplot2::geom_point(data = subset(plot_data, Type == "md1"), ggplot2::aes(shape = Type, color = Type),size = 4) + 
          ggplot2::geom_point(data = subset(plot_data, Type == "md2"), ggplot2::aes(shape = Type, color = Type), size = 4) + 
          ggplot2::geom_text(vjust = -0.5, size = 3.5) + 
          ggplot2::scale_shape_manual(values = c("md1" = "-", "md2" = "+"),labels = c("md1" = "P(No|Yes)", "md2" = "P(Yes|No)")) + 
          ggplot2::scale_color_manual(values = c("md1" = "blue", "md2" = "red"),labels = c("md1" = "P(No|Yes)", "md2" = "P(Yes|No)")) + 
          ggplot2::geom_vline(xintercept = 20, linetype = "dashed", color = "black") + 
          ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +   
          ggplot2::labs(x = "Percentage (%)", y = "Mean Drop") +
          ggplot2::theme_minimal() + ggplot2::theme(legend.title = ggplot2::element_blank(),legend.position = "top")
        
        print(scatter_plot)
        TRUE
      },
      .elicitation_rates=function(data, attributes, product_col = "Product", ideal_label = "Ideal") {
        products <- unique(data[[product_col]][data[[product_col]] != ideal_label])
        results <- list()
        
        for (product in products) {
          product_data <- data[data[[product_col]] == product, ]
          ideal_data <- data[data[[product_col]] == ideal_label, ]
          
          product_results <- data.frame(
            Attribute = attributes,
            ER = numeric(length(attributes)),
            Lower_CI = numeric(length(attributes)),
            Upper_CI = numeric(length(attributes)),
            UCL = numeric(length(attributes)),
            LCL = numeric(length(attributes)),
            ESS = numeric(length(attributes))
          )
          
          for (attr in attributes) {
            p_real <- mean(product_data[[attr]] == 1, na.rm = TRUE)
            p_ideal <- mean(ideal_data[[attr]] == 1, na.rm = TRUE)
            n_real <- sum(product_data[[attr]] == 1, na.rm = TRUE)
            n_ideal <- sum(ideal_data[[attr]] == 1, na.rm = TRUE)
            
            ER <- p_real - p_ideal # Elicitation Rates - faster.
            # ER <- n_real/ntotal - n_ideal/ntotal
            # ER=binom.test(ess,ntotal) # ER magnitude ok but not direction
            
            n_r <- sum(product_data[[attr]] == 1 & ideal_data[[attr]] == 0, na.rm = TRUE) # 1|0
            n_i <- sum(ideal_data[[attr]] == 1 & product_data[[attr]] == 0, na.rm = TRUE) # 0|1
            exi1 <- sum(product_data[[attr]] == 1 & ideal_data[[attr]] == 1, na.rm = TRUE) # 1|1
            exi0 <- sum(product_data[[attr]] == 0 & ideal_data[[attr]] == 0, na.rm = TRUE) # 0|0
            ntotal <- n_r + n_i + exi1 + exi0
            ess <- abs(n_r - n_i)
            
            # Normal approximation to the binomial calculation
            pro <- abs(ER)
            SE <- sqrt(pro*(1-pro)/ntotal)
            alpha <- 0.05
            Z <- qnorm(1-alpha/2)
            
            lower_CI <- ER - Z * SE
            upper_CI <- ER + Z * SE
            
            # Approx. correction needed
            UCL <- abs(ER) / 2 + Z * SE
            LCL <- -abs(ER) / 2 - Z * SE
            ESS <- ess # effect size
            
            product_results[product_results$Attribute == attr, ] <- list(attr, ER, lower_CI, upper_CI, UCL, LCL, ESS)
          }
          
          product_results <- product_results[order(-abs(product_results$ER)), ]
          results[[product]] <- product_results
        }
        
        return(results)
      },
      .elicitationplots=function(image5,...) {
        
        if (is.null(image5$state) || is.null(self$options$cons) || is.null(self$options$produ) || is.null(self$options$lik) || is.null(self$options$attr)){return(FALSE)}
        
        results<- image5$state
        plots <- list()
        
        for (product in names(results)) {
          product_data <- results[[product]]
          
          product_data$Attribute <- factor(product_data$Attribute, levels = unique(product_data$Attribute))
          
          plot <- ggplot2::ggplot(product_data, ggplot2::aes(x = Attribute, y = ER)) +
            ggplot2::geom_bar(stat = "identity", fill = "blue", width = 0.6) +
            ggplot2::geom_line(ggplot2::aes(y = UCL, group = 1), color = "black", linetype = "dotdash", size = 0.8) +
            ggplot2::geom_line(ggplot2::aes(y = LCL, group = 1), color = "black", linetype = "dotdash", size = 0.8) +
            ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
            ggplot2::labs(x = paste0("List of attributes: ",product, " vs Ideal"),y = "Elicitation Proportion") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          
          plots[[product]] <- plot
        }
        
        #nplots=length(plots) 
        gridExtra::grid.arrange(grobs=plots,ncol=2)
        TRUE
      }) # Close - List
) # Close - R6::R6Class
