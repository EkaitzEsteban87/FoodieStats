desirabilityClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "desirabilityClass",
    inherit = desirabilityBase,
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
          
            qvars=length(self$options$vars)
            yield=private$.getyieldGUI(data=self$data,options=self$options)
            responseName=yield$responseName
            responsedata=yield$responsedata
            rvars=length(responseName)
            
            if (is.null(self$options$vars) || qvars<3 || rvars<2){return()}
            
            varNames <- self$options$vars
            vardata=self$data[varNames]
            varNames=private$.namingissue(varNames) # solve naming issues
            self$results$ternarydata$setState(varNames) # save info
            
            nda=length(vardata)
            vardata2= data.frame(matrix(nrow = dim(vardata)[1], ncol = dim(vardata)[2]))
            for (k in 1:nda){
              vardata2[k]=as.numeric(as.character(unlist(vardata[k])))
            }
            
            colnames(vardata2)=varNames
            doe=cbind(vardata2,responsedata)
            
            doelm=doe
            zk=dim(doelm)[2]-rvars
            row0=rowSums(vardata2)
            
            scaling <- cbind(matrix(1, nrow = zk, ncol = 1),matrix(0, nrow = zk, ncol = 1))
            
            tolerance <- 1e-2 # sometimes data is rounded
            if (any(abs(row0-1) > tolerance)){ # Normalized Data 
              for (k in 1:zk){
                x=doelm[,k]
                normalized = (x-min(x))/(max(x)-min(x))
                doelm[,k]=normalized
                scaling[k,1]=(max(x)-min(x))
                scaling[k,2]=min(x)
              }
            }
            
            cod=doelm[1:qvars] # x1, x2, x3...
            doetable=cbind(vardata2,cod,responsedata)
            
            # Experimental data table and Scaling Table
            private$.experimentaltable(rvars,doetable,varNames,responseName)    
            private$.scalingtable(varNames,scaling)            
            
            #-------------------------------------------
            # Individual desirability and optimal point and show in a table
            desidata=private$.getDesirabilityGUI(data=self$data,options=self$options)
            modelformula <- desidata$modelformula
            modelformula2 <- desidata$modelformula2
            desindx <-desidata$desindx
            mvars=length(modelformula)
            
            if (mvars<2){return()}
            
            models1 <- list()
            for (i in 1:mvars) {models1[[i]] <- lm(as.formula(modelformula[i]),data=doelm)}
            
            overallD=private$.getdesirabilitymodels(desidata)
            
            # 12 - 15Mb of memory needed
            ngr0=switch(as.character(qvars),"3"=1000,"4"=125,"5"=50,"6"=30,"7"=21,"8"=16,"9"=14,"10"= 12,10)
            simplexgrid=AlgDesign::gen.mixture(ngr0,qvars)
            colnames(simplexgrid)=varNames
            
            predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
            predOutcomes1 <- do.call(cbind,predictions)
            overall_d <- stats::predict(overallD,predOutcomes1,all=TRUE) # Individuals included
            
            ind=which.max(overall_d[,ncol(overall_d)])
            overall_dmax=overall_d[ind,] # All desirability values
            optpoint=simplexgrid[ind,] # optimal point
            
            models2 <- list()
            mvars2=length(modelformula2)
            for (i in 1:mvars2) {models2[[i]] <- lm(as.formula(modelformula2[i]),data=doelm)} # esto esta bien (falta comprobar con fruitpunch que Watermelon y x1 son diferentes)
            optresults <- lapply(models2, stats::predict, newdata = optpoint)
            optoutputs=unlist(optresults)
            
            dvars=ncol(overall_d)-1
            private$.desitable(dvars,rvars,desindx,varNames,responseName,optpoint,overall_dmax,optoutputs,scaling)
            
            # Show Overall Desirability plot
            self$results$optpoint$setState(optpoint)
            image97=self$results$overall4kplot
            image97$setState(doelm)
            
            # Show individual Desirability plots
            self$results$desiresponse$setState(responseName[desindx])
            image98=self$results$individual4kplot
            image98$setState(doelm)
            image98$setSize(960,480*dvars)
            #-------------------------------------------
            # Add more features
            # ToDo Cox plots

            #------------------------
            # Check Status
            #------------------------
            #object1=image2$state
            #xh1=length(serialize(object1,connection=NULL))
            #self$results$debugger$setContent(cbind(xh1,xh2,xh3,xh4))
        },
        .experimentaltable=function(rvars,doetable,varNames,responseName){
          
          table0 <- self$results$doetable2
          qvars=length(self$options$vars)
          
          # Fill columns
          for (i in 1:qvars){table0$addColumn(name=paste0("v",i), title=varNames[i], type='number')}
          for (i in 1:qvars){table0$addColumn(name=paste0("cod",i), title=paste0("x",i), type='number')}
          for (i in 1:rvars){table0$addColumn(name=paste0("yie",i), title=responseName[i], type='number')}
          
          # Fill Rows
          num_rows <- nrow(doetable)
          for (i in 1:num_rows){
            row_values <- c(sapply(1:qvars, function(j) doetable[i,j]),
                            sapply(1:qvars, function(j) doetable[i,j+qvars]),
                            sapply(1:rvars, function(j) doetable[i,j+2*qvars]))
            
            col_names <- c(paste0("v",1:qvars),paste0("cod",1:qvars),paste0("yie",1:rvars))
            names(row_values) <- col_names
            
            row=as.list(row_values)
            table0$addRow(rowKey=i,row)
          }
        },
        .desitable=function(dvars,rvars,desindx,varNames,responseName,optpoint,overall_dmax,optoutputs,scaling){
          table1 <- self$results$desitable1
          table2 <- self$results$desitable2
          table3 <- self$results$desitable3
          
          # Create table by code 
          qvars=length(self$options$vars)

          # Fill columns Table 1
          for (i in 1:qvars){table1$addColumn(name=paste0("v",i), title=varNames[i], type='number')}
          for (i in 1:qvars){table1$addColumn(name=paste0("cod",i), title=paste0("x",i), type='number')}
          
          # Fill Rows Table 1
          row_values=c(scaling[,1]*optpoint+scaling[,2],optpoint)
          col_names <- c(paste0("v",1:qvars),paste0("cod",1:qvars))
          names(row_values) <- col_names
          row=as.list(row_values)
          table1$addRow(rowKey=1,row)
          
          # Fill columns Table 2
          for (i in 1:dvars){table2$addColumn(name=paste0("ides",i), title=paste0(responseName[desindx[i]]," des."), type='number')}
          table2$addColumn(name="des", title="Overall desirability",type='number')
          
          # Fill Rows Table 2
          row_values2=c(overall_dmax)
          col_names2 <- c(paste0("ides",1:dvars),"des")
          names(row_values2) <- col_names2
          row2=as.list(row_values2)
          table2$addRow(rowKey=1,row2)
          
          for (i in 1:rvars){table3$addColumn(name=paste0("yie",i), title=paste0(responseName[i]), type='number')}
          # Fill Rows
          row_values3=c(optoutputs)
          col_names3 <- c(paste0("yie",1:rvars))
          names(row_values3) <- col_names3
          row3=as.list(row_values3)
          table3$addRow(rowKey=1,row3)
        },
        .scalingtable=function(varNames,scaling){
          table4 <- self$results$scalingtable
          
          qvars=length(self$options$vars)
          
          # Fill columns
          table4$addColumn(name='nam4', title="Model Terms",type='text')
          table4$addColumn(name='sca4', title="Scaling formula",type='text')
          # Fill Rows
          xa=paste0("x",1:qvars) 
          rowz=list()
          for (i in 1:qvars){
            rowz$nam4 <- varNames[i]
            rowz$sca4 <- paste0("= ",scaling[i,1],"\u00b7",xa[i],"+",scaling[i,2])
            rowz=as.list(rowz)
            table4$addRow(rowKey=i,rowz)
          }
        },
        .getDesirabilityGUI=function(data,options) {
          desinames <- list()
          desinames2 <- list()
          desitypes <- list()
          desilows <- list()
          desihighs <- list()
          desitargets <- list()
          desindex <- list()
          
          for (i in 1:5) {
            model_info <- options[[paste0("d", i, "modelinfo")]]
            desi_info <- options[[paste0("d", i, "type")]]
            
            if (!is.null(model_info)) {
              modeldata <- data[model_info]
              desinames2[[i]] <- as.character(t(modeldata[1, ]))
              
              if(desi_info != "dNone"){
                desindex[[i]] <- i
                desinames[[i]] <- as.character(t(modeldata[1, ]))
                desitypes[[i]] <- options[[paste0("d", i, "type")]]
                desilows[[i]] <- as.numeric(options[[paste0("d", i, "low")]])
                desihighs[[i]] <- as.numeric(options[[paste0("d", i, "high")]])
                desitargets[[i]] <- as.numeric(options[[paste0("d", i, "target")]])
              }
            }
          }
          
          modelformula=unlist(desinames)
          modelformula2=unlist(desinames2)
          lowdata <- unlist(desilows)
          highdata <- unlist(desihighs)
          targetdata <- unlist(desitargets)
          desiratype <- unlist(desitypes)
          desindx <- unlist(desindex)
          
          return(list(lowdata = lowdata, highdata = highdata, targetdata = targetdata, desiratype = desiratype,modelformula = modelformula,modelformula2=modelformula2,desindx=desindx))
        },
        .getyieldGUI=function(data,options) {
          desinames <- list()
          desidata <- list()
          for (i in 1:5) {
            responseName <- options[[paste0("d", i, "yield")]]
            if (!is.null(responseName)) {
              responsedata <- data[responseName]
              desidata[[i]] <- responsedata
              responseName=private$.namingissue(responseName) # solve naming issues
              desinames[[i]] <- responseName
            }
          }
          
          desidata_clean <- desidata[!sapply(desidata,is.null)] # null list
          responseName=unlist(desinames)
          responsedata <-  as.data.frame(do.call(cbind, desidata_clean))
          colnames(responsedata)=responseName

          return(list(responseName=responseName,responsedata=responsedata))
        },
        .getdesirabilitysurface = function(steps=100,models=models1,FUN=overallD,idx=1,...){
          a = seq(0,1,length=steps)
          b = seq(0,sqrt(3)/2,length=steps)
          x1=outer(a,b,function(a,b){2*b/sqrt(3)})
          x2=outer(a,b,function(a,b){1-(2*b/sqrt(3))-(a-b/sqrt(3))})
          x3=outer(a,b,function(a,b){a-b/sqrt(3)})
          x11=as.vector(x1)
          x22=as.vector(x2)
          x33=as.vector(x3)
          
          values <- list(x11 = x11, x22 = x22, x33 = x33) 
          varNames=self$results$ternarydata$state
          simplexgrid <- data.frame(setNames(values, varNames))
        
          predictions <- lapply(models, stats::predict, newdata = simplexgrid)
          predOutcomes1 <- do.call(cbind,predictions)
          overall_d0 <- stats::predict(FUN,predOutcomes1,all=TRUE) # all=TRUE
          overall_d=overall_d0[,idx]
          dim(overall_d)=c(steps,steps)
          return(overall_d)
        },
        .TernaryContour = function(Func, resolution = 96L, direction = getOption("ternDirection", 1L),
                                   region = getOption("ternRegion", ternRegionDefault),within = NULL, filled = FALSE, legend, legend... = list(),
                                   nlevels = 10, levels = pretty(zlim, nlevels), zlim,
                                   color.palette = function(n) viridisLite::viridis(n, alpha = 0.6),
                                   fill.col = color.palette(length(levels) - 1),...) {
        
          if (direction == 1L) {
            x <- seq(-0.5, 0.5, length.out = resolution)
            y <- seq(0, sqrt(0.75), length.out = resolution)
          } else if (direction == 2L) {
            x <- seq(0, sqrt(0.75), length.out = resolution)
            y <- seq(-0.5, 0.5, length.out = resolution)
          } else if (direction == 3L) {
            x <- seq(-0.5, 0.5, length.out = resolution)
            y <- seq(-sqrt(0.75), 0, length.out = resolution)
          } else { # (direction == 4) 
            x <- seq(-sqrt(0.75), 0, length.out = resolution)
            y <- seq(-0.5, 0.5, length.out = resolution)
          }
          
          ternRegionDefault <- structure(cbind(
            a = c(min = 0, max = 100),
            b = c(0, 100),
            c = c(0, 100)
          ), class = "ternRegion")
          
          RegionCorners <- function(region = getOption("ternRegion", ternRegionDefault)){
            cbind(a = region[c(2, 3, 5)],b = region[c(1, 4, 5)],c = region[c(1, 3, 6)])
          }
          
          if (is.null(within)){
            within <- Ternary::GrowPolygon(t(Ternary::TernaryToXY(RegionCorners(region))),
                                  buffer = 1 / resolution)
          }else{within <- xy.coords(within)}
          
          FunctionWrapper <- function(x, y) {
            abc <- Ternary::XYToTernary(x, y, direction)
            inPlot <- as.logical(sp::point.in.polygon(x, y, within$x, within$y))
            evaluated <-Func(abc[1, inPlot], abc[2, inPlot], abc[3, inPlot],...)
            
            if (length(evaluated) == 1L) {warning("`Func(a, b, c)` should return a vector, but returned a single value.")}
            
            ret <- rep_len(NA_real_, length(x))
            ret[inPlot] <- evaluated
            # Return:
            ret
          }
          z <- outer(X = x, Y = y, FUN = FunctionWrapper)
          if (missing(zlim)) zlim <- range(z, finite = TRUE)
          if (filled) {graphics::.filled.contour(x, y, z, levels, fill.col)}
          contour(x, y, z, add = TRUE, nlevels = nlevels, levels = levels,zlim = zlim, ...)
          # Return:
          invisible(list(x = x, y = y, z = z))
        },
        .colpalletes=function(...){
          color1=self$options$colorselection
          basecolors <- switch(color1,
            "SpectralStylish"=colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")),
            "JamoviStylish"=colorRampPalette(c("#3e6da9","#6B9DE8", "#9f9f9f","lightgray","#E6AC40","#ebbc66")),
            "BeachStylish"=colorRampPalette(c("orange", "lightyellow", "lightblue", "lightgreen")),
            "OceanStylish"=colorRampPalette(c("royalblue2", "lightgreen", "darkturquoise","lavender")),
            "SummerSalad"=colorRampPalette(c("#8A3E73", "#FF282A", "#FBE62D", "#90CF44", "#59B512")),
            "FruitCup"=colorRampPalette(c("#B42B4D", "#F40224", "#111920", "#FDB70D", "#C1C454")),
            "Delight"=colorRampPalette(c("#FA4490", "#FCCED4", "#6CE4E7", "#DAF069", "#924B9C")),
            "Greensmoothie"=colorRampPalette(c("#326414", "#428612", "#54AC5A", "#BCD76D", "#E1EDC1")),
            "Turmeric"=colorRampPalette(c("#E28401", "#EC9D04", "#F0B51D", "#C83701", "#B00005")),
            "Snacks"=colorRampPalette(c("#4E070C", "#D22701", "#FF670E", "#FDE4CE", "#AAD15F")),
            "Market"=colorRampPalette(c("#3E5B8F", "#AB5B84", "#FFCA70", "#FFA789", "#FF6064")),
            "Sashimi"=colorRampPalette(c("#F45F67", "#FC7100", "#FB8818", "#F5DDC2", "#5CA135")),
            "Macarons"=colorRampPalette(c("#E85F81", "#F7BFCA", "#DDD8EC", "#E9CDC3", "#CDD96D")),
            "Feast"=colorRampPalette(c("#883668", "#A01B2C", "#D50102", "#F15623", "#264E01")),
            "Veggies"=colorRampPalette(c("#829461", "#ABB95B", "#FFF7D9", "#FFC559", "#9C5273")),
            "Gingerbread"=colorRampPalette(c("#9B1B32", "#547061", "#F3F3F2", "#C69255", "#A1754F")),
            "Beetroot"=colorRampPalette(c("#835D84", "#CE558F", "#D872A8", "#ACB885", "#EFA55A")),
            "Spice"=colorRampPalette(c("#C80238", "#F92827", "#FDA501", "#C5C957", "#D19B66")),
            "Sweet"=colorRampPalette(c("#F6C4C7", "#F1CCD3", "#F5F7F2", "#C1312E", "#951911")),
            "Tropical"=colorRampPalette(c("#35417B", "#E22A4F", "#FA8D54", "#F4EB67", "#7EC487")),
            "Breakfast"=colorRampPalette(c("#C6374B", "#D23F40", "#F7EAD4", "#F7D655", "#95A438")),
            "Carrots"=colorRampPalette(c("#642B52", "#754599", "#9650B8", "#F9F8FD", "#FE7C39")),
            "Stick"=colorRampPalette(c("#549829", "#93C154", "#F8FCFF", "#115B9A", "#613677")),
            "Tea"=colorRampPalette(c("#AB002D", "#C00219", "#FFAC67", "#D7D797", "#A6C058")),
            "Decadent"=colorRampPalette(c("#C64573", "#A6C769", "#FAFAF3", "#FDB14A", "#18AED4")),
            "Umami"=colorRampPalette(c("#883A61", "#CC010A", "#E7A545", "#72B800", "#189E01")),
            "Berry"=colorRampPalette(c("#A60839", "#ED0101", "#F9D3A0", "#435272", "#1D121B")),
            "Sugary"=colorRampPalette(c("#FE7573", "#FFC8D0", "#C3AACD", "#FDEEAB", "#FED28B")),
            "Citrus"=colorRampPalette(c("#D10134", "#FD423F", "#FE8000", "#FFB701", "#67AD00")),
            "Chard"=colorRampPalette(c("#C43BA6", "#FE0165", "#F6F7F9", "#FFEB61", "#A0CD01")),
            "Shrimp"=colorRampPalette(c("#69325A", "#CA0500", "#FA9D83", "#FE8B00", "#397C09")),
            "SweetSalad"=colorRampPalette(c("#C92038", "#FF0101", "#A44000", "#FEAA00", "#6EA602")),
            "Cheesecake"=colorRampPalette(c("#D90024", "#E20437", "#FCFBF7", "#2B4456", "#11263B")),
            "FruitBasket"=colorRampPalette(c("#622B3D", "#FD2A82", "#FF841F", "#FADF3A", "#41522E")),
            "Cayenne"=colorRampPalette(c("#E20101", "#EF5400", "#F5E5D6", "#FDC707", "#724635")),
            "Smoothie"=colorRampPalette(c("#eedbc0", "#efbf26", "#b3a126", "#cc8e23", "#d4b729")),
            "Mojito"=colorRampPalette(c("#8b967e", "#a0ad8d", "#c3cd79", "#e3ecaa", "#5c7444")),
            "Coffee"=colorRampPalette(c("#ccb1a0", "#f1e2d1", "#907966", "#6c442b", "#f4cc5c")),
            "Wine"=colorRampPalette(c("#ffb9b9", "#ee7272", "#a31818", "#6d0202", "#360000")),
            "Beer"=colorRampPalette(c("#FFF897", "#FAE96F", "#F6C101", "#EC9D00", "#DF8D03", "#C96E12")),
            "Fish"=colorRampPalette(c("#7e7d9e", "#a6adbe", "#577ea2", "#306598", "#002a65")),
            "Cheese"=colorRampPalette(c("#f7df47", "#f9e02e", "#f9d02e", "#f9c02e", "#f9b02e")),
            "Bread"=colorRampPalette(c("#e5ccac", "#f2d3a1", "#f2c480", "#b07645", "#8b5220")),
            "Chocolate"=colorRampPalette(c("#310A0B", "#491B1D", "#743A36", "#B96A59", "#E0A387")),
            "Beef"=colorRampPalette(c("#F7D5D4", "#E8B3B9", "#CB6862", "#AA3C3B", "#E97856", "#FCB79A")),
            "Honey"=colorRampPalette(c("#f9c901", "#f6e000", "#985b10", "#6b4701", "#896800")),
            "Dairy"=colorRampPalette(c("#ffffff", "#ffe8ee", "#d8f4ff", "#f6eee1", "#f2e5d5")),
            "Vegetables"=colorRampPalette(c("#457d00", "#c6c736", "#96b125", "#fea938", "#fcec9a")),
            "Bean"=colorRampPalette(c("#736731", "#99a55a", "#e5d080", "#8c683b", "#795a3b")),
            "Seafood"=colorRampPalette(c("#37412a", "#24b4ab", "#9fe3c1", "#ffffff", "#fa8072")),
            "Octopus"=colorRampPalette(c("#a6ffff", "#ffaffa", "#ee00a4", "#d79eff", "#a60033"))
            )
          return(basecolors)
        },
        .showlegend=function(location="topleft",zRange,insetv=-0.025,colo=colpalletes()){
          legval=rev(round(seq(round(zRange[1]),round(zRange[2]),length.out=10),1)) # legend values
          private$.SpectrumLegend(x = location,palette = colo,legend = legval,lwd = 14,bty = "n",inset = insetv,horiz = FALSE,cex = 1,seg.len = 0.1,xpd = NA)
        },
        .SpectrumLegend=function(x = "topright", ...,
                                palette,legend,lty = 1, lwd = 14,bty = "o",
                                adj = if (horiz) c(0.5, 0.5) else c(0, 0.5),
                                horiz = FALSE,lend = "butt",cex = 1,seg.len = 1) {
          if (is.function(palette)) {palette <- palette(256)}
          nCol <- length(palette)
          if (nCol < 1) {stop("palette has length zero")}
          
          lgd <- legend(x = x,legend = legend,horiz = horiz,adj = adj,cex = cex,
                        bty=ifelse(horiz,"n",bty),lty=0,ncol=1,seg.len=seg.len,...)
          
          textXY <- lgd$text
          Cex <- cex * par("cex")
          xyc <- graphics::xyinch(par("cin"), warn.log = FALSE)
          
          if (horiz) {
            xEnds <- range(textXY$x)
            yc <- Cex * xyc[2L]
            barSpace <- yc
            yEnds <- textXY$y[c(1, 1)] - barSpace
            
            lgd$rect$left <- lgd$rect$left + (barSpace / 2) # as not plotting lines
            lgd$rect$top <- lgd$rect$top
            lgd$rect$h <- lgd$rect$h + barSpace
            
            if (bty == "o") {
              box <- lgd$rect
              dots <- list(...)
              rect(box$left, box$top - box$h,
                   box$left + box$w, box$top,
                   # col = dots$bg, # TODO not supported - overprints text
                   lwd = dots$box.lwd, lty = dots$box.lty,
                   border = dots$box.col)
            }
          } else {
            xc <- Cex * xyc[1L]
            xEnds <- textXY$x[c(1, 1)] - xc - (xc * seg.len / 2)
            yEnds <- range(textXY$y)
          }
          
          segX <- xEnds[1] + ((xEnds[2] - xEnds[1]) * 0:nCol / nCol)
          segY <- yEnds[1] + ((yEnds[2] - yEnds[1]) * 0:nCol / nCol)
          
          nPlus1 <- nCol + 1L
          # Create overlap to avoid hairline gaps in SVG render
          epsilon <- 0.004
          epsX <- abs(segX[nPlus1] - segX[1]) * epsilon
          epsY <- abs(segY[nPlus1] - segY[1]) * epsilon
          graphics::segments(segX[-nPlus1], segY[-nPlus1],
                             segX[-1] + epsX, segY[-1] + epsY,
                             col = palette,
                             lwd = lwd, lty = lty, lend = lend)
          
          # Return:
          invisible(lgd)
        },
        .simplexPlot3 = function(varNames,surface=NULL,main=NULL,xlab,ylab,zlab,phi=30,theta=30,col=colpalletes()) {
          mat = surface
          steps=dim(mat)[1]
          x.c = varNames[1]
          y.c = varNames[2]
          z.c = varNames[3]
          if (missing(ylab)){ylab = y.c}
          if (missing(xlab)){xlab = x.c}
          if (missing(zlab)){zlab = z.c}
          
          xphi = phi%%360
          xtheta = theta%%360
          
          acc = nrow(mat)
          sca = sin(1/3 * pi)
          ncmat = ncol(mat)
          v = seq(acc, 1, length = acc)
          w = c(seq(2, acc, length = acc/2), seq(acc, 2, length = acc/2))
          mat[outer(w, v, `+`) <= acc] = NA
          if (is.function(col)) {
            nrMat <- nrow(mat)
            ncMat <- ncol(mat)
            nbcol <- 100
            color <- col(nbcol)
            matFacet = mat[-1, -1] + mat[-1, -ncmat] + mat[-acc, -1] + mat[-acc, -ncmat]
            facetcol <- cut(matFacet, nbcol)
          }else{
            color = col
            facetcol = 1}
          
          maxim = max(mat, na.rm = TRUE) * acc
          minim = min(mat, na.rm = TRUE) * acc
          per = persp(x = seq(0, acc, length = acc), y = seq(0, acc * sca, length = ncmat), mat * acc, phi = xphi, theta = xtheta, scale = TRUE, col = "transparent", 
                      border = FALSE, box = FALSE, main = main, xlab = xlab, ylab = ylab)
          lineList = contourLines(x = seq(0, acc, length = acc), y = seq(0, acc * sca, length = ncmat), mat)
          for (i in seq(along = lineList)) lines(trans3d(lineList[[i]]$x, lineList[[i]]$y, z = minim, pmat = per))
          
          legval=round(seq(minim/steps,maxim/steps,length.out=10),6) # z ticks values
          legval2 <- paste0(sprintf("% 4.2f", legval), " \u2014")
          legpos=seq(minim,maxim,length.out=10) # z ticks position
          
          if (xphi < 90) {
            lines(trans3d(x = seq(0, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
            lines(trans3d(x = seq(acc, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
            lines(trans3d(x = 0:acc, y = 0, z = maxim, pmat = per), lty = 2)
          }
          if (xtheta > 323 || xtheta < 37) {
            lines(trans3d(x = acc/2, y = acc * sca, z = minim:maxim, pmat = per), lty = 2) #x1
            lines(trans3d(x = 0, y = 0, z = minim:maxim, pmat = per), lty = 1) #x2
            lines(trans3d(x = acc, y = 0, z = minim:maxim, pmat = per), lty = 2) #x3
            text(trans3d(x = 0-2, y = 0, z =legpos[-1], pmat = per), labels = legval2[-1], lwd = 2,col = "#0A0A0A", cex = 0.7) #x2
          }
          if (xtheta > 37 && xtheta < 156) 
            lines(trans3d(x = 0, y = 0, z = minim:maxim, pmat = per), lty = 2)
          if (xtheta > 156 && xtheta < 323) {
            lines(trans3d(x = acc, y = 0, z = minim:maxim, pmat = per), lty = 2)
          }
          lines(trans3d(x = seq(0, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = minim, pmat = per), lty = 1, lwd = 2)
          lines(trans3d(x = seq(acc, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = minim, pmat = per), lty = 1, lwd = 2)
          lines(trans3d(x = 0:acc, y = 0, z = minim, pmat = per), lty = 1, lwd = 2)
          
          # aqui se escribe x1, x2 y x3
          text(trans3d(x = acc/2 + acc/50, y = acc * sca + acc * sca/50, z = minim, pmat = per), labels = xlab, lwd = 2)
          text(trans3d(x = -acc/50, y = -acc * sca/50, z = minim, pmat = per), labels = ylab, lwd = 2)
          text(trans3d(x = acc + acc/50, 0, z = minim, pmat = per), labels = zlab, cex = 1, lwd = 2)
          par(new = TRUE)
          persp(x = seq(0, acc, length = acc), y = seq(0, acc * sca, length = ncmat), mat * acc, phi = xphi, theta = xtheta, scale = TRUE, col = color[facetcol], 
                border = FALSE, box = FALSE)
          if (xphi > 0) {
            lines(trans3d(x = seq(0, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
            lines(trans3d(x = seq(acc, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
            lines(trans3d(x = 0:acc, y = 0, z = maxim, pmat = per), lty = 2)
          }
          if (xtheta > 37 && xtheta < 156) {
            lines(trans3d(x = acc/2, y = acc * sca, z = minim:maxim, pmat = per), lty = 2) #x1
            lines(trans3d(x = acc, y = 0, z = minim:maxim, pmat = per), lty = 1) #x3
            text(trans3d(x = acc+3, y = 0, z =legpos[-1], pmat = per), labels = legval2[-1], lwd = 2,col = "#0A0A0A", cex = 0.7) #x3
          }
          if (xtheta > 156 && xtheta < 323) {
            lines(trans3d(x = acc/2, y = acc * sca, z = minim:maxim, pmat = per), lty = 1) #x1
            lines(trans3d(x = 0, y = 0, z = minim:maxim, pmat = per), lty = 2) #x2
            text(trans3d(x = acc/2-3, y = acc * sca, z =legpos[-1], pmat = per), labels = legval2[-1], lwd = 2,col = "#0A0A0A", cex = 0.7) #x1
          }
          invisible(mat)
        },
        .overall4kplot=function(image97,...){
          qvars=length(self$options$vars)
          if (is.null(image97$state) || qvars >4){return(FALSE)}
          
          varNames=self$results$ternarydata$state
          doelm=image97$state
          
          desidata=private$.getDesirabilityGUI(data=self$data,options=self$options)
          overallD=private$.getdesirabilitymodels(desidata)
          modelformula <- desidata$modelformula
          models1 <- list()
          mvars=length(modelformula)
          for (i in 1:mvars) {models1[[i]] <- lm(as.formula(modelformula[i]),data=doelm)}
          
          par(mar=rep(0.1,4))
          par(mfcol=c(1,2))
          
          if (is.null(self$options$latentvar)){latent_var=1}else{latent_var=as.numeric(self$options$latentvar)} # ensure default
          if (is.null(self$options$latentplane)){latent_plane=0}else{latent_plane=as.numeric(self$options$latentplane)} # ensure default

          indx=c(1,3,2) # default
          dirX=1 # default
          pointX="up" # default
          
          if (qvars==4 && latent_var==1){indx=c(2,4,3)}
          if (qvars==4 && latent_var==2){
            indx=c(1,4,3)
            dirX=2
            pointX="right"}         
          if (qvars==4 && latent_var==3){
            indx=c(1,4,2)
            dirX=2
            pointX="right"}
          if (qvars==4 && latent_var==4){
            indx=c(1,3,2)
            dirX=2
            pointX="right"}
          
          atipX = varNames[indx[1]]
          btipX = varNames[indx[2]]
          ctipX = varNames[indx[3]]

          if ((qvars==4 && latent_var==1) || qvars==3){
            alabX <- paste0("\u2190 ",atipX," %")
            blabX <- paste0(btipX," % \u2192")
            clabX <- paste0("\u2190 ",ctipX," %")
          }else{
            alabX <- paste0(atipX," % \u2192")
            blabX <- paste0("\u2190 ",btipX," %")
            clabX <- paste0("\u2190 ",ctipX," %")     
          }
          
          axlabs=seq(0, 1-latent_plane, by = 0.1)
          gridl=10*(1-latent_plane)
          Ternary::TernaryPlot(point=pointX,atip=atipX,btip=btipX,ctip=ctipX,alab=alabX,blab=blabX,clab=clabX,clockwise = FALSE,axis.labels = axlabs, grid.lines = gridl)

          # Desirability for 3 variables
          if (qvars==3){
            FunctionToContour <- function(a,c,b,...) {
              values <- list(x11 = a, x22 = b,x33 = c) 
              simplexgrid <- data.frame(setNames(values, varNames))
              predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
              predOutcomes1 <- do.call(cbind,predictions)
              overall_d <- stats::predict(overallD,predOutcomes1)
            }
          }
          
          # Desirability for 4 variables (4 cases)
          if (qvars==4 && latent_var==1){
            FunctionToContour=function(a,c,b,...){
              ct=as.numeric(self$options$latentplane) # plane value
              values <- list(x11=ct,x22=(1-ct)*a,x33=(1-ct)*b,x44=(1-ct)*c)
              simplexgrid <- data.frame(setNames(values, varNames))
              predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
              predOutcomes1 <- do.call(cbind,predictions)
              overall_d <- stats::predict(overallD,predOutcomes1)
            }
          }

          if (qvars==4 && latent_var==2){
            FunctionToContour=function(a,c,b,...){
              ct=as.numeric(self$options$latentplane) # plane value
              values <- list(x11=(1-ct)*a,x22=ct,x33=(1-ct)*b,x44=(1-ct)*c)
              simplexgrid <- data.frame(setNames(values, varNames))
              predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
              predOutcomes1 <- do.call(cbind,predictions)
              overall_d <- stats::predict(overallD,predOutcomes1)
            }
          }

          if (qvars==4 && latent_var==3){
            FunctionToContour=function(a,c,b,...){
              ct=as.numeric(self$options$latentplane) # plane value
              values <- list(x11=(1-ct)*a,x22=(1-ct)*b,x33=ct,x44=(1-ct)*c)
              simplexgrid <- data.frame(setNames(values, varNames))
              predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
              predOutcomes1 <- do.call(cbind,predictions)
              overall_d <- stats::predict(overallD,predOutcomes1)
            }
          }

          if (qvars==4 && latent_var==4){
            FunctionToContour=function(a,c,b,...){
              ct=as.numeric(self$options$latentplane) # plane value
              values <- list(x11=(1-ct)*a,x22=(1-ct)*b,x33=(1-ct)*c,x44=ct)
              simplexgrid <- data.frame(setNames(values, varNames))
              predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
              predOutcomes1 <- do.call(cbind,predictions)
              overall_d <- stats::predict(overallD,predOutcomes1)
            }
          }

          # TODO - Ternary package must be updated - be careful FunctionToContour - AN UPDATE MAY BROKE THE PLOT
          # https://github.com/ms609/Ternary/blob/master/R/Contours.R
          # https://github.com/ms609/Ternary/blob/23ec29c6b3e37f0a91451f96dff4f080b147e3f5/R/Contours.R#L701
          
          contourval=private$.TernaryContour(FunctionToContour,col=private$.colpalletes(),direction = dirX,resolution=128L,filled=TRUE,nlevels = 20)
          zRange <- range(contourval$z,na.rm=TRUE)
          
          valuesgrid <- seq(0,1,by=1/gridl)
          coord_list <- list()
          for (val in valuesgrid) {
            coord1 <- data.frame(A = c(val, 0),B = c(0, val),C = c(1 - val, 1 - val))
            coord2 <- data.frame(A = c(1 - val, 1 - val),B = c(val, 0),C = c(0, val))
            coord3 <- data.frame(A = c(0, val),B = c(1 - val, 1 - val),C = c(val, 0))
            coord_list[[length(coord_list) + 1]] <- coord1
            coord_list[[length(coord_list) + 1]] <- coord2
            coord_list[[length(coord_list) + 1]] <- coord3
          }
          for (coords in coord_list) {Ternary::AddToTernary(lines,coords,col="lightgray",lty="dotted",lwd=0.1)}
          
          OptPointCoord=self$results$optpoint$state

          if (qvars==3){Ternary::TernaryPoints(OptPointCoord[indx],pch=21,cex=1,col="black",bg="black",lwd=2)} # optimal point
          if (qvars==4){
            datt=OptPointCoord[OptPointCoord[latent_var]==latent_plane,]
            if (dim(datt)[1]!=0){Ternary::TernaryPoints(datt[indx],pch=21,cex=1,col="black",bg="black",lwd=2)} # optimal point 4k
          }
          
          Ternary::TernaryPolygon(diag(3)) # triangular frame
          private$.showlegend(location="topright",zRange,insetv=0.01,colo=private$.colpalletes()) # Legend

          if(qvars==4){
            simplex = function(n){qr.Q(qr(matrix(1,nrow=n)),complete=TRUE)[,-1]}
            tetra = simplex(4)
            df0=rbind(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1))
            colnames(df0)=varNames
            
            df2 = t(apply(df0,1,function(x) x /sum(x)))
            #dfa=bary2cart(tetra,df2)
            dfa=df2%*%tetra
            
            x <- seq(-1,1, length.out = 10)
            y <- seq(-1,1, length.out = 10)
            z <- outer(x, y, function(a, b) a*b)
            xlim <- c(-0.2, 0.83)
            ylim <- c(-0.2, 0.83)
            zlim <- c(-0.2, 0.83)
            
            per = persp(x,y,z, phi = 135, theta = -120, scale = TRUE, col = "transparent",border = FALSE, box = FALSE, xlim = xlim, ylim = ylim, zlim = zlim)
            
            points(trans3d(x = dfa[,1], y = dfa[,2], z = dfa[,3], pmat = per), pch=19) # points
            comb <- combn(4,2)
            for(q in 1:6){
              q0=comb[1,q]
              q1=comb[2,q]
              x00=trans3d(x=dfa[q0,1],y=dfa[q0,2],z=dfa[q0,3], pmat = per)$x
              y00=trans3d(x=dfa[q0,1],y=dfa[q0,2],z=dfa[q0,3], pmat = per)$y
              x11=trans3d(x=dfa[q1,1],y=dfa[q1,2],z=dfa[q1,3], pmat = per)$x
              y11=trans3d(x=dfa[q1,1],y=dfa[q1,2],z=dfa[q1,3], pmat = per)$y
              lines(c(x00, x11), c(y00, y11), col = "black", lwd = 1)
            }
            offs=0.02
            
            for(k in 1:4){ # labels
              x1=trans3d(x=dfa[k,1],y=dfa[k,2],z=dfa[k,3], pmat = per)$x
              y1=trans3d(x=dfa[k,1],y=dfa[k,2],z=dfa[k,3], pmat = per)$y
              text(x1+offs,y1+offs, labels = colnames(df0)[k], lwd = 1,col = "#0A0A0A", cex = 1.2,font=2) #x2
            }
            
            ct=as.numeric(self$options$latentplane) # latent plane
            if (qvars==4 && latent_var==1){df1=rbind(c(ct,1-ct,0,0),c(ct,0,1-ct,0),c(ct,0,0,1-ct))}
            if (qvars==4 && latent_var==2){df1=rbind(c(1-ct,ct,0,0),c(0,ct,1-ct,0),c(0,ct,0,1-ct))}         
            if (qvars==4 && latent_var==3){df1=rbind(c(1-ct,0,ct,0),c(0,1-ct,ct,0),c(0,0,ct,1-ct))}
            if (qvars==4 && latent_var==4){df1=rbind(c(1-ct,0,0,ct),c(0,1-ct,0,ct),c(0,0,1-ct,ct))}
            
            df3 = t(apply(df1,1,function(x) x /sum(x)))
            #dfaa=bary2cart(tetra,df3)
            dfaa=df3%*%tetra
            
            comb <- combn(3,2)
            for(q in 1:3){
              print(q)
              q0=comb[1,q]
              q1=comb[2,q]
              x00=trans3d(x=dfaa[q0,1],y=dfaa[q0,2],z=dfaa[q0,3], pmat = per)$x
              y00=trans3d(x=dfaa[q0,1],y=dfaa[q0,2],z=dfaa[q0,3], pmat = per)$y
              x11=trans3d(x=dfaa[q1,1],y=dfaa[q1,2],z=dfaa[q1,3], pmat = per)$x
              y11=trans3d(x=dfaa[q1,1],y=dfaa[q1,2],z=dfaa[q1,3], pmat = per)$y
              lines(c(x00, x11), c(y00, y11), col = "red", lwd = 2,lty=6)
            }
            
            gonx=trans3d(x=dfaa[,1],y=dfaa[,2],z=dfaa[,3], pmat = per)$x
            gony=trans3d(x=dfaa[,1],y=dfaa[,2],z=dfaa[,3], pmat = per)$y
            dfaaa=cbind(gonx,gony)
            polygon(dfaaa, col = rgb(1, 0, 0, alpha = 0.3), border = NA)
          }        
          
          if(qvars==3){
            allds=private$.getdesirabilitysurface(steps=100,models=models1,FUN=overallD,idx=mvars+1) # overall
            azimut=self$options$thetaval
            latitude=self$options$phival
            private$.simplexPlot3(varNames,surface=allds,theta=azimut,phi=latitude,col=private$.colpalletes())
          }
          TRUE
        },
        .individual4kplot=function(image98,...){
          qvars=length(self$options$vars)
          if (is.null(image98$state) || qvars >4){return(FALSE)}
          
          varNames=self$results$ternarydata$state
          YieldNames=self$results$desiresponse$state
          doelm=image98$state
          
          desidata=private$.getDesirabilityGUI(data=self$data,options=self$options)
          overallD=private$.getdesirabilitymodels(desidata)
          modelformula <- desidata$modelformula
          models1 <- list()
          mvars=length(modelformula)
          for (i in 1:mvars) {models1[[i]] <- lm(as.formula(modelformula[i]),data=doelm)}
          
          # par(mar=rep(0.1,4))
          par(mfrow=c(mvars,2))
          
          if (is.null(self$options$latentvar)){latent_var=1}else{latent_var=as.numeric(self$options$latentvar)} # ensure default
          if (is.null(self$options$latentplane)){latent_plane=0}else{latent_plane=as.numeric(self$options$latentplane)} # ensure default
          
          indx=c(1,3,2) # default
          dirX=1 # default
          pointX="up" # default
          
          if (qvars==4 && latent_var==1){indx=c(2,4,3)}
          if (qvars==4 && latent_var==2){
            indx=c(1,4,3)
            dirX=2
            pointX="right"}         
          if (qvars==4 && latent_var==3){
            indx=c(1,4,2)
            dirX=2
            pointX="right"}
          if (qvars==4 && latent_var==4){
            indx=c(1,3,2)
            dirX=2
            pointX="right"}
          
          atipX = varNames[indx[1]]
          btipX = varNames[indx[2]]
          ctipX = varNames[indx[3]]
          
          if ((qvars==4 && latent_var==1) || qvars==3){
            alabX <- paste0("\u2190 ",atipX," %")
            blabX <- paste0(btipX," % \u2192")
            clabX <- paste0("\u2190 ",ctipX," %")
          }else{
            alabX <- paste0(atipX," % \u2192")
            blabX <- paste0("\u2190 ",btipX," %")
            clabX <- paste0("\u2190 ",ctipX," %")     
          }
          
          axlabs=seq(0, 1-latent_plane, by = 0.1)
          gridl=10*(1-latent_plane)
          
          for (k in 1:mvars){
            Ternary::TernaryPlot(point=pointX,atip=atipX,btip=btipX,ctip=ctipX,alab=alabX,blab=blabX,clab=clabX,clockwise = FALSE,axis.labels = axlabs, grid.lines = gridl)
          
            # Desirability for 3 variables
            if (qvars==3){
              FunctionToContour <- function(a,c,b,...) {
                values <- list(x11 = a, x22 = b,x33 = c) 
                simplexgrid <- data.frame(setNames(values, varNames))
                predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
                predOutcomes1 <- do.call(cbind,predictions)
                overall_d <- stats::predict(overallD,predOutcomes1,all=TRUE)
                overall_d2=overall_d[,k]
              }
            }
          
            # Desirability for 4 variables (4 cases)
            if (qvars==4 && latent_var==1){
              FunctionToContour=function(a,c,b,...){
                ct=as.numeric(self$options$latentplane) # plane value
                values <- list(x11=ct,x22=(1-ct)*a,x33=(1-ct)*b,x44=(1-ct)*c)
                simplexgrid <- data.frame(setNames(values, varNames))
                predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
                predOutcomes1 <- do.call(cbind,predictions)
                overall_d <- stats::predict(overallD,predOutcomes1,all=TRUE)
                overall_d2=overall_d[,k]
              }
            }
          
            if (qvars==4 && latent_var==2){
              FunctionToContour=function(a,c,b,...){
                ct=as.numeric(self$options$latentplane) # plane value
                values <- list(x11=(1-ct)*a,x22=ct,x33=(1-ct)*b,x44=(1-ct)*c)
                simplexgrid <- data.frame(setNames(values, varNames))
                predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
                predOutcomes1 <- do.call(cbind,predictions)
                overall_d <- stats::predict(overallD,predOutcomes1,all=TRUE)
                overall_d2=overall_d[,k]
              }
            }
          
            if (qvars==4 && latent_var==3){
              FunctionToContour=function(a,c,b,...){
                ct=as.numeric(self$options$latentplane) # plane value
                values <- list(x11=(1-ct)*a,x22=(1-ct)*b,x33=ct,x44=(1-ct)*c)
                simplexgrid <- data.frame(setNames(values, varNames))
                predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
                predOutcomes1 <- do.call(cbind,predictions)
                overall_d <- stats::predict(overallD,predOutcomes1,all=TRUE)
                overall_d2=overall_d[,k]
              }
            }
          
            if (qvars==4 && latent_var==4){
              FunctionToContour=function(a,c,b,...){
                ct=as.numeric(self$options$latentplane) # plane value
                values <- list(x11=(1-ct)*a,x22=(1-ct)*b,x33=(1-ct)*c,x44=ct)
                simplexgrid <- data.frame(setNames(values, varNames))
                predictions <- lapply(models1, stats::predict, newdata = simplexgrid)
                predOutcomes1 <- do.call(cbind,predictions)
                overall_d <- stats::predict(overallD,predOutcomes1,all=TRUE)
                overall_d2=overall_d[,k]
              }
            }
          
            # TODO - Ternary package must be updated - be careful FunctionToContour - AN UPDATE MAY BROKE THE PLOT
            # https://github.com/ms609/Ternary/blob/master/R/Contours.R
            # https://github.com/ms609/Ternary/blob/23ec29c6b3e37f0a91451f96dff4f080b147e3f5/R/Contours.R#L701
          
            contourval=private$.TernaryContour(FunctionToContour,col=private$.colpalletes(),direction = dirX,resolution=128L,filled=TRUE,nlevels = 20)
            zRange <- range(contourval$z,na.rm=TRUE)
          
            valuesgrid <- seq(0,1,by=1/gridl)
            coord_list <- list()
            for (val in valuesgrid) {
              coord1 <- data.frame(A = c(val, 0),B = c(0, val),C = c(1 - val, 1 - val))
              coord2 <- data.frame(A = c(1 - val, 1 - val),B = c(val, 0),C = c(0, val))
              coord3 <- data.frame(A = c(0, val),B = c(1 - val, 1 - val),C = c(val, 0))
              coord_list[[length(coord_list) + 1]] <- coord1
              coord_list[[length(coord_list) + 1]] <- coord2
              coord_list[[length(coord_list) + 1]] <- coord3
            }
            for (coords in coord_list) {Ternary::AddToTernary(lines,coords,col="lightgray",lty="dotted",lwd=0.1)}
          
            OptPointCoord=self$results$optpoint$state
          
            if (qvars==3){Ternary::TernaryPoints(OptPointCoord[indx],pch=21,cex=1,col="black",bg="black",lwd=2)} # optimal point
            if (qvars==4){
              datt=OptPointCoord[OptPointCoord[latent_var]==latent_plane,]
              if (dim(datt)[1]!=0){Ternary::TernaryPoints(datt[indx],pch=21,cex=1,col="black",bg="black",lwd=2)} # optimal point 4k
            }
          
            Ternary::TernaryPolygon(diag(3)) # triangular frame
            mtext(paste0("Individual desirability for ",YieldNames[k]),side=1,line=1,cex=1,col="black")
            private$.showlegend(location="topright",zRange,insetv=0.01,colo=private$.colpalletes()) # Legend
          
            if(qvars==4){
              simplex = function(n){qr.Q(qr(matrix(1,nrow=n)),complete=TRUE)[,-1]}
              tetra = simplex(4)
              df0=rbind(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1))
              colnames(df0)=varNames
            
              df2 = t(apply(df0,1,function(x) x /sum(x)))
              #dfa=bary2cart(tetra,df2)
              dfa=df2%*%tetra
            
              x <- seq(-1,1, length.out = 10)
              y <- seq(-1,1, length.out = 10)
              z <- outer(x, y, function(a, b) a*b)
              xlim <- c(-0.2, 0.83)
              ylim <- c(-0.2, 0.83)
              zlim <- c(-0.2, 0.83)
            
              per = persp(x,y,z, phi = 135, theta = -120, scale = TRUE, col = "transparent",border = FALSE, box = FALSE, xlim = xlim, ylim = ylim, zlim = zlim)
            
              points(trans3d(x = dfa[,1], y = dfa[,2], z = dfa[,3], pmat = per), pch=19) # points
              comb <- combn(4,2)
              for(q in 1:6){
                q0=comb[1,q]
                q1=comb[2,q]
                x00=trans3d(x=dfa[q0,1],y=dfa[q0,2],z=dfa[q0,3], pmat = per)$x
                y00=trans3d(x=dfa[q0,1],y=dfa[q0,2],z=dfa[q0,3], pmat = per)$y
                x11=trans3d(x=dfa[q1,1],y=dfa[q1,2],z=dfa[q1,3], pmat = per)$x
                y11=trans3d(x=dfa[q1,1],y=dfa[q1,2],z=dfa[q1,3], pmat = per)$y
                lines(c(x00, x11), c(y00, y11), col = "black", lwd = 1)
              }
              offs=0.02
            
              for(k in 1:4){ # labels
                x1=trans3d(x=dfa[k,1],y=dfa[k,2],z=dfa[k,3], pmat = per)$x
                y1=trans3d(x=dfa[k,1],y=dfa[k,2],z=dfa[k,3], pmat = per)$y
                text(x1+offs,y1+offs, labels = colnames(df0)[k], lwd = 1,col = "#0A0A0A", cex = 1.2,font=2) #x2
              }
            
              ct=as.numeric(self$options$latentplane) # latent plane
              if (qvars==4 && latent_var==1){df1=rbind(c(ct,1-ct,0,0),c(ct,0,1-ct,0),c(ct,0,0,1-ct))}
              if (qvars==4 && latent_var==2){df1=rbind(c(1-ct,ct,0,0),c(0,ct,1-ct,0),c(0,ct,0,1-ct))}         
              if (qvars==4 && latent_var==3){df1=rbind(c(1-ct,0,ct,0),c(0,1-ct,ct,0),c(0,0,ct,1-ct))}
              if (qvars==4 && latent_var==4){df1=rbind(c(1-ct,0,0,ct),c(0,1-ct,0,ct),c(0,0,1-ct,ct))}
            
              df3 = t(apply(df1,1,function(x) x /sum(x)))
              #dfaa=bary2cart(tetra,df3)
              dfaa=df3%*%tetra
            
              comb <- combn(3,2)
              for(q in 1:3){
                print(q)
                q0=comb[1,q]
                q1=comb[2,q]
                x00=trans3d(x=dfaa[q0,1],y=dfaa[q0,2],z=dfaa[q0,3], pmat = per)$x
                y00=trans3d(x=dfaa[q0,1],y=dfaa[q0,2],z=dfaa[q0,3], pmat = per)$y
                x11=trans3d(x=dfaa[q1,1],y=dfaa[q1,2],z=dfaa[q1,3], pmat = per)$x
                y11=trans3d(x=dfaa[q1,1],y=dfaa[q1,2],z=dfaa[q1,3], pmat = per)$y
                lines(c(x00, x11), c(y00, y11), col = "red", lwd = 2,lty=6)
              }
            
              gonx=trans3d(x=dfaa[,1],y=dfaa[,2],z=dfaa[,3], pmat = per)$x
              gony=trans3d(x=dfaa[,1],y=dfaa[,2],z=dfaa[,3], pmat = per)$y
              dfaaa=cbind(gonx,gony)
              polygon(dfaaa, col = rgb(1, 0, 0, alpha = 0.3), border = NA)
            }        
          
            if(qvars==3){
              allds=private$.getdesirabilitysurface(steps=100,models=models1,FUN=overallD,idx=k) # individuals
              azimut=self$options$thetaval
              latitude=self$options$phival
              private$.simplexPlot3(varNames,surface=allds,theta=azimut,phi=latitude,col=private$.colpalletes())
            }
          }
          TRUE
        },
        .namingissue=function(xnames){
          xnames <- make.names(xnames)
          xnames <- gsub("\\.","",xnames) # clean
          return(xnames)
        },
        .getdesirabilitymodels=function(desidata,scaledata,lowScaledata,highScaledata){
          lowdata <- desidata$lowdata
          highdata <- desidata$highdata
          targetdata <- desidata$targetdata
          desiratype <- desidata$desiratype
          dvars=length(desiratype)
          desirability_list=list()
          
          if (missing(scaledata)){scaledata=rep(0.1,dvars)} # ToDo
          if (missing(lowScaledata)){lowScaledata=rep(1,dvars)} # ToDo
          if (missing(highScaledata)){highScaledata=rep(1,dvars)} # ToDo
          
          for (j in 1:dvars){
            if (desiratype[j]=="dTarget"){desirability_list[[j]]=desirability::dTarget(low=lowdata[j],high=highdata[j],target=targetdata[j],scale=0.1)}
            if (desiratype[j]=="dMax"){desirability_list[[j]]=desirability::dMax(low=lowdata[j],high=highdata[j],scale=0.1)}
            if (desiratype[j]=="dMin"){desirability_list[[j]]=desirability::dMin(low=lowdata[j],high=highdata[j],scale=0.1)}
          }
          overallD <- do.call(desirability::dOverall, desirability_list)
          return(overallD)
        }) # Close - List
) # Close - R6::R6Class
