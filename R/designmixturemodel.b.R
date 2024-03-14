designmixturemodelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "designmixturemodelClass",
    inherit = designmixturemodelBase,
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
            
            if (is.null(self$options$vars) || is.null(self$options$yield) || qvars<3){return()}
            self$results$cols$setState(qvars)

            varNames <- self$options$vars
            responseName <- self$options$yield

            vardata=self$data[varNames]
            responsedata=self$data[responseName]
            
             # solve naming issues
            varNames <- make.names(varNames)
            varNames <- gsub("\\.","",varNames) # clean
            responseName <- make.names(responseName)
            responseName <- gsub("\\.","",responseName) # clean

            nda=length(vardata)
            vardata2= data.frame(matrix(nrow = dim(vardata)[1], ncol = dim(vardata)[2]))
            for (k in 1:nda){
              vardata2[k]=as.numeric(as.character(unlist(vardata[k])))
            }
            
            colnames(vardata2)=varNames
            colnames(responsedata)=responseName
            
            doe=cbind(vardata2,responsedata)
            doersm=doe
            doelm=doe
            zk=dim(doelm)[2]-1
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
              
            selmodel=as.numeric(self$options$choosemodel)
            modelformula=private$.getformula(responseName,varNames,model=selmodel,scheffe=-1)
            modely <- lm(as.formula(modelformula),data=doelm)

            # Residual Plot
            image2 <- self$results$residualplot
            image2$setState(doelm)

            coe=modely$coefficients
            coe[is.na(coe)] <- 0 # NA correction
            
            # Mixture Plot
            #image5 <- self$results$mixtureplot
            #image5$setState(coe)
            #self$results$ternarydata$setState(doelm)
            
            # image5$setSize(960,480*npe)
            
            # Pruebas Mixture 4k plot
            image6 <- self$results$mixture4kplot
            image6$setState(coe)
            self$results$ternarydata$setState(doelm)
            
            # self$results$debugger$setContent(scaling)
            
            # Main effect plot (cox direction)
            image3 <- self$results$coxplot
            image3$setState(doelm)
            
            #-----------------------
            # Mixture design Table - Create table by code
            #-----------------------       
            
            Predicted=modely$fitted.values
            Residu=modely$residuals
            cod=doelm[1:(length(doelm)-1)] # x1, x2, x3...
            doetable=cbind(vardata2,cod,responsedata,Predicted,Residu)            
            
            # Fill columns
            table0 <- self$results$doetable2
            for (i in 1:qvars){table0$addColumn(name=paste0("v",i), title=varNames[i], type='number')}
            for (i in 1:qvars){table0$addColumn(name=paste0("cod",i), title=paste0("x",i), type='number')}
            
            table0$addColumn(name='res1', title=responseName,type='number')
            table0$addColumn(name='pre1', title='Predicted',type='number')
            table0$addColumn(name='rdual1', title='Residuals',type='number')
            
            # Fill Rows
            num_rows <- nrow(doetable)
            for (i in 1:num_rows){
              row_values <- c(sapply(1:qvars, function(j) doetable[i,j]),
                              sapply(1:qvars, function(j) doetable[i,j+qvars]))
              
              col_names <- c(paste0("v",1:qvars),paste0("cod",1:qvars))
              names(row_values) <- col_names
              
              row_values$res1 <- doetable[i,2*qvars+1]
              row_values$pre1 <- doetable[i,2*qvars+2]
              row_values$rdual1 <- doetable[i,2*qvars+3]
              
              row=as.list(row_values)
              table0$addRow(rowKey=i,row)
            }
            
            #-----------------------
            # end table
            #-----------------------          
            

            #-----------------------
            # Estimation Table - Create table by code
            #-----------------------    
            est=summary(modely)
            estidata=est$coefficients
            
            # Fill columns
            table1 <- self$results$estitable

            table1$addColumn(name='nam1', title="Model Terms",type='text')
            table1$addColumn(name='coe1', title=colnames(estidata)[1],type='number')
            table1$addColumn(name='sse1', title=colnames(estidata)[2],type='number')
            table1$addColumn(name='stu1', title=colnames(estidata)[3],type='number')
            table1$addColumn(name='pva1', title=colnames(estidata)[4],type='number',format='zto,pvalue')
            
            # Fill Rows
            num_rows <- nrow(estidata)
            for (i in 1:num_rows){
              row_values$nam1 <- rownames(estidata)[i]
              row_values$coe1 <- estidata[i,1]
              row_values$sse1 <- estidata[i,2]
              row_values$stu1 <- estidata[i,3]
              row_values$pva1 <- estidata[i,4]
              
              row=as.list(row_values)
              table1$addRow(rowKey=i,row)
            }
            #-----------------------
            # end table
            #-----------------------          

            modelformula2=private$.getformula(responseName,varNames,model=selmodel,scheffe=NULL) # with intercept()
            modely2 <- lm(as.formula(modelformula2),data=doelm)
            
            xtabla=summary(modely2) 
            Rsquare=xtabla$r.squared
            adjR=xtabla$adj.r.squared
            #-----------------------
            # ANOVA Table - Create table by code
            #-----------------------                
            anova <- stats::anova(modely)
            n0=dim(anova[1])[1]
            todf <- sum(anova[1])
            redf <- anova[n0,1] # Residual error df
            ovdf <- todf-redf # overall model df
            tosq <- sum(anova[2])
            resq <- anova[n0,2]
            ovsq <- tosq-resq # overal model ssq
            MSE <- resq/redf # mean square error
            ovmq <- ovsq/ovdf # mean square
            Fval <- ovmq/MSE # F stats
            ovpv <- pf(Fval,df1=ovdf,df2=redf,lower.tail=FALSE) # overall model p_value
            # p_value <- 1 - pf(Fval, ovdf, redf)
            
            # Fill columns
            table2 <- self$results$anovatable
            
            table2$addColumn(name='nam2', title="Source",type='text')
            table2$addColumn(name='doe2', title="Dof",type='number')
            table2$addColumn(name='ssq2', title="Sum of Squares",type='number')
            table2$addColumn(name='msq2', title="Mean Square",type='number')
            table2$addColumn(name='fva2', title="F-value",type='number')
            table2$addColumn(name='pva2', title="Pr(>F)",type='number',format='zto,pvalue')
            
            # Fill Rows
            row1=list()
            row1$nam2 <- "Overall Model"
            row1$doe2 <- ovdf
            row1$ssq2 <- ovsq
            row1$msq2 <- ovmq
            row1$fva2 <- Fval
            row1$pva2 <- ovpv
            row1=as.list(row1)
            table2$addRow(rowKey=1,row1)
            
            row2=list()
            row2$nam2 <- "Residual"
            row2$doe2 <- redf
            row2$ssq2 <- resq
            row2$msq2 <- MSE
            row2=as.list(row2)
            table2$addRow(rowKey=2,row2)   
            
            row3=list()
            row3$nam2 <- "Total"
            row3$doe2 <- todf
            row3$ssq2 <- tosq
            row3=as.list(row3)
            table2$addRow(rowKey=3,row3)  
            
            cadena <- paste0("Multiple R-Squared: ", format(Rsquare, digits = 3), "\n","Adjusted R-Squared: ", format(adjR, digits = 3), "\n","Mean Square Error: ", format(MSE, digits = 3))
            table2$setNote('Note',cadena)
            #-----------------------
            # end table
            #-----------------------          

            #-----------------------
            # Predicted model Table - Create simple
            #-----------------------    
            doelm_mix=doelm # x1, x2, x3
            colnames(doelm_mix)[1:qvars]=paste0("x",1:qvars)
            modelformula3=private$.getformula(responseName,paste0("x",1:qvars),model=selmodel,scheffe=-1)
            modely_mix <- lm(as.formula(modelformula3),data=doelm_mix)
            modelbaseL <- coef(modely_mix)[names(coef(modely_mix))] # x1,x2,x3...
            formulatexL=private$.formulatable(modelbaseL,responseName)
            
            table <- self$results$maintable
            table$setRow(rowNo=1, values=list(equ1=formulatexL,Rsq1=Rsquare,Ad1=adjR,Mse1=MSE))
            chaint <- "cubic(x1,x2) = x1·x2·(x1-x2) \n and \n x2·x3·I(x1^2) = (x1^2)·x2·x3"
            table$setNote('Note',chaint)
            #-----------------------
            # End Table
            #-----------------------  
            
            #-----------------------
            # Scaling Table - Create table by code
            #-----------------------    
            
            # Fill columns
            table4 <- self$results$scalingtable
            
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
            #-----------------------
            # end table
            #-----------------------        
            
            #------------------------
            # Check Status
            #------------------------
            #object1=image2$state
            #object2=image3$state
            #object3=image5$state
            #object4=self$results$ternarydata$state
            #xh1=length(serialize(object1,connection=NULL))
            #xh2=length(serialize(object2,connection=NULL))
            #xh3=length(serialize(object3,connection=NULL))
            #xh4=length(serialize(object4,connection=NULL))
            #self$results$debugger$setContent(cbind(xh1,xh2,xh3,xh4))
            
        },
        .residualplot=function(image2,...){
          qvars=length(self$options$vars)
          if (is.null(image2$state) || is.null(self$options$yield) || qvars<2){return(FALSE)}
          
          doelm=image2$state
          varNames=colnames(doelm)[1:qvars]
          responseName=colnames(doelm)[qvars+1]
          selmodel=as.numeric(self$options$choosemodel)
          
          modelformula=private$.getformula(responseName,varNames,model=selmodel,scheffe=-1)
          modely <- lm(as.formula(modelformula),data=doelm)
          
          colcol <- rep("#6B9DE8",dim(doelm)[1])
          colcol[duplicated(doelm[,1:qvars])]<- "#E6AC40" #color label replicates
          
          plot(predict(modely), rstudent(modely),ylim=c(-4,4),
               xlab=expression( paste("predicted response ", hat(y)) ),
               ylab="studentized residuals",type="n",
               main=expression(paste("(",frac(paste("y - ",hat(y)),sigma),") ~ ",hat(y))))
          text(predict(modely), rstudent(modely),label=1:nrow(doelm), col=colcol)
          abline(h=c(-2,0,2),lty=c(2,1,2))
          grid(nx = NULL, ny = NULL, lty = 3, col = "gray", lwd = 1)
          TRUE
        },
        .coxplot=function(image3,...){ # Main Effect plot
          if (is.null(image3$state) || is.null(self$options$yield)){return(FALSE)}
          
          plotdata=image3$state
          qvars=length(self$options$vars)
          selmodel=as.numeric(self$options$choosemodel)
          if (is.null(self$options$choosevars)){selvars="original"}else{selvars=self$options$choosevars} # ensure default
          
          if (selvars=="original"){
            varNames=colnames(plotdata)[1:qvars]
          }else{
            varNames=paste0("x",1:qvars)
            colnames(plotdata)[1:qvars]=varNames
            }
          
          responseName=colnames(plotdata)[qvars+1]
          
          swit <- self$options$xaxis
          par(mfcol=c(1,2))
          
          if(qvars==4){
            simplex = function(n){qr.Q(qr(matrix(1,nrow=n)),complete=TRUE)[,-1]}
            tetra = simplex(4)
            df0=rbind(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1),c(0,1/3,1/3,1/3),c(1/3,0,1/3,1/3),c(1/3,1/3,0,1/3),c(1/3,1/3,1/3,0),c(1/4,1/4,1/4,1/4))
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
            
            per = persp(x,y,z, phi = 20, theta = 120, scale = TRUE, col = "transparent",border = FALSE, box = FALSE, xlim = xlim, ylim = ylim, zlim = zlim)
            
            points(trans3d(x = dfa[,1], y = dfa[,2], z = dfa[,3], pmat = per), pch=19)
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
            
            for(k in 1:4){
              x0=trans3d(x=dfa[4+k,1],y=dfa[4+k,2],z=dfa[4+k,3], pmat = per)$x
              y0=trans3d(x=dfa[4+k,1],y=dfa[4+k,2],z=dfa[4+k,3], pmat = per)$y
              x1=trans3d(x=dfa[k,1],y=dfa[k,2],z=dfa[k,3], pmat = per)$x
              y1=trans3d(x=dfa[k,1],y=dfa[k,2],z=dfa[k,3], pmat = per)$y
              arrows(x0,y0,x1,y1,col ="red", length = 0.2,lwd = 2)
              text(x1+offs,y1+offs, labels = colnames(df0)[k], lwd = 1,col = "#0A0A0A", cex = 1.2,font=2) #x2
            }
          }
            
          if(qvars!=4){
            atipX=varNames[1]
            btipX=varNames[3]
            ctipX=varNames[2]
            alabX <- paste0("\u2190 ",atipX," %")
            blabX <- paste0(btipX," % \u2192")
            clabX <- paste0("\u2190 ",ctipX," %")

            #TernaryPlot(point="up",atip="x1",btip="x3",ctip="x2",alab="\u2190 x1 %",blab="x3 % \u2192",clab="\u2190 x2 %",clockwise = FALSE)
            Ternary::TernaryPlot(point="up",atip=atipX,btip=btipX,ctip=ctipX,alab=alabX,blab=blabX,clab=clabX,clockwise = FALSE)
            
            Ternary::TernaryArrows(c(0,0.5,0.5),c(1,0,0),col="red",length=0.15, lwd=2)
            Ternary::TernaryArrows(c(0.5,0,0.5),c(0,1,0),col="red",length=0.15, lwd=2)
            Ternary::TernaryArrows(c(0.5,0.5,0),c(0,0,1),col="red",length=0.15, lwd=2)
            Ternary::AddToTernary(points, list(x1=rep(0.33,3),x2=rep(0.33,3),x3=rep(0.33,3)),pch=16,col="black",cex=1)
          }
            
          modelformula=private$.getformula(responseName,varNames,model=selmodel,scheffe=-1)
          res <- lm(as.formula(modelformula),data=plotdata)
          
          n=qvars
          x=seq(from = 0, to = 1, by = 0.002)
          centroid=1/n
          z=(1-x)/(n-1)
          coe=res$coefficients
          y1 <- matrix(z, ncol = n, nrow = length(x))
          for (i in 1:n){
            v1 <- data.frame(matrix(z, ncol = n, nrow = length(x)))
            v1[,i] = x
            colnames(v1) <- varNames
            y1[,i]=predict(res,newdata=v1)
          }
          
          if (swit=="mixtureproportion"){
            for (i in 1:n){
              if (i==1){
                plot(x, y1[,1], type = "l", main = "Mixture Effect Plot (Cox direction)", xlab = "Mixture proportions", ylab="Predicted Response", ylim = c(min(y1),max(y1)),col = rainbow(n)[i],lty = 2,lwd=2)
              }else{lines(x, y1[,i], col = rainbow(n)[i], lty = 2,lwd=2)}
              #text(0.95, y1[length(x), i], varNames[i], pos = 4, offset = -0.01, col = "black")
              text(0, y1[1,i], varNames[i], pos = 4, offset = 0.03, col = "black")
            }
            legend("bottomleft", legend = varNames, col = rainbow(n), lty = 2,lwd=2)
            axis(side = 1, at = seq(0, 1, by = 0.1))
            grid(nx = NULL, ny = NULL, lty = 3, col = "gray", lwd = 1)
            abline(v = c(0.1,0.3,0.5,0.7,0.9), lty = 3, col = "gray", lwd = 1)
          }else{
            for (i in 1:n){
              if (i==1){
                plot(x-centroid, y1[,1], type = "l", main = "Mixture Effect Plot (Cox direction)", xlab = "Deviation from centroid", ylab="Predicted Response", ylim = c(min(y1),max(y1)),col = rainbow(n)[i],lty = 2,lwd=2)
              }else{lines(x-centroid, y1[,i], col = rainbow(n)[i], lty = 2,lwd=2)}
              #text(0.95-centroid, y1[length(x), i], varNames[i], pos = 4, offset = -0.01, col = "black")
              text(0-centroid, y1[1,i], varNames[i], pos = 4, offset = 0.03, col = "black")
            }
            legend("bottomleft", legend = varNames, col = rainbow(n), lty = 2,lwd=2)
            axis(side = 1, at = seq(-0.4,0.9, by = 0.10))
            grid(nx = NULL, ny = NULL, lty = 3, col = "gray", lwd = 1)
            abline(v = c(-0.3,-0.1,0.1,0.3,0.5,0.7), lty = 3, col = "gray", lwd = 1)
          }
          TRUE
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
          private$.SpectrumLegend(x = location,palette = colo,legend = legval,lwd = 14,
                                    bty = "n",inset = insetv,horiz = FALSE,cex = 1,seg.len = 0.1,xpd = NA)
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
        .wirePlot3 = function(varNames,responseName,data=NULL,main=NULL,xlab,ylab,zlab,form=NULL,phi=30,theta=30,steps=100,factors=NULL,legRange=NULL,col=colpalletes()) {
          # out = list() # .wirePlot3 = function(x,y,z,response,data=NULL,main=NULL,xlab,ylab,zlab,form=NULL,phi=30,theta=30,steps=100,factors=NULL,legRange=NULL,col=colpalletes()) {
          mdo = data
          x.c = varNames[1]
          y.c = varNames[2]
          z.c = varNames[3]
          r.c = responseName
          if (missing(ylab)) 
            ylab = y.c
          if (missing(xlab)) 
            xlab = x.c
          if (missing(zlab)) 
            zlab = z.c
          if (missing(form)) 
            form="yield ~ -1+x1+x2+x3"

          xphi = phi%%360
          xtheta = theta%%360
          
          lm.1 = lm(as.formula(form), data = mdo)
          dcList = vector(mode = "list", length = length(names(mdo)))
          names(dcList) = names(mdo)[1:4]
          dcList[1:length(names(mdo))] = 0
          
          help.predict = function(a, b, x.c, y.c, lm.1, ...) {
            dcList[[x.c]] = 2 * b/sqrt(3)
            dcList[[y.c]] = 1 - (2 * b/sqrt(3)) - (a - b/sqrt(3))
            dcList[[z.c]] = a - b/sqrt(3)
            temp = do.call(data.frame, dcList)
            invisible(predict(lm.1, temp))
          }
          
          a = seq(0, 1, length = steps)
          b = seq(0, sqrt(3)/2, length = steps)
          mat = outer(a, b, help.predict, x.c, y.c, lm.1) # esto calcula la superficie
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
        .getformula=function(responseName,varNames,model=1,scheffe=-1){
          linear <- as.list(varNames)
          quadratic=NULL
          cubic=NULL
          full=NULL
          quartic=NULL # Special quartic (Cornell p.71)
          if (model>1){
            combi<- combn(varNames, 2, simplify = TRUE)
            quadratic <- apply(combi,2,function(x) as.list(x))}
          if (model>2){
            combi<- combn(varNames, 3, simplify = TRUE)
            cubic <- apply(combi,2,function(x) as.list(x))}
          if (model==3){
            combi<- combn(varNames, 2, simplify = TRUE)
            full <- apply(combi, 2, function(row) {list(paste("cubic(", paste(row, collapse = ","), ")", sep = ""))})}
          if (model==1.5){
            combi<- combn(rev(varNames),(length(varNames)-1), simplify = TRUE)
            quartic = apply(rbind(varNames,varNames,combi),2,function(x) as.list(x))}
          
          terms0 = c(scheffe,linear,quadratic,cubic,full,quartic)
          a=jmvcore::composeFormula(responseName,terms0) 
          a <- gsub("`", "", a) # clean
          return(a)
        },
        .formulatable=function(modely,responsy){
          modeltex=modely
          namestex <- names(modeltex)
          namestex <- gsub("Intercept","", namestex)
          namestex <- gsub(":", "\u00b7", namestex)
          names(modeltex) <- namestex
          sign1 <- sign(modeltex)
          sign1tex <- ifelse(sign1 == 1, "+", "-")
          modeltexcad <- as.character(round(modeltex,4))
          pointss <- sapply(strsplit(modeltexcad, "\\."), function(x) ifelse(length(x) > 1, nchar(x[2]), 0))
          spf=paste0("%.",max(pointss),"f",sep="")
          modeltextt <- sprintf(spf, abs(modeltex))
          modeltextt[1] <- sprintf(spf,modeltex[1])
          formula0 <- paste0(responsy,"=",modeltextt[1],"\u00b7",names(modeltex)[1],paste0(sign1tex[-1],modeltextt[-1],"\u00b7",names(modeltex)[-1], collapse = "")) # Scheffe
          return(formula0)
        },
        .mixture4kplot=function(image6,...){
          qvars=length(self$options$vars)
          if (is.null(image6$state) || is.null(self$options$yield) || qvars >4){return(FALSE)}
          
          selmodel=as.numeric(self$options$choosemodel)
          dat=self$results$ternarydata$state
          
          if (is.null(self$options$choosevars)){selvars="original"}else{selvars=self$options$choosevars} # ensure default
          if (selvars=="original"){
            varNames=colnames(dat)[1:qvars]
          }else{
            varNames=paste0("x",1:qvars)
            colnames(dat)[1:qvars]=varNames
          }
          responseName=colnames(dat)[qvars+1]
          modelformula=private$.getformula(responseName,varNames,model=selmodel,scheffe=-1)
          
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
          #Ternary::TernaryPlot(point="up",atip="x1",btip="x3",ctip="x2",alab="\u2190 x1 %",blab="x3 % \u2192",clab="\u2190 x2 %",clockwise = FALSE)
          
          # Models for 3 variables
          if (selmodel==1 && qvars==3){
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              coe[1]*a+coe[2]*b+coe[3]*c}
          }
          
          if (selmodel==2 && qvars==3){
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              coe[1]*a+coe[2]*b+coe[3]*c+coe[4]*a*b+coe[5]*a*c+coe[6]*b*c}
          }
          
          # I(x1*x2*(x1-x2))+I(x1*x3*(x1-x3))+I(x2*x3*(x2-x3))
          if (selmodel==3 && qvars==3){  # Full Cubic 
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              coe[1]*a+coe[2]*b+coe[3]*c+coe[4]*a*b*(a-b)+coe[5]*a*c*(a-c)+coe[6]*b*c*(b-c)+coe[7]*a*b+coe[8]*a*c+coe[9]*b*c+coe[10]*a*b*c}
          } 
          
          if (selmodel==4 && qvars==3){ # Special Cubic
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              coe[1]*a+coe[2]*b+coe[3]*c+coe[4]*a*b+coe[5]*a*c+coe[6]*b*c+coe[7]*a*b*c}
          } 
          
          # Special Quartic x2:x3:I(x1^2) x1:x3:I(x2^2) x1:x2:I(x3^2) 
          if (selmodel==1.5 && qvars==3){ 
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              coe[1]*a+coe[2]*b+coe[3]*c+coe[4]*a*b+coe[5]*a*c+coe[6]*b*c+coe[7]*a*a*b*c+coe[8]*b*b*a*c+coe[9]*c*c*a*b}
          } 
          
          # Models for 4 variables
          if (selmodel==1 && qvars==4){
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              latent_var=as.numeric(self$options$latentvar) # latent var
              xx=c(1,2,3,4)
              xx=xx[-latent_var]
              ct=as.numeric(self$options$latentplane) # latent plane
              (1-ct)*coe[xx[1]]*a+(1-ct)*coe[xx[2]]*b+(1-ct)*coe[xx[3]]*c+coe[latent_var]*ct}
          }
          
          if (selmodel==2 && qvars==4){
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              # linear
              latent_var=as.numeric(self$options$latentvar) # latent var
              xx=c(1,2,3,4)
              xx=xx[-latent_var]
              ct=as.numeric(self$options$latentplane) # latent plane
              # quadratic
              vz=c(1,2,3,4)
              cz=combn(vz,2,simplify=TRUE)
              foo1=which(cz==latent_var,arr.ind=TRUE)[,2] # latent
              foo2=setdiff(1:ncol(cz),foo1) # rest
              indx1=foo1+4 # latent
              indx2=foo2+4 # rest
              (1-ct)*coe[xx[1]]*a+(1-ct)*coe[xx[2]]*b+(1-ct)*coe[xx[3]]*c+coe[latent_var]*ct+(1-ct)*coe[indx1[1]]*a*ct+(1-ct)*coe[indx1[2]]*b*ct+(1-ct)*coe[indx1[3]]*c*ct+(1-ct)^2*coe[indx2[1]]*a*b+(1-ct)^2*coe[indx2[2]]*a*c+(1-ct)^2*coe[indx2[3]]*b*c}
          }  
          
          if (selmodel==4 && qvars==4){
            FunctionToContour=function(a,c,b,...){
              coe=image6$state
              # linear
              latent_var=as.numeric(self$options$latentvar) # latent var
              xx=c(1,2,3,4)
              xx=xx[-latent_var]
              ct=as.numeric(self$options$latentplane) # latent plane
              # quadratic
              vz=c(1,2,3,4)
              cz=combn(vz,2,simplify=TRUE)
              foo1=which(cz==latent_var,arr.ind=TRUE)[,2] # latent
              foo2=setdiff(1:ncol(cz),foo1) # rest
              indx1=foo1+4 # latent
              indx2=foo2+4 # rest
              # special cubic
              cc=combn(vz,3,simplify=TRUE)
              foo3=which(cc==latent_var,arr.ind=TRUE)[,2] # latent
              foo4=setdiff(1:ncol(cc),foo3) # rest
              indx3=foo3+10 # latent
              indx4=foo4+10 # rest
              (1-ct)*coe[xx[1]]*a+(1-ct)*coe[xx[2]]*b+(1-ct)*coe[xx[3]]*c+coe[latent_var]*ct+(1-ct)*coe[indx1[1]]*a*ct+(1-ct)*coe[indx1[2]]*b*ct+(1-ct)*coe[indx1[3]]*c*ct+(1-ct)^2*coe[indx2[1]]*a*b+(1-ct)^2*coe[indx2[2]]*a*c+(1-ct)^2*coe[indx2[3]]*b*c+(1-ct)^2*coe[indx3[1]]*a*b*ct+(1-ct)^2*coe[indx3[2]]*a*c*ct+(1-ct)^2*coe[indx3[3]]*b*c*ct+(1-ct)^3*coe[indx4[1]]*a*b*c}
          }    
  
          if (selmodel==3 && qvars==4){return(FALSE)} # Not implemented yet
          if (selmodel==1.5 && qvars==4){return(FALSE)} # Not implemented yet

          # TODO - Ternary package must be updated - be careful FunctionToContour - AN UPDATE MAY BROKE THE PLOT
          # https://github.com/ms609/Ternary/blob/master/R/Contours.R
          # https://github.com/ms609/Ternary/blob/23ec29c6b3e37f0a91451f96dff4f080b147e3f5/R/Contours.R#L701
          
          contourval=private$.TernaryContour(FunctionToContour,
                                             col=private$.colpalletes(),
                                             direction = dirX,
                                             resolution=128L,
                                             filled=TRUE,
                                             nlevels = 20)
          
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
          
          # Aqui sustituir dat por points en el plano
          if (qvars==3){Ternary::TernaryPoints(dat[indx], pch = 16)} # design points - counterwise=FALSE
          if (qvars==4){
            datt=dat[dat[latent_var]==latent_plane,]
            if (dim(datt)[1]!=0){Ternary::TernaryPoints(datt[indx], pch = 16)}
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
            azimut=self$options$thetaval
            latitude=self$options$phival
            private$.wirePlot3(varNames,responseName,data=dat,form=modelformula,theta=azimut,phi=latitude,steps=200,legRange=zRange,col=private$.colpalletes())
          }
          TRUE
        }) # Close - List
) # Close - R6::R6Class
