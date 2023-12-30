designrsmmodelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "designrsmmodelClass",
    inherit = designrsmmodelBase,
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
            if (is.null(self$options$vars) || is.null(self$options$yield) || qvars<2){return()}
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
            
            if (is.null(self$options$rsdesign)){design0="bbd"}else{design0=self$options$rsdesign} # ensure default
            
            for (k in 1:zk){
              x0=median(doelm[,k])
              if (design0=="cci"){x1=min(doelm[,k])}else{x1=unname(quantile(doelm[,k],0.15))}
              x2=x0-x1
              uncode=(unlist(doelm[,k])-x0)/x2
              doelm[,k]=uncode
            }
            
            image1 <- self$results$mainplot
            image1$setState(doelm)
            
            if (design0=="bbd"){
              image2 <- self$results$interactionplot
              image2$setState(doelm)
            }
            
            modelformula=private$.getformula(responseName,varNames) # private$.
            modelo1 <- lm(as.formula(modelformula),data=doelm)
            foo=summary(modelo1)
            coefs=foo$coefficients[,1]
            tval=foo$coefficients[,3]
            gdl=modelo1$df.residual
            alpha1 <- (self$options$alphaval)/100
            t_limit=qt(alpha1/2,gdl,lower.tail=FALSE) # mtab

            self$results$paretodata$setState(list(tval,t_limit))
            
            image3 <- self$results$paretoplot
            image3$setState(doelm)
            
            terms1=private$.refinemodel(modelo1,responseName,doelm,alphaval=alpha1)
            
            baseformu=jmvcore::composeFormula(terms1)
            baseformu <- gsub("`", "", baseformu) # clean
            modelformula=jmvcore::composeFormula(responseName,terms1)
            modelformula <- gsub("`", "", modelformula) # clean
            
            modeloR <- lm(as.formula(modelformula),data=doersm)
            modeloL <- lm(as.formula(modelformula),data=doelm)
            
            Predicted=modeloL$fitted.values
            Residu=modeloL$residuals
            cod=doelm[1:(length(doelm)-1)]
            doetable=cbind(vardata2,cod,responsedata,Predicted,Residu)
            
            #-----------------------
            # Create table by code
            #-----------------------
            
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
            
            modelbaseL <- coef(modeloL)[names(coef(modeloL))]
            formulatexL=private$.formulatable(modelbaseL,responseName)

            modelbaseR <- coef(modeloR)[names(coef(modeloR))]
            formulatexR=private$.formulatable(modelbaseR,responseName)

            xtabla=summary(modeloL) 
            R2=xtabla$r.squared 
            adjR2=xtabla$adj.r.squared
            
            table <- self$results$maintable
            table$setRow(rowNo=1, values=list(equ1=formulatexL,Rsq1=R2,Ad1=adjR2))
            
            table2 <- self$results$maintable2
            table2$setRow(rowNo=1, values=list(equ2=formulatexR,Rsq2=R2,Ad2=adjR2))
            
            anova <- stats::anova(modeloL)
            self$results$doeanova$setContent(anova)

            checkvars=length(all.vars(as.formula(baseformu)))
            
            if (checkvars<2){return()}
            
            npe=length(rsm::contour.lm(modeloR,as.formula(baseformu),image=FALSE,plot.it=FALSE))
            self$results$colsrsm$setState(npe)
            self$results$modelrsm$setState(list(terms1,responseName))
            image4 <- self$results$rsmplot
            image4$setSize(960,480*npe)
            image4$setState(doersm)
            
            #object1=image1$state
            #object2=self$results$paretodata$state
            #xh1=length(serialize(object1,connection=NULL))
            #xh2=length(serialize(object2,connection=NULL))
            #self$results$debugger$setContent(cbind(xh1,xh2))
            
        },
        .mainplot=function(image1,...){
          if (is.null(image1$state) || is.null(self$options$yield)){return(FALSE)}
          plotdata=image1$state
          
          y00=dim(plotdata)[2]
          xx <- c(-1,0,1)
          xx1 <- seq(from=-1,to=1,by=0.02)
          xy=data.frame(xx1)
          colnames(xy)="xx"
          par(mfrow=c(1,(y00-1)))
          
          for (k in 1:(y00-1)){
            x0=plotdata[,k]==-1
            a1=mean(plotdata[x0,y00])
            x0=plotdata[,k]==0
            a2=mean(plotdata[x0,y00])
            x0=plotdata[,k]==1
            a3=mean(plotdata[x0,y00])
            yy=c(a1,a2,a3)
            
            df <- data.frame(xx,yy)
            main_model <- lm(yy ~ poly(xx,2),data=df)
            b=predict(main_model,newdata=xy, type="response")
            
            ylimits=c(max(b),min(b))
            plot(df$xx,predict(main_model),xlab=colnames(plotdata[k]),ylab=paste0("Mean of ",colnames(plotdata[y00])),col="#E6AC40",ylim=ylimits,cex = 3,pch = 16)
            grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
            lines(xx1,b,col="#6B9DE8",lwd=2,ylim=ylimits)
          }
          TRUE
        },
        .interactionplot=function(image2,...){
          if (is.null(image2$state) || is.null(self$options$yield) || dim(image2$state)[2]<3){return(FALSE)}
          
          plotdata=image2$state
          y00=dim(plotdata)[2]

          panel.intera = function(x,y){
            par(new = TRUE)
            interaction.plot(x.factor = x, # x-axis variable
                             trace.factor = y, # variable for lines
                             response = plotdata[,y00], # y-axis variable
                             fun = mean, # metric to plot
                             col = c("#6B9DE8","#9b9b9b","#E6AC40"),
                             lty = 1, #line type
                             lwd = 2, #line width
                             legend = FALSE) 
          }
          
          pairs(plotdata[1:(y00-1)],
                upper.panel = panel.intera,
                lower.panel = panel.intera,
                main = "Interaction Effect Matrix",
                oma=c(3,3,3,15))
          
          par(xpd=TRUE)
          
          for (k in 1:(y00-1)){
            yleg=(1-(k-1)*(1/(y00-1)))*0.95
            legend(0.9, yleg, as.vector(c(-1,0,1)),title=colnames(plotdata)[k],fill=c("#6B9DE8","#9b9b9b","#E6AC40"))
          }
          TRUE
        },
        .paretoplot=function(image3,...){
          if (is.null(image3$state) || is.null(self$options$yield)){return(FALSE)}
          
          paretodata=self$results$paretodata$state
          tval=paretodata[[1]]
          t_limit=paretodata[[2]]
          
          pareto=abs(tval[-1])
          pareto2=data.frame(pareto)
          paretoylabel=rownames(pareto2)[order(pareto)]
          colores=rep("#E6AC40",length(pareto))
          cy=sign(tval[-1])
          colores[cy>0]="#6B9DE8"
          
          par(mar = c(5, 14, 4, 2) + 0.1) # c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1
          barplot(sort(pareto2[,1]),col=colores,horiz=TRUE,las=1,names=paretoylabel,xlab="Absolute effects",main="Pareto Chart of the Standardized Effects")
          legend("bottomright",legend=c("negative", "positive"),col=c("#E6AC40","#6B9DE8"), pch=15, cex=1,pt.cex=2,bty="n",horiz=TRUE)
          abline(v=t_limit,col="red", lwd = 2, lty = 2)
          TRUE
        },
        .rsmplot=function(image4,...){
          if (is.null(image4$state) || is.null(self$options$yield) || (self$results$colsrsm$state<1))
            return(FALSE)
          
          npe=self$results$colsrsm$state
          plotrsm=self$results$modelrsm$state

          doersm=image4$state
          terms1=plotrsm[[1]]
          responseName=plotrsm[[2]]

          baseformu=jmvcore::composeFormula(terms1)
          baseformu <- gsub("`", "", baseformu) # clean
          modelformula=jmvcore::composeFormula(responseName,terms1)
          modelformula <- gsub("`", "", modelformula) # clean
          modeloR <- lm(as.formula(modelformula),data=doersm)
          
          Jamov=colorRampPalette(c("#6B9DE8", "#E6AC40"))(50)
          
          azimut=self$options$thetaval
          latitude=self$options$phival
          
          par(mfcol = c(npe,2))
          
          rsm::contour.lm(modeloR,as.formula(baseformu),image=TRUE,img.col = Jamov,decode = TRUE,main=self$options$yield)
          rsm::persp.lm(modeloR,as.formula(baseformu),contours="colors",col=Jamov,theta=azimut,phi=latitude,decode = TRUE,zlab=self$options$yield, font.lab=2,col.lab=33)
          TRUE
        },
        .getformula=function(responseName,varNames){
          combi<- combn(varNames, 2, simplify = TRUE)
          interactions <- apply(combi,2,function(x) as.list(x))
          first <- as.list(varNames)
          second = apply(rbind(varNames,varNames),2,function(x) as.list(x))
          terms0 = c(first,second,interactions)
          a=jmvcore::composeFormula(responseName,terms0) 
          return(a)
        },
        .refinemodel=function(modely,responseName,doelm,alphaval=0.05){
          if (is.null(self$options$mupdate)){refine="default"}else{refine=self$options$mupdate} # ensure default
          
          if (refine=="default"){dd=summary(modely)}
          if (refine=="forward"){
            regvacia<-lm(as.formula(jmvcore::composeFormula(responseName,1)),data=doelm)
            regforw<-step(regvacia, scope = list(lower=regvacia, upper=modely),direction = "forward",trace = FALSE)
            dd=summary(regforw)
          }
          if (refine=="both"){
            regvacia<-lm(as.formula(jmvcore::composeFormula(responseName,1)),data=doelm)
            regstep<-step(regvacia, scope = list(lower=regvacia, upper=modely),direction = "both",trace = FALSE)
            dd=summary(regstep)
          }
          if (refine=="backward"){
            regback <- step(modely, direction = "backward", trace = FALSE)
            dd=summary(regback)
          }
          if (refine=="backwardalpha"){dd=private$.backwardselim(modely,doelm,alphaval)}
          if (refine=="allin"){
            newterms0=private$.all_in(modely,alphaval)
            newformu=jmvcore::composeFormula(responseName,newterms0)
            finalmodel <- lm(as.formula(newformu),data=doelm)
            dd=summary(finalmodel)
          }
          
          terms1=rownames(dd$coefficients)
          if ("(Intercept)" %in% terms1) {terms1 <- terms1[-which(terms1 == "(Intercept)"), drop = FALSE]}
          return(terms1)
        },
        .backwardselim=function(modely,doelm,alphaval=0.05){
          while (TRUE) {
            resumen <- summary(modely)
            pval <- resumen$coefficients[, "Pr(>|t|)"]
            highp <- names(pval)[which.max(pval)]
            if (max(pval) > alphaval) {
              modely <- update(modely, as.formula(paste(". ~ . -", highp)))
            } else {break}
          }
          dd=summary(modely)
          return(dd)
        },
        .all_in=function(modely,alphaval=0.05){
          summodel=summary(modely)
          coe=(summodel$coefficients[-1,4]<alphaval)
          coenames <- names(which(coe))
          newsecond=coenames[grep("I\\([^)]+\\^2\\)", coenames)]
          newinteractions <- grep(":", coenames, value = TRUE)
          newfirst <- setdiff(coenames, c(newsecond,newinteractions))
          newsecond <- gsub("`", "", newsecond) # clean
          newinteractions<- gsub("`", "", newinteractions) # clean
          newfirst <- gsub("`", "", newfirst) # clean
  
          if (length(newsecond)>0){
            foo <- sub("I\\(([^)]+)\\^2\\)", "\\1", newsecond)
            newsecond2 = apply(rbind(foo,foo),2,function(x) as.list(x))
          }else{newsecond2=as.list(newsecond)}
  
          if (length(newinteractions)>0){
            ele <- unlist(strsplit(newinteractions,":"))
            ele1 <- ele[c(TRUE, FALSE)]
            ele2 <- ele[c(FALSE, TRUE)]
            ele3=rbind(ele1,ele2)
            rownames(ele3) <- NULL
            newinteractions2 <- apply(ele3,2,function(x) as.list(x))
          }else{newinteractions2=as.list(newinteractions)}
  
          newterms0 = c(newfirst,newsecond2,newinteractions2)
          return(newterms0)
        },
        .formulatable=function(modely,responsy){
          modeltex=modely
          namestex <- names(modeltex)
          namestex <- gsub("Intercept","", namestex)
          namestex <- gsub(":", "*", namestex)
          names(modeltex) <- namestex
          sign1 <- sign(modeltex)
          sign1tex <- ifelse(sign1 == 1, "+", "-")
          modeltexcad <- as.character(round(modeltex,6))
          pointss <- sapply(strsplit(modeltexcad, "\\."), function(x) ifelse(length(x) > 1, nchar(x[2]), 0))
          spf=paste0("%.",max(pointss),"f",sep="")
          modeltextt <- sprintf(spf, abs(modeltex))
          modeltextt[1] <- sprintf(spf,modeltex[1])
          formula0 <- paste0(responsy,"=",modeltextt[1],paste0(sign1tex[-1],modeltextt[-1],"*",names(modeltex)[-1], collapse = ""))
          return(formula0)
        }) # Close - List
) # Close - R6::R6Class
