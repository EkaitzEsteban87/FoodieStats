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
            
            scaling <- cbind(matrix(1, nrow = zk, ncol = 1),matrix(0, nrow = zk, ncol = 1))
            
            for (k in 1:zk){
              x0=median(doelm[,k])
              if (design0=="cci"){x1=min(doelm[,k])}else{x1=unname(quantile(doelm[,k],0.15))}
              x2=x0-x1
              codif=(unlist(doelm[,k])-x0)/x2
              doelm[,k]=codif # coded
              scaling[k,1]=x2
              scaling[k,2]=x0
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
            
            modeloR <- lm(as.formula(modelformula),data=doersm) # original vars
            modeloL <- lm(as.formula(modelformula),data=doelm) # coded vars
            
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
            
            private$.scalingtable(varNames,scaling)
            
            
            xtabla=summary(modeloL) 
            R2=xtabla$r.squared 
            adjR2=xtabla$adj.r.squared
            #-----------------------
            # ANOVA Table - Create table by code
            #-----------------------                
            anova <- stats::anova(modeloL)
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
            tableANO <- self$results$anovatable
            
            tableANO$addColumn(name='nam2', title="Source",type='text')
            tableANO$addColumn(name='doe2', title="Dof",type='number')
            tableANO$addColumn(name='ssq2', title="Sum of Squares",type='number')
            tableANO$addColumn(name='msq2', title="Mean Square",type='number')
            tableANO$addColumn(name='fva2', title="F-value",type='number')
            tableANO$addColumn(name='pva2', title="Pr(>F)",type='number',format='zto,pvalue')
            
            # Fill Rows
            row1=list()
            row1$nam2 <- "Overall Model"
            row1$doe2 <- ovdf
            row1$ssq2 <- ovsq
            row1$msq2 <- ovmq
            row1$fva2 <- Fval
            row1$pva2 <- ovpv
            row1=as.list(row1)
            tableANO$addRow(rowKey=1,row1)
            
            row2=list()
            row2$nam2 <- "Residual"
            row2$doe2 <- redf
            row2$ssq2 <- resq
            row2$msq2 <- MSE
            row2=as.list(row2)
            tableANO$addRow(rowKey=2,row2)   
            
            row3=list()
            row3$nam2 <- "Total"
            row3$doe2 <- todf
            row3$ssq2 <- tosq
            row3=as.list(row3)
            tableANO$addRow(rowKey=3,row3)  
            
            cadena <- paste0("Multiple R-Squared: ", format(R2, digits = 3), "\n","Adjusted R-Squared: ", format(adjR2, digits = 3), "\n","Mean Square Error: ", format(MSE, digits = 3))
            tableANO$setNote('Note',cadena)
            #-----------------------
            # end table
            #-----------------------          
            
            #-----------------------
            # OR Estimation Table - Create table by code
            #-----------------------                
            modeloR <- lm(as.formula(modelformula),data=doersm) # original vars
            modelbaseR <- coef(modeloR)[names(coef(modeloR))]
            formulatexR=private$.formulatable(modelbaseR,responseName)
            est=summary(modeloR)
            estidata=est$coefficients

            # Fill columns
            tableOR <- self$results$estitableOR
            
            tableOR$addColumn(name='nam1', title="Model Terms",type='text')
            tableOR$addColumn(name='coe1', title=colnames(estidata)[1],type='number')
            tableOR$addColumn(name='sse1', title=colnames(estidata)[2],type='number')
            tableOR$addColumn(name='stu1', title=colnames(estidata)[3],type='number')
            tableOR$addColumn(name='pva1', title=colnames(estidata)[4],type='number',format='zto,pvalue')
            
            # Fill Rows
            rowo=list()
            num_rows <- nrow(estidata)
            for (i in 1:num_rows){
              rowo$nam1 <- rownames(estidata)[i]
              rowo$coe1 <- estidata[i,1]
              rowo$sse1 <- estidata[i,2]
              rowo$stu1 <- estidata[i,3]
              rowo$pva1 <- estidata[i,4]
              
              rowo=as.list(rowo)
              tableOR$addRow(rowKey=i,rowo)
            }

            # with Simple table 2
            table2 <- self$results$maintable2
            table2$setRow(rowNo=1, values=list(equ2=formulatexR,Rsq2=R2,Ad2=adjR2,Mse2=MSE))
            

            #-----------------------
            # Coded Estimation Table - Create table by code
            #-----------------------                
            colnames(doelm)[1:qvars]=paste0("x",1:qvars) # es un poco makarra
            modelformulaX=private$.getformula(responseName,paste0("x",1:qvars))
            modeloX <- lm(as.formula(modelformulaX),data=doelm) # coded vars
            terms2=private$.refinemodel(modeloX,responseName,doelm,alphaval=alpha1)
            modelformulaX=jmvcore::composeFormula(responseName,terms2)
            modelformulaX <- gsub("`", "", modelformulaX) # clean
            modeloX <- lm(as.formula(modelformulaX),data=doelm) # coded vars
            modelbaseL <- coef(modeloX)[names(coef(modeloX))]
            formulatexL=private$.formulatable(modelbaseL,responseName)
            est=summary(modeloX)
            estidata=est$coefficients
            
            # Fill columns
            tableX <- self$results$estitableX
            
            tableX$addColumn(name='nam1', title="Model Terms",type='text')
            tableX$addColumn(name='coe1', title=colnames(estidata)[1],type='number')
            tableX$addColumn(name='sse1', title=colnames(estidata)[2],type='number')
            tableX$addColumn(name='stu1', title=colnames(estidata)[3],type='number')
            tableX$addColumn(name='pva1', title=colnames(estidata)[4],type='number',format='zto,pvalue')
            
            # Fill Rows
            rowo=list()
            num_rows <- nrow(estidata)
            for (i in 1:num_rows){
              rowo$nam1 <- rownames(estidata)[i]
              rowo$coe1 <- estidata[i,1]
              rowo$sse1 <- estidata[i,2]
              rowo$stu1 <- estidata[i,3]
              rowo$pva1 <- estidata[i,4]
              
              rowo=as.list(rowo)
              tableX$addRow(rowKey=i,rowo)
            }
            
            # with Simple table 1
            table <- self$results$maintable
            table$setRow(rowNo=1, values=list(equ1=formulatexL,Rsq1=R2,Ad1=adjR2,Mse1=MSE))
            
            checkvars=length(all.vars(as.formula(baseformu)))
            if (checkvars<2){return()}
            
            npe=length(rsm::contour.lm(modeloR,as.formula(baseformu),image=FALSE,plot.it=FALSE))
            self$results$colsrsm$setState(npe)
            self$results$modelrsm$setState(list(terms1,responseName))
            image4 <- self$results$rsmplot
            image4$setSize(960,480*npe)
            image4$setState(doersm)
            
            # mesh-grid optimization method #
            
            xnames=all.vars(modeloL$terms)[-1]
            xnames_ordenado <- xnames[match(varNames, xnames, nomatch = 0)]
            xnames <- xnames_ordenado[xnames_ordenado != ""]
            
            ovars=length(xnames)
            levl=switch(as.character(ovars),"3"=101,"4"=31,"5"=21,"6"=11,"7"=8,5)
            x=AlgDesign::gen.factorial(levels=levl,nVars=ovars,center=TRUE)
            xx=x/max(x)
            colnames(xx)=xnames # add this
            
            resultado=stats::predict(modeloL,xx)
            maxid=which.max(resultado)
            minid=which.min(resultado)
            x00=xx[maxid,]
            x11=xx[minid,]
            resmax=c(x00,resultado[maxid])
            resmin=c(x11,resultado[minid])
            
            # gradient-based optimization method #
            
            fr <- function(x0,modeloL,maxi,xnames) {
              df <- as.data.frame(t(x0))
              names(df)=xnames
              res=stats::predict(modeloL,df)
              if (maxi){res=-res} # max y min
              return(res)
            }
            
            resmax1=stats::optim(par=x00,fn=fr,gr=NULL,method="L-BFGS-B",lower=-1,upper=1,hessian=FALSE,modeloL,maxi=TRUE,xnames)
            resmax1$value=-resmax1$value
            resmin1=stats::optim(par=x11,fn=fr,gr=NULL,method="L-BFGS-B",lower=-1,upper=1,hessian=FALSE,modeloL,maxi=FALSE,xnames)
          
            vars_eliminadas <- setdiff(varNames,xnames)
            indices_eliminados <- match(vars_eliminadas,varNames)
            indx_res <- setdiff(seq_along(varNames), indices_eliminados)
            
            tablemax <- self$results$optimizator1
            tablemin <- self$results$optimizator2
            
            private$.maxmintable(xnames,responseName,resmax1$par,resmax1$value,scaling,indx_res,tablemax)
            private$.maxmintable(xnames,responseName,resmin1$par,resmin1$value,scaling,indx_res,tablemin)
            
            #object1=tablemax$state
            #object2=self$results$paretodata$state
            #xh1=length(serialize(object1,connection=NULL))
            #xh2=length(serialize(object2,connection=NULL))
            #self$results$debugger$setContent(xh1)
            
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
          if (is.null(image4$state) || is.null(self$options$yield) || (self$results$colsrsm$state<1)){return(FALSE)}
          
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
          
          Jamov=private$.colpalletes()(500)
          
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
          a <- gsub("`","",a) # clean
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
          namestex <- gsub(":", "\u00b7", namestex)
          names(modeltex) <- namestex
          sign1 <- sign(modeltex)
          sign1tex <- ifelse(sign1 == 1, "+", "-")
          modeltexcad <- as.character(round(modeltex,4))
          pointss <- sapply(strsplit(modeltexcad, "\\."), function(x) ifelse(length(x) > 1, nchar(x[2]), 0))
          spf=paste0("%.",max(pointss),"f",sep="")
          modeltextt <- sprintf(spf, abs(modeltex))
          modeltextt[1] <- sprintf(spf,modeltex[1])
          formula0 <- paste0(responsy,"=",modeltextt[1],paste0(sign1tex[-1],modeltextt[-1],"\u00b7",names(modeltex)[-1], collapse = ""))
          return(formula0)
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
        .maxmintable=function(varNames,responseName,optpoint,optoutputs,scalingraw,indx,table1){
          qvars=length(varNames)
          scaling=scalingraw[indx,]

          # Fill columns Table 1
          for (i in 1:qvars){table1$addColumn(name=paste0("v",i), title=varNames[i], type='number')}
          for (i in 1:qvars){table1$addColumn(name=paste0("cod",i), title=paste0("x",indx[i]), type='number')}
          table1$addColumn(name=paste0("yie",1),title=paste0(responseName[1]),type='number')
          
          # Fill Rows Table 1
          row_values=c(scaling[,1]*optpoint+scaling[,2],optpoint,optoutputs)
          col_names <- c(paste0("v",1:qvars),paste0("cod",1:qvars),paste0("yie",1))
          names(row_values) <- col_names
          row=as.list(row_values)
          table1$addRow(rowKey=1,row)
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
        }) # Close - List
) # Close - R6::R6Class
