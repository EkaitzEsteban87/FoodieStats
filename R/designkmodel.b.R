designkmodelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "designkmodelClass",
    inherit = designkmodelBase,
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
            qvars=length(self$options$vars)+length(self$options$cats)
            if (is.null(self$options$vars) || is.null(self$options$yield) || qvars<2){return()}
            self$results$cols$setState(qvars)
            
            varNames <- self$options$vars
            catNames = self$options$cats
            responseName=self$options$yield

            catdata=self$data[catNames]
            vardata=self$data[varNames]
            responsedata=self$data[responseName]
            
            nda=length(vardata)
            vardata2= data.frame(matrix(nrow = dim(vardata)[1], ncol = dim(vardata)[2]))
            for (k in 1:nda){
              vardata2[k]=as.numeric(as.character(unlist(vardata[k])))
            }
            colnames(vardata2)=varNames
            
            doe=cbind(vardata2,catdata,responsedata)
            
            image2 <- self$results$interactionplot
            image2$setState(doe)

            ncat=length(unique(unlist(catdata)))
            
            if (ncat>0){
            numdata= data.frame(matrix(nrow = dim(catdata)[1], ncol = 1))
            foo= unlist(catdata)
            foo=levels(foo)
            
            if (ncat==2){
              firstcat=foo[1]
              numdata[catdata==firstcat]=-1
              secondcat=foo[2]
              numdata[catdata==secondcat]=1
            }
            
            colnames(numdata)=names(catdata)
            
            doersm=cbind(vardata2,numdata,responsedata)
            }else{
              doersm=doe
            }
            
            image1 <- self$results$mainplot
            image1$setState(doersm)
            
            doelm=doersm
            zk=dim(doelm)[2]-1
            
            for (k in 1:zk) {
              x0=median(doelm[,k])
              x1=unname(quantile(doelm[,k],0.15))
              x2=x0-x1
              uncode=(unlist(doelm[,k])-x0)/x2
              doelm[,k]=uncode
            }
                      
            n=dim(doelm)[1]-1
            modelo <- lm(as.formula(paste(responseName,"~.^",n,sep="")),data=doelm)
            self$results$modellm$setState(list(responseName,n,doelm))
            #self$results$modellm$setState(modelo)
            
            image3 <- self$results$paretoplot
            image3$setState(doe)

            image4 <- self$results$halfplot
            image4$setState(doe)
            
            if (is.null(self$options$choosecoded)){x="coded"}else{x=self$options$choosecoded}
            if (x=="coded"){
              modelo2 <- lm(as.formula(paste(responseName,"~.^",2)),data=doelm)
            }else{
              modelo2 <- lm(as.formula(paste(responseName,"~.^",2)),data=doersm)}
            
            alphaval <- (self$options$alphaval)/100
            backwardselim <- function(modelo2) {
              while (TRUE) {
                resumen <- summary(modelo2)
                pval <- resumen$coefficients[, "Pr(>|t|)"]
                highp <- names(pval)[which.max(pval)]
                
                if (max(pval) > alphaval) {
                  modelo2 <- update(modelo2, as.formula(paste(". ~ . -", highp)))
                } else {break}
              }
              return(modelo2)
            }
            
            if (is.null(self$options$mupdate)){modeldis="none"}else{modeldis=self$options$mupdate} # ensure default
            finalmodel=switch(modeldis,
              "none"=modelo2,
              "automatic"=backwardselim(modelo2),
              "manual"=modelo2)

            anova <- stats::anova(finalmodel)
            self$results$doeanova$setContent(anova)
            
            modelbase <- coef(finalmodel)[names(coef(finalmodel))]
            modelplus <- coef(finalmodel)[names(coef(finalmodel))]
            modelminus <- coef(finalmodel)[names(coef(finalmodel))]

            model_names <- names(modelplus)
            model_values <- unname(modelplus)
            
            cateffect <- model_names[model_names == catNames]
            
            if (length(cateffect)>0){
              indx1 <- grep(paste0(catNames, ":"), model_names)
              catinteract1 <- setNames(model_values[indx1], model_names[indx1])
              indx2 <- grep(paste0(":",catNames), model_names)
              catinteract2 <- setNames(model_values[indx2], model_names[indx2])
              
              foo1 = names(c(catinteract1,catinteract2))
              foo2 <- sub(paste0(":",catNames),"",foo1)
              catinteract <- sub(paste0(catNames,":"),"",foo2)
              
              if (length(cateffect)>0){
                modelplus["(Intercept)"] <- modelplus["(Intercept)"] + modelplus[catNames]
                modelminus["(Intercept)"] <- modelminus["(Intercept)"] - modelminus[catNames]
              }
              
              if (length(catinteract)>0){
                nint=length(catinteract)
                for (k in 1:nint){
                  rint=catinteract[k]
                  rrint=foo1[k]
                  if (!is.na(modelplus[rint])){
                    modelplus[rint] <- modelplus[rint] + modelplus[rrint]
                    modelminus[rint] <- modelminus[rint] - modelminus[rrint]
                  }
                }
              }
              modelplus <- modelplus[!grepl(catNames, names(modelplus))]
              modelminus <- modelminus[!grepl(catNames, names(modelminus))]
            }
            
            xtabla=summary(finalmodel) 
            R2=xtabla$r.squared 
            Predicted=finalmodel$fitted.values
            cod=doelm[1:(length(doelm)-1)]
            colnames(cod) <- abbreviate(colnames(cod))
            colnames(cod) <- paste0("Coded-", colnames(cod))
            doetable=cbind(vardata2,catdata,cod,responsedata,Predicted)
            
            self$results$doetable$setContent(doetable)
            
            formulatable <- function(modely,responsy) {
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
            }
            
            formulatex <-formulatable(modelbase,responseName)
            table <- self$results$maintable
            table$setRow(rowNo=1, values=list(
              cox1=formulatex,
              rsq1=R2))
            
            if (ncat>1){
              formulatex1 <-formulatable(modelminus,paste0(responseName,"(",firstcat,")",sep=""))
              formulatex2 <-formulatable(modelplus,paste0(responseName,"(",secondcat,")",sep=""))
              table <- self$results$maintable3k
              table$setRow(rowNo=1, values=list(cox3=formulatex1))
              table$setRow(rowNo=2, values=list(cox3=formulatex2))                
              }

            doecode=doersm
            n0=dim(doersm)[2]-1
            columns_to_code <- names(doecode)[1:n0]
            coding_formulas <- list()
            col2=c("xA","xB","xC","xD","xE","xF","xG","xH","xI","xJ","xK","xL","xM","xN","xO","xP","xQ","xR","xS","xT","xU","xW","xX","xY","xZ")
            k=0
            decode=data.frame()
            
            for (col in columns_to_code) {
              k=k+1
              x0=median(doecode[,col])
              x1=unname(quantile(doecode[,col],0.15))
              x2=x0-x1
              formula_str=paste(col2[k],"~(",col,"-",x0,")/",x2,sep="")
              coding_formulas[[k]] <- as.formula(formula_str)
              decode=rbind(decode,cbind(col,col2[k]))
            }
            Ns=length(coding_formulas)
            
            doecode <- switch(   
              Ns,   
              coded.data(doecode,coding_formulas[[1]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]],coding_formulas[[6]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]],coding_formulas[[6]],coding_formulas[[7]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]],coding_formulas[[6]],coding_formulas[[7]],coding_formulas[[8]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]],coding_formulas[[6]],coding_formulas[[7]],coding_formulas[[8]],coding_formulas[[9]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]],coding_formulas[[6]],coding_formulas[[7]],coding_formulas[[8]],coding_formulas[[9]],coding_formulas[[10]]),
              coded.data(doecode,coding_formulas[[1]],coding_formulas[[2]],coding_formulas[[3]],coding_formulas[[4]],coding_formulas[[5]],coding_formulas[[6]],coding_formulas[[7]],coding_formulas[[8]],coding_formulas[[9]],coding_formulas[[10]],coding_formulas[[11]])
            )
            
            combinaciones1 <- combn(decode$col, 2, simplify = TRUE)
            combinaciones2 <- combn(decode$V2, 2, simplify = TRUE)
            nuevas_comb1 <- apply(combinaciones1, 2, function(pair) paste(pair, collapse = ":"))
            nuevas_comb2 <- apply(combinaciones2, 2, function(pair) paste(pair, collapse = ":"))
            
            foo=cbind(nuevas_comb1,nuevas_comb2)
            colnames(foo)=c("col","V2")
            decodext <- rbind(decode,foo)
            nombres_mapeados <- setNames(decodext$col,decodext$V2)
            
            effects <- coef(finalmodel)[-pmatch("(Intercept)", names(coef(finalmodel)))]
            
            if (length(effects)==0){return()}
           
            nombres_actuales <- names(effects)
            nombres_finales <- sapply(nombres_actuales, function(nombre) {
              for (nombre_map in names(nombres_mapeados)) {
                nombre <- sub(nombres_mapeados[nombre_map], nombre_map, nombre)
              }
              return(nombre)
            })
            names(effects) <- nombres_finales
            effect_names <- names(effects)
            effect_values <- unname(effects)
            
            effect_names_only <- effect_names[!grepl(":", effect_names)]
            effects_only <- setNames(effect_values[!grepl(":", effect_names)], effect_names_only)
            firstorder=paste("FO(", paste(names(effects_only), collapse = ", "), ")", sep = "")
            
            interaction_only <- setNames(effect_values[grepl(":", effect_names)], effect_names[grepl(":", effect_names)])
            interorder <- paste(names(interaction_only), collapse = "+")
            
            baseformu=paste("~",firstorder,"+",interorder,sep="")
            rsmformu=paste(responseName,baseformu,sep="")
            
            modelo_rsm <- rsm(as.formula(rsmformu),data=doecode)
            
            self$results$rsmformula$setState(baseformu)
            
            npe=length(rsm::contour.lm(modelo_rsm,as.formula(baseformu),image=FALSE,plot.it=FALSE))
            self$results$colsrsm$setState(npe)
            
            # self$results$modelrsm$setState(modelo_rsm)
            self$results$modelrsm$setState(list(doecode,rsmformu))
            image5 <- self$results$rsmplot
            image5$setState(doersm)

#            object1=self$results$modellm$state
#            object2=self$results$modelrsm$state
#            xh1=length(serialize(object1,connection=NULL))
#            xh2=length(serialize(object2,connection=NULL))
#            self$results$debugger$setContent(cbind(xh1,xh2))
            
#            object1=image1$state
#            xh1=length(serialize(object1,connection=NULL))
#            object2=image2$state
#            xh2=length(serialize(object2,connection=NULL))
#            object3=image3$state
#            xh3=length(serialize(object3,connection=NULL))
#            object4=image4$state
#            xh4=length(serialize(object4,connection=NULL))
#            object5=image5$state
#            xh5=length(serialize(object5,connection=NULL))
#            self$results$debugger$setContent(cbind(xh1,xh2,xh3,xh4,xh5))
            
        },
        .mainplot=function(image1,...) {
          if (is.null(image1$state) || is.null(self$options$yield)){return(FALSE)}
          plotdata=image1$state
          tvars <- self$results$cols$state
          ncolumns=switch(tvars,1,2,3,2,3,3,4,4,5,5)
          if (is.null(ncolumns)){ncolumns=5}
          ap=ggDoE::main_effects(plotdata,response=self$options$yield,n_columns=ncolumns,color_palette ='turbo')
          self$results$showplot$setContent(ap)
          TRUE
        },
        .interactionplot=function(image2,...) {
          if (is.null(image2$state) || is.null(self$options$yield) || dim(image2$state)[2]<3){return(FALSE)}
          plotdata=image2$state
          tvars <- (self$results$cols$state*(self$results$cols$state-1))/2
          ncolumns=switch(tvars,1,2,3,2,3,3,4,4,5,5)
          if (is.null(ncolumns)){ncolumns=5}
          ggDoE::interaction_effects(plotdata,response=self$options$yield, colors = c("#6B9DE8", "#E6AC40"),linetypes = c("solid", "dashed"),n_columns=ncolumns)
          TRUE
        },
        .paretoplot=function(image3,...) {
          if (is.null(image3$state) || is.null(self$options$yield)){return(FALSE)}
          
          plotlm=self$results$modellm$state
          modelo <- lm(as.formula(paste(plotlm[[1]],"~.^",plotlm[[2]],sep="")),data=plotlm[[3]])
          
          #modelo=self$results$modellm$state
          alphaval=(self$options$alphaval)/100
          ap=ggDoE::pareto_plot(modelo,method =self$options$criteria,alpha=alphaval)
          self$results$showplot$setContent(ap)
          TRUE
        },
        .halfplot=function(image4,...){
          if (is.null(image4$state) || is.null(self$options$yield)){return(FALSE)}
          
          plotlm=self$results$modellm$state
          modelo <- lm(as.formula(paste(plotlm[[1]],"~.^",plotlm[[2]],sep="")),data=plotlm[[3]])
          #modelo=self$results$modellm$state
          alphaval=(self$options$alphaval)/100
          ap=ggDoE::half_normal(modelo,method=self$options$criteria,alpha=alphaval,ref_line=TRUE,label_active=TRUE,margin_errors=TRUE)
          self$results$showplot$setContent(ap)
          TRUE
        },
        .rsmplot=function(image5,...) {
          if (is.null(image5$state) || is.null(self$options$yield) || (self$results$colsrsm$state<1))
            return(FALSE)
          npe=self$results$colsrsm$state
          
          plotrsm=self$results$modelrsm$state
          modeloR <- rsm(as.formula(plotrsm[[2]]),data=plotrsm[[1]])

          Jamov=colorRampPalette(c("#6B9DE8", "#E6AC40"))(50)
          baseformu <- self$results$rsmformula$state
          
          azimut=self$options$thetaval
          latitude=self$options$phival
          par(mfcol = c(npe,2))
          rsm::contour.lm(modeloR,as.formula(baseformu),image=TRUE,img.col = Jamov,decode = TRUE,main=self$options$yield)
          rsm::persp.lm(modeloR,as.formula(baseformu),contours="colors",col=Jamov,theta=azimut,phi=latitude,decode = TRUE,zlab=self$options$yield, font.lab=2,col.lab=33)
          TRUE
        }) # Close - List
) # Close - R6::R6Class
