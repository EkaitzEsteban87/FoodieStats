createdesignClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "createdesignClass",
    inherit = createdesignBase,
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
        .run = function(){
          # Default settings
          if (is.null(self$options$expdesign)){design0="2k"}else{design0=self$options$expdesign} 
          if (is.null(self$options$nvars)){qvars=3}else{qvars=as.numeric(self$options$nvars)} 
          if (is.null(self$options$nlevels)){niveles=3}else{niveles=as.numeric(self$options$nlevels)} 
          if (is.null(self$options$cubecp)){centerC=3}else{centerC=as.numeric(self$options$cubecp)} 
          if (is.null(self$options$starcp)){centerS=1}else{centerS=as.numeric(self$options$starcp)} 
          if (is.null(self$options$resolut)){resol=5}else{resol=as.numeric(self$options$resolut)} 
          if (is.null(self$options$inscribed)){inscr=FALSE}else{inscr=self$options$inscribed} 
          if (is.null(self$options$staralpha)){alpha0="rotatable"}else{alpha0=self$options$staralpha} 
          
          navar=paste0("x",1:qvars)
          formulas=private$.formulaeList(n=qvars)
          
          # 2^(k-p) fractional design
          if (design0=="ff2k"){
            foo=FrF2::FrF2(nruns=NULL,nfactors = qvars, resolution = resol, randomize = FALSE,replications=1,factor.names =navar)
            fooo=as.numeric(as.character(unlist(foo)))
            design <- matrix(fooo, nrow = dim(foo)[1], byrow = FALSE)}
          
          # Full 2^k design
          if (design0=="2k"){design=AlgDesign::gen.factorial(levels=2,nVars=qvars,center=TRUE,varNames = navar)}
          
          # Full 3^k design
          if (design0=="3k"){design=AlgDesign::gen.factorial(levels=3,nVars=qvars,center=TRUE,varNames = navar)}          

          # Full n^k design
          #if (design0=="nk"){design=AlgDesign::gen.factorial(levels=niveles,nVars=qvars,center=TRUE,varNames = navar)} 
          
          # Box-behnken design
          if (design0=="bbd"){
            behnken=rsm::bbd(k=qvars,n0 = centerC,randomize = FALSE,coding=formulas)
            numcol=ncol(behnken)
            design=behnken[,(numcol-qvars+1):numcol]
          }
          
          # Central Composite design
          if (design0=="ccd"){
            composite=rsm::ccd(basis=qvars,randomize = FALSE,alpha=alpha0,n0 = c(centerC,centerS),inscribed=inscr,oneblock=TRUE,coding=formulas)
            numcol=ncol(composite)
            design=composite[,(numcol-qvars+1):numcol]
          }
          
          # Simplex Centroid Mixture design
          if (design0=="scd"){design=mixexp::SCD(qvars)}
          
          # Simplex Lattice Mixture design
          if (design0=="sld"){design=AlgDesign::gen.mixture(niveles,navar)}          
          
          # Simplex Lattice + process Mixture design (Work in progress)
          
          #self$results$debugger$setContent(design)
          
          #-----------------------
          # Mixture design Table - Create table by code
          #-----------------------       
          # Fill columns
          table0 <- self$results$designtable
          table0$addColumn(name="nrun", title="No. run", type='number')
          for (i in 1:qvars){table0$addColumn(name=paste0("v",i), title=paste0("x",i), type='number')}
          # Fill Rows
          num_rows <- nrow(design)
          col_names=c("nrun",paste0("v",1:qvars))
          
          for (i in 1:num_rows){
            row_values <- format(c(i,design[i,]),digits=3)
            names(row_values) <- col_names
            row=as.list(row_values)
            table0$addRow(rowKey=i,row)
          }
          
          designname=private$.designnames()
          lev1=paste0(sort(unique(unlist(format(design,digits=3)))),collapse =", ")
          if (design0=="ff2k"){lev1="-1, 1"}
          chain <- paste0(designname," - Total No. of runs: ",num_rows, "\n"," - Coded levels: ",lev1)
          table0$setNote('Note',chain)
          #-----------------------
          # end table
          #-----------------------
          
          # Add more features
          image1 <- self$results$designplot
          image1$setState(design)
          private$.saveOutputs(design) # SpreadSheet

        },
        .designnames=function(...){
          design0=self$options$expdesign
          designname <- switch(design0,
                               "2k"="2k Full Factorial",
                               "3k"="3k Full Factorial",
                               "ff2k"="2k Fractional Factorial",
                               "bbd"="Box-Behnken",
                               "ccd"="Central Composite",
                               "scd"="Simplex Central Mixture",
                               "sld"="Simplex Lattice Mixture"
                               )
          return(designname)
        },
        .formulaeList=function(n){
          Lformulas <- list()
          for (i in 1:n) {
            nvariable <- paste0("x",i)
            formula1 <- as.formula(paste0(nvariable,"~",paste0("X",i)))
            Lformulas[[nvariable]]=formula1
          }
          return(Lformulas)
        },
        .designplot=function(image1,...){ # TODO - more plot options - Ternary
          a0=0
          design=image1$state
          design0=self$options$expdesign
          qvars=as.numeric(self$options$nvars)
          if (design0=="scd"){a0=1}
          if (design0=="sld"){a0=1}
          if (is.null(image1$state) || a0==0 || qvars!=3){return(FALSE)}
          
          mixexp::DesignPoints(design)
          TRUE
        },
        .saveOutputs = function(design,...) {
          if (self$options$writeexcel && self$results$writeexcel$isNotFilled()) {
            n=as.numeric(self$options$nvars)
            keys <- 1:n
            titles <- paste0("x",keys)
            descriptions <- paste0("Design matrix model ",self$options$expdesign," - Coded variable x",keys)
            measureTypes <- rep("continuous",n)
            
            self$results$writeexcel$set(keys=keys,titles=titles,descriptions=descriptions,measureTypes=measureTypes)
            self$results$writeexcel$setRowNums(rownames(as.data.frame(design)))
            
            for (i in 1:n) {self$results$writeexcel$setValues(values=design[,i],index=i)}
          }
        }) # Close - List
) # Close - R6::R6Class
