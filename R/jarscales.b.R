
# This file is a generated template, your changes will not be overwritten
# setwd("/home/ekaitz/Desktop/FoodieStats")
# jmvtools::install()
# contact@jamovi.org

JARScalesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "JARScalesClass",
    inherit = JARScalesBase,
    private = list(
        .run = function() {
            require(jmvcore)

            # self$results$text$setContent((atr[,1]>jjar))

            # Get data from UI pane
            atr_names=self$options$attr
            atr=self$data[atr_names]
            N=length(atr)

            b=self$options$lik # liking
            like=self$data[b] # liking

            # Scale of the attributes (3 and 5 provided)
            sc=self$options$attrscale
            jjar= switch(sc,"Three"= 2,"Five"= 3,"Seven"= 4,"Nine"= 5)

            # zero buffer
            names_low=data.frame(rep(NA,N))
            names_high=data.frame(rep(NA,N))
            n_low=data.frame(rep(NA,N))
            n_high=data.frame(rep(NA,N))
            n_jar=data.frame(rep(NA,N))
            f_low=data.frame(rep(NA,N))
            f_high=data.frame(rep(NA,N))
            f_jar=data.frame(rep(NA,N))

            if (N){ # Consumer Reseach
                for (k in 1:N){
                    atr[,k]=as.numeric(as.character(atr[,k]))
                    n_high[k,1]=mean((atr[,k]>jjar))*100
                    n_low[k,1]=mean((atr[,k]<jjar))*100
                    n_jar[k,1]=mean((atr[,k]==jjar))*100

                    f_high[k,1]=length(which((atr[,k]>jjar)))
                    f_low[k,1]=length(which((atr[,k]<jjar)))
                    f_jar[k,1]=length(which((atr[,k]==jjar)))

                    names_low[k,1]=paste(atr_names[k],"low",sep="_")
                    names_high[k,1]=paste(atr_names[k],"high",sep="_")
                }
            }

            # zero buffer
            mdroph=data.frame(rep(NA,N))
            mdropl=data.frame(rep(NA,N))
            mlow=data.frame(rep(NA,N))
            mjar=data.frame(rep(NA,N))
            mhigh=data.frame(rep(NA,N))
            jarpenalty=data.frame(rep(NA,N))
            tr1=data.frame(rep(NA,N))
            tr2=data.frame(rep(NA,N))
            tr3=data.frame(rep(NA,N))
            tr4=data.frame(rep(NA,N))

            # Mean drop - liking analysis
            if (length(like) & N){
                confid=(self$options$attrhocalpha)/100
                for (k in 1:N){
                    jar_raw=like[(atr[,k]==jjar),1]
                    high_raw=like[(atr[,k]>jjar),1]
                    low_raw=like[(atr[,k]<jjar),1]
                    nonjar_raw=like[(atr[,k]!=jjar),1]

                    if (self$options$attrhoc) {
                        if (length(jar_raw)>0 & length(nonjar_raw)>0){
                        tr=t.test(jar_raw,nonjar_raw,alternative="two.sided",mu=0,paired=FALSE,var.equal=TRUE,conf.level=confid)
                        tr1[k,1]=tr$statistic # valor de t (Standardized difference en Xlstat)
                        tr2[k,1]=tr$stderr # Standard error of the mean (SE)
                        tr3[k,1]=tr$p.value # p value
                        if (tr$p.value<(1-confid)){tr4[k,1]="Yes"}else{tr4[k,1]="No"}
                    }}

                    nivel_jar=mean(jar_raw)
                    nivel_alto=mean(high_raw)
                    nivel_bajo=mean(low_raw)

                    mlow[k,1]=nivel_bajo
                    mjar[k,1]=nivel_jar
                    mhigh[k,1]=nivel_alto

                    mdroph[k,1]=nivel_jar-nivel_alto
                    mdropl[k,1]=nivel_jar-nivel_bajo
                    jarpenalty[k,1]=nivel_jar-weighted.mean(c(nivel_alto,nivel_bajo),c(n_high[k,1],n_low[k,1]),na.rm=TRUE)
                }
            }

            # zero buffer
            qtl=data.frame(rep(NA,N))
            sel=data.frame(rep(NA,N))
            p_tukeyl=data.frame(rep(NA,N))
            trl=data.frame(rep(NA,N))
            qth=data.frame(rep(NA,N))
            seh=data.frame(rep(NA,N))
            p_tukeyh=data.frame(rep(NA,N))
            trh=data.frame(rep(NA,N))

            #self$results$text$setContent(f_low>0)

            # Apply Tukey test
            if (self$options$posthoc & length(like)){
                confid1=(self$options$posthocalpha)/100
                y1=like[,1]
                x1=as.character(like)
                for (k in 1:N){
                    x1[atr[,k]==jjar]="jar"
                    x1[atr[,k]>jjar]="high"
                    x1[atr[,k]<jjar]="low"

                    if (length(unique(x1))>1 & f_jar[k,1]>0){
                        model=data.frame(y1,x1)
                        fm1=aov(y1~x1,data=model)
                        df=fm1$df.residual # degree of freedom
                        a=TukeyHSD(fm1)
                        pval0=a$x1[,4]

                        if (length(pval0)>1){
                            pvalues=pval0[grepl("jar",names(pval0))]
                            pvallow=pvalues[grepl("low",names(pvalues))]
                            pvalhigh=pvalues[grepl("high",names(pvalues))]
                        }else{
                            pvallow=pval0[1]
                            pvalhigh=pval0[1]
                            }

                        if (f_low[k,1]>0){
                            if (pvallow<(1-confid1)){trl[k,1]="Yes"}else{trl[k,1]="No"}
                            q0=qtukey(pvallow,3,df,lower.tail = FALSE)
                            qtl[k,1]=q0/sqrt(2) # idem XLStat - qvalue
                            sel[k,1]=mdropl[k,1]/qtl[k,1] # Standard error of the mean (SE)
                            p_tukeyl[k,1]=pvallow # ptukey
                        }

                        if (f_high[k,1]>0){
                            if (pvalhigh<(1-confid1)){trh[k,1]="Yes"}else{trh[k,1]="No"}
                            q1=qtukey(pvalhigh,3,df,lower.tail = FALSE)
                            qth[k,1]=q1/sqrt(2) # idem XLStat - tvalue
                            seh[k,1]=mdroph[k,1]/qth[k,1] # Standard error of the mean (SE)
                            p_tukeyh[k,1]=pvalhigh # ptukey
                        }
                    }
                }
            }

            # Get data for plotting
            if (self$options$showternary & N){
                plotData=cbind(n_jar,n_high,n_low)
                image <- self$results$ternaplot
                image$setState(plotData) # save data
                #image$setVisible(TRUE)
            }

            if (self$options$showbarras & N){
                plotData3=t(cbind(n_low,n_jar,n_high))
                image3 <- self$results$barraplot
                image3$setState(plotData3) # save data
                #image3$setVisible(TRUE)
            }

            if (self$options$showdiagnose & N & length(like)){
                plotData2=cbind(mdroph,mdropl,n_high,n_low,names_high,names_low)
                image2 <- self$results$diagplot
                image2$setState(plotData2) # save data
                #image2$setVisible(TRUE)
            }

            # Results - consumers
            if (N){
                table <- self$results$Consumidores
                for (xk in 1:N){
                    table$setRow(rowNo=xk, values=list(
                    var=atr_names[xk],
                    flow=f_low[xk,1],
                    fjar=f_jar[xk,1],
                    fhigh=f_high[xk,1],
                    consulow=n_low[xk,1],
                    consujar=n_jar[xk,1],
                    consuhigh=n_high[xk,1]
                    ))} # close dinamic table
                }

            #self$results$text$setContent(table2$visible)

            # Result table 2 - Attribute penalty table
            if (length(like) & N){
                table2 <- self$results$penalizacion
                for (xk in 1:N){
                    if (is.nan(jarpenalty[xk,1])){res0=as.character(NA)}else{res0=jarpenalty[xk,1]}
                    if (is.na(tr1[xk,1])){res1=as.character(tr1[xk,1])}else{res1=tr1[xk,1]}
                    if (is.na(tr2[xk,1])){res2=as.character(tr2[xk,1])}else{res2=tr2[xk,1]}
                    if (is.na(tr3[xk,1])){res3=as.character(tr3[xk,1])}else{res3=tr3[xk,1]}
                    table2$setRow(rowNo=xk, values=list(
                        var=atr_names[xk],
                        penalty=res0,
                        penaltyttest=res1,
                        penaltyse=res2,
                        penaltyp=res3,
                        penaltysig=tr4[xk,1]
                    ))}
            }

            # Result table 3 - Level penalty table
            if (length(like) & N){
                table3 <- self$results$MeanDropLow
                for (xk in 1:N){
                    if (is.nan(mdropl[xk,1])){res0=as.character(NA)}else{res0=mdropl[xk,1]}
                    if (is.na(qtl[xk,1])){res1=as.character(qtl[xk,1])}else{res1=qtl[xk,1]}
                    if (is.na(sel[xk,1])){res2=as.character(sel[xk,1])}else{res2=sel[xk,1]}
                    if (is.na(p_tukeyl[xk,1])){res3=as.character(p_tukeyl[xk,1])}else{res3=p_tukeyl[xk,1]}
                    table3$setRow(rowNo=xk, values=list(
                        var=atr_names[xk],
                        droplow=res0,
                        qlow=res1,
                        selow=res2,
                        tukeylow=res3,
                        siglow=trl[xk,1]
                    ))}
            }

            # Result table 4 - Level penalty table
            if (length(like) & N){
                table4 <- self$results$MeanDropHigh
                for (xk in 1:N){
                    if (is.nan(mdroph[xk,1])){res0=as.character(NA)}else{res0=mdroph[xk,1]}
                    if (is.na(qth[xk,1])){res1=as.character(qth[xk,1])}else{res1=qth[xk,1]}
                    if (is.na(seh[xk,1])){res2=as.character(seh[xk,1])}else{res2=seh[xk,1]}
                    if (is.na(p_tukeyh[xk,1])){res3=as.character(p_tukeyh[xk,1])}else{res3=p_tukeyh[xk,1]}
                    table4$setRow(rowNo=xk, values=list(
                        var=atr_names[xk],
                        drophigh=res0,
                        qhigh=res1,
                        sehigh=res2,
                        tukeyhigh=res3,
                        sighigh=trh[xk,1]
                    ))}
            }
        },
        .plot=function(image,...) {  # <-- TERNARY PLOT
            plotData <- image$state
            TernaryPlot(point="up",atip="JAR",btip="Too High",ctip="Too Low",alab="JAR % \u2192",blab="High % \u2192",clab="Low % \u2190")
            TernaryPoints(plotData, pch = 16)
            plotLabels = plotData
            plotLabels[,1]=plotLabels[,1]*0.95
            plotLabels[,2]=plotLabels[,2]*1.05
            TernaryText(plotLabels,labels=self$options$attr,col="blue")
            TRUE
        },
        .plot2=function(image2,...) {  # <-- DIAGNOSIS PLOT
            plotData <- image2$state
            freq=c(plotData[,3],plotData[,4])
            mdrop=c(plotData[,1],plotData[,2])
            plotlabels=c(plotData[,5],plotData[,6])

            xmax=max(freq)
            xl=c(0,ceiling(xmax))
            if(floor(min(mdrop,na.rm=TRUE))>0){
                yl=c(0,ceiling(max(mdrop,na.rm=TRUE)))
            } else {
                yl=c(floor(min(mdrop,na.rm=TRUE)),ceiling(max(mdrop,na.rm=TRUE)))
            }
            # Diagnose plot
            plot(plotData[,3],plotData[,1],pch="+",col="red",ylim=yl,xlim=xl,xlab="% of customer criticizing",ylab="Mean drop for overall liking")
            points(plotData[,4],plotData[,2],pch="-",col="blue",ylim=yl,xlim=xl,xlab="% of customer criticizing",ylab="Mean drop for overall liking")
            text(freq,mdrop,labels=plotlabels,cex=0.8,font=1,pos=2)
            legend("bottomleft",pch=c("+","-"),c("High","Low"),col=c("red","blue"),horiz = TRUE,bty="n")
            abline(h=self$options$mdropthreshold,lwd=2,lty=3)
            abline(v=self$options$threshold,lwd=2,lty=3)
            TRUE
        },
        .plot3=function(image3,...) {  # <-- BARPLOT
            plotData3 <- image3$state
            atr_names=self$options$attr
            # Bar diagram
            barplot(plotData3,names.arg=atr_names,xlab ="% of customer criticizing (cumulative)",ylab="Attributes",axes=TRUE,col=c("cyan","green","red"),horiz=TRUE)
            legend("bottomleft",c("Low","JAR","High"),cex = 0.9,fill=c("cyan","green","red"),horiz = TRUE,bty="n")
            TRUE
            }) # Close - List
) # Close - R6::R6Class
