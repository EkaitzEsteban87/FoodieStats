sensorywheelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "sensorywheelClass",
    inherit = sensorywheelBase,
    private = list(
        .init = function() {
            showdesc=self$options$showdescription
            if (showdesc){
            self$results$description$setContent(
              '<html>
                <head>
                <title>The Sensory Wheel</title>
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
                background-color: white;
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
                <h2>Introduction to the Sensory Wheel</h2>
                <p>The Sensory Wheel is a powerful tool used in various fields, including wine tasting and coffee evaluation. It helps individuals describe and understand complex sensory experiences by categorizing aromas, flavors, and sensations into a structured framework.</p>
                
                <h2>Components of the Sensory Wheel</h2>
                <p>The Sensory Wheel typically consists of concentric circles, each representing a category of sensory attributes. The inner circles describe general or primary sensations, while the outer circles provide more specific details.</p>
                
                <h2>How to Use the Sensory Wheel</h2>
                <p>When evaluating a product, one starts from the inner circle and works outward. Begin by identifying the primary sensation (e.g., fruity or floral), then move to the outer circles to specify the exact attributes (e.g., citrus or rose). This method allows for a precise and standardized way of describing sensory experiences.</p>
                
                </div>
                </body>
                </html>')}
        },
        .run = function() {
            require(jmvcore)
            require(ggsunburst) 
            lev_names=self$options$levv
            data0=self$data[lev_names]
            
            nrows=dim(data0)[1]
            ncols=dim(data0)[2]
            
            if (ncols>1){
              unique_counts <- sapply(data0, function(col) length(unique(col)))
              data0 <- data0[, order(unique_counts)]
            }            
            
            
            if (ncols>0){
              
              df=data0
              unique_words <- unique(unlist(df))
              unique_words <- levels(unique_words) 
              word_to_number <- as.numeric(factor(unique_words))
              df[] <- word_to_number[match(unlist(df), unique_words)]
              nwords=max(word_to_number) 
                            
              empty_df <- data.frame(matrix(NA, nrow = nrows, ncol = ncols))
              res <- table(factor(data0[,1], levels = unique(data0[,1]))) 
              nres=length(res)            
              color1=self$options$colorselection
              style1=self$options$wheelstyle
              npalette <- switch(   
                style1,   
                "unsorted"= nwords,   
                (nres+2)
              ) 
              
              basecolors <- switch(   
                color1,   
                "JamoviStylish"=colorRampPalette(c("#6B9DE8", "#E6AC40"))(npalette),
                "BeachStylish"=colorRampPalette(c("orange", "lightyellow", "lightblue", "lightgreen"))(npalette),
                "OceanStylish"=colorRampPalette(c("royalblue2", "lightgreen", "darkturquoise","lavender"))(npalette),
                "SummerSalad"=colorRampPalette(c("#8A3E73", "#FF282A", "#FBE62D", "#90CF44", "#59B512"))(npalette),
                "FruitCup"=colorRampPalette(c("#B42B4D", "#F40224", "#111920", "#FDB70D", "#C1C454"))(npalette),
                "Delight"=colorRampPalette(c("#FA4490", "#FCCED4", "#6CE4E7", "#DAF069", "#924B9C"))(npalette),
                "Greensmoothie"=colorRampPalette(c("#326414", "#428612", "#54AC5A", "#BCD76D", "#E1EDC1"))(npalette),
                "Turmeric"=colorRampPalette(c("#E28401", "#EC9D04", "#F0B51D", "#C83701", "#B00005"))(npalette),
                "Snacks"=colorRampPalette(c("#4E070C", "#D22701", "#FF670E", "#FDE4CE", "#AAD15F"))(npalette),
                "Market"=colorRampPalette(c("#3E5B8F", "#AB5B84", "#FFCA70", "#FFA789", "#FF6064"))(npalette),
                "Sashimi"=colorRampPalette(c("#F45F67", "#FC7100", "#FB8818", "#F5DDC2", "#5CA135"))(npalette),
                "Macarons"=colorRampPalette(c("#E85F81", "#F7BFCA", "#DDD8EC", "#E9CDC3", "#CDD96D"))(npalette),
                "Feast"=colorRampPalette(c("#883668", "#A01B2C", "#D50102", "#F15623", "#264E01"))(npalette),
                "Veggies"=colorRampPalette(c("#829461", "#ABB95B", "#FFF7D9", "#FFC559", "#9C5273"))(npalette),
                "Gingerbread"=colorRampPalette(c("#9B1B32", "#547061", "#F3F3F2", "#C69255", "#A1754F"))(npalette),
                "Beetroot"=colorRampPalette(c("#835D84", "#CE558F", "#D872A8", "#ACB885", "#EFA55A"))(npalette),
                "Spice"=colorRampPalette(c("#C80238", "#F92827", "#FDA501", "#C5C957", "#D19B66"))(npalette),
                "Sweet"=colorRampPalette(c("#F6C4C7", "#F1CCD3", "#F5F7F2", "#C1312E", "#951911"))(npalette),
                "Tropical"=colorRampPalette(c("#35417B", "#E22A4F", "#FA8D54", "#F4EB67", "#7EC487"))(npalette),
                "Breakfast"=colorRampPalette(c("#C6374B", "#D23F40", "#F7EAD4", "#F7D655", "#95A438"))(npalette),
                "Carrots"=colorRampPalette(c("#642B52", "#754599", "#9650B8", "#F9F8FD", "#FE7C39"))(npalette),
                "Stick"=colorRampPalette(c("#549829", "#93C154", "#F8FCFF", "#115B9A", "#613677"))(npalette),
                "Tea"=colorRampPalette(c("#AB002D", "#C00219", "#FFAC67", "#D7D797", "#A6C058"))(npalette),
                "Decadent"=colorRampPalette(c("#C64573", "#A6C769", "#FAFAF3", "#FDB14A", "#18AED4"))(npalette),
                "Umami"=colorRampPalette(c("#883A61", "#CC010A", "#E7A545", "#72B800", "#189E01"))(npalette),
                "Berry"=colorRampPalette(c("#A60839", "#ED0101", "#F9D3A0", "#435272", "#1D121B"))(npalette),
                "Sugary"=colorRampPalette(c("#FE7573", "#FFC8D0", "#C3AACD", "#FDEEAB", "#FED28B"))(npalette),
                "Citrus"=colorRampPalette(c("#D10134", "#FD423F", "#FE8000", "#FFB701", "#67AD00"))(npalette),
                "Chard"=colorRampPalette(c("#C43BA6", "#FE0165", "#F6F7F9", "#FFEB61", "#A0CD01"))(npalette),
                "Shrimp"=colorRampPalette(c("#69325A", "#CA0500", "#FA9D83", "#FE8B00", "#397C09"))(npalette),
                "SweetSalad"=colorRampPalette(c("#C92038", "#FF0101", "#A44000", "#FEAA00", "#6EA602"))(npalette),
                "Cheesecake"=colorRampPalette(c("#D90024", "#E20437", "#FCFBF7", "#2B4456", "#11263B"))(npalette),
                "FruitBasket"=colorRampPalette(c("#622B3D", "#FD2A82", "#FF841F", "#FADF3A", "#41522E"))(npalette),
                "Cayenne"=colorRampPalette(c("#E20101", "#EF5400", "#F5E5D6", "#FDC707", "#724635"))(npalette),
                "Smoothie"=colorRampPalette(c("#eedbc0", "#efbf26", "#b3a126", "#cc8e23", "#d4b729"))(npalette),
                "Mojito"=colorRampPalette(c("#8b967e", "#a0ad8d", "#c3cd79", "#e3ecaa", "#5c7444"))(npalette),
                "Coffee"=colorRampPalette(c("#ccb1a0", "#f1e2d1", "#907966", "#6c442b", "#f4cc5c"))(npalette),
                "Wine"=colorRampPalette(c("#ffb9b9", "#ee7272", "#a31818", "#6d0202", "#360000"))(npalette),
                "Beer"=colorRampPalette(c("#FFF897", "#FAE96F", "#F6C101", "#EC9D00", "#DF8D03", "#C96E12"))(npalette),
                "Fish"=colorRampPalette(c("#7e7d9e", "#a6adbe", "#577ea2", "#306598", "#002a65"))(npalette),
                "Cheese"=colorRampPalette(c("#f7df47", "#f9e02e", "#f9d02e", "#f9c02e", "#f9b02e"))(npalette),
                "Bread"=colorRampPalette(c("#e5ccac", "#f2d3a1", "#f2c480", "#b07645", "#8b5220"))(npalette),
                "Chocolate"=colorRampPalette(c("#310A0B", "#491B1D", "#743A36", "#B96A59", "#E0A387"))(npalette),
                "Beef"=colorRampPalette(c("#F7D5D4", "#E8B3B9", "#CB6862", "#AA3C3B", "#E97856", "#FCB79A"))(npalette),
                "Honey"=colorRampPalette(c("#f9c901", "#f6e000", "#985b10", "#6b4701", "#896800"))(npalette),
                "Dairy"=colorRampPalette(c("#ffffff", "#ffe8ee", "#d8f4ff", "#f6eee1", "#f2e5d5"))(npalette),
                "Vegetables"=colorRampPalette(c("#457d00", "#c6c736", "#96b125", "#fea938", "#fcec9a"))(npalette),
                "Bean"=colorRampPalette(c("#736731", "#99a55a", "#e5d080", "#8c683b", "#795a3b"))(npalette),
                "Seafood"=colorRampPalette(c("#37412a", "#24b4ab", "#9fe3c1", "#ffffff", "#fa8072"))(npalette),
                "Octopus"=colorRampPalette(c("#a6ffff", "#ffaffa", "#ee00a4", "#d79eff", "#a60033"))(npalette),
                "Random"= sample(colors(), npalette),  
                "Gummy"= rainbow(npalette),
                "Cm"= cm.colors(npalette),   
                "Topo"= topo.colors(npalette),   
                "Terrain"= terrain.colors(npalette), 
                "Heat"= heat.colors(npalette), 
                hcl.colors(npalette, palette = color1) 
              ) 
              
              if (style1=="unsorted"){
                wheelcolors=basecolors}
              else{
                basecolors=basecolors[1:nres]
              }

              if (style1=="sorted"){
                for (z1 in 1:ncols){
                  row0=0
                  for (z2 in 1:nres){
                    colores_degradados <- basecolors[z2]
                    ntimes=as.numeric(res[z2])
                    for (z3 in 1:ntimes){
                      row0=row0+1
                      empty_df[row0,z1]=colores_degradados
                    }
                  }
                }
              }

              if (style1=="faded"){
                for (z1 in 1:ncols){
                  row0=0
                  for (z2 in 1:nres){
                    degradado_palette <- colorRampPalette(c(basecolors[z2], "white"), space = "rgb")
                    colores_degradados <- degradado_palette(ncols+2)
                    ntimes=as.numeric(res[z2])
                    for (z3 in 1:ntimes){
                      row0=row0+1
                      empty_df[row0,z1]=colores_degradados[z1]
                    }
                  }
                }
              }
              
              if (style1!="unsorted"){
                unique_colors <- matrix(ncol = 2, nrow = 0)
                for (i in 1:ncol(df)){
                  unique_colors <- unique(rbind(unique_colors, unique(cbind(df[, i], empty_df[, i]))))
                }
                unique_colors <- as.data.frame(unique_colors)
                colnames(unique_colors) <- c("Numero", "Color")
                df2 <- unique_colors[order(as.numeric(unique_colors$Numero)),]
                df2 <- df2[!duplicated(df2$Numero, fromLast = FALSE), ]
                wheelcolors=df2[,2]
              }
              
              leaf1=self$options$leaflength
              data0[,ncols] <- switch(   
                leaf1,   
                "Short"= paste(data0[,ncols],"->dist:0.5",sep=""),
                "Medium"= paste(data0[,ncols],"->dist:1.5",sep=""),
                "Large"= paste(data0[,ncols],"->dist:2",sep=""),
                "Verylarge"= paste(data0[,ncols],"->dist:2.5",sep=""),
                data0[,ncols]
              )
              
              add_attribute=""
              if (self$options$nodelabelrotation){add_attribute="radial"}
              
              plotData <- sunburst_data(data0, type = "lineage", sep=",",node_attributes =add_attribute)
              image <- self$results$wheelplot
              image$setState(plotData) 
              self$results$wheeloptions$setState(wheelcolors) 
            }
        },
        .plot=function(image,...) { 
            N=length(self$options$levv)
            if (N>0){
              plotData <- image$state
              wheelcolors <- self$results$wheeloptions$state
              node=self$options$nodesize
              leaf=self$options$leafsize
              labelcolor=self$options$labelcolors
              ap=sunburst(plotData, node_labels = T, node_labels.min = 1, leaf_labels.size = leaf, node_labels.size = node, leaf_labels.color = labelcolor,node_labels.color = labelcolor,rects.fill.aes = "name") + scale_fill_manual(values = wheelcolors, guide = F)
              self$results$showheelplot$setContent(ap) # show plot
              TRUE}
        }) 
) 