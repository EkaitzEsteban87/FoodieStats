
# This file is automatically generated, you probably don't want to edit this

designmixturemodelOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "designmixturemodelOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            showdescription = FALSE,
            yield = NULL,
            vars = NULL,
            choosemodel = "1",
            showcox = FALSE,
            xaxis = "centroid",
            choosevars = "original",
            showresidual = FALSE,
            showmixture = FALSE,
            thetaval = -25,
            phival = 20,
            colorselection = "SpectralStylish",
            latentvar = "1",
            latentplane = "0", ...) {

            super$initialize(
                package="FoodieStats",
                name="designmixturemodel",
                requiresData=TRUE,
                ...)

            private$..showdescription <- jmvcore::OptionBool$new(
                "showdescription",
                showdescription,
                default=FALSE)
            private$..yield <- jmvcore::OptionVariable$new(
                "yield",
                yield,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..choosemodel <- jmvcore::OptionList$new(
                "choosemodel",
                choosemodel,
                options=list(
                    "1",
                    "2",
                    "4",
                    "3",
                    "1.5"),
                default="1")
            private$..showcox <- jmvcore::OptionBool$new(
                "showcox",
                showcox,
                default=FALSE)
            private$..xaxis <- jmvcore::OptionList$new(
                "xaxis",
                xaxis,
                options=list(
                    "centroid",
                    "mixtureproportion"),
                default="centroid")
            private$..choosevars <- jmvcore::OptionList$new(
                "choosevars",
                choosevars,
                options=list(
                    "original",
                    "mixtures"),
                default="original")
            private$..showresidual <- jmvcore::OptionBool$new(
                "showresidual",
                showresidual,
                default=FALSE)
            private$..showmixture <- jmvcore::OptionBool$new(
                "showmixture",
                showmixture,
                default=FALSE)
            private$..thetaval <- jmvcore::OptionNumber$new(
                "thetaval",
                thetaval,
                default=-25,
                min=-180,
                max=180)
            private$..phival <- jmvcore::OptionNumber$new(
                "phival",
                phival,
                default=20,
                min=-180,
                max=180)
            private$..colorselection <- jmvcore::OptionList$new(
                "colorselection",
                colorselection,
                options=list(
                    "SpectralStylish",
                    "JamoviStylish",
                    "BeachStylish",
                    "OceanStylish",
                    "Market",
                    "Gingerbread",
                    "Umami",
                    "SummerSalad",
                    "FruitCup",
                    "Delight",
                    "Greensmoothie",
                    "Turmeric",
                    "Snacks",
                    "Sashimi",
                    "Macarons",
                    "Feast",
                    "Veggies",
                    "Beetroot",
                    "Spice",
                    "Sweet",
                    "Tropical",
                    "Breakfast",
                    "Carrots",
                    "Stick",
                    "Tea",
                    "Decadent",
                    "Berry",
                    "Sugary",
                    "Citrus",
                    "Chard",
                    "Shrimp",
                    "SweetSalad",
                    "Cheesecake",
                    "FruitBasket",
                    "Cayenne",
                    "Smoothie",
                    "Mojito",
                    "Coffee",
                    "Wine",
                    "Beer",
                    "Fish",
                    "Cheese",
                    "Bread",
                    "Chocolate",
                    "Beef",
                    "Honey",
                    "Dairy",
                    "Vegetables",
                    "Bean",
                    "Seafood",
                    "Octopus"),
                default="SpectralStylish")
            private$..latentvar <- jmvcore::OptionList$new(
                "latentvar",
                latentvar,
                options=list(
                    "1",
                    "2",
                    "3",
                    "4"),
                default="1")
            private$..latentplane <- jmvcore::OptionList$new(
                "latentplane",
                latentplane,
                options=list(
                    "0",
                    "0.1",
                    "0.2",
                    "0.3",
                    "0.4",
                    "0.5",
                    "0.6",
                    "0.7",
                    "0.8"),
                default="0")
            private$..writeexcel <- jmvcore::OptionOutput$new(
                "writeexcel")

            self$.addOption(private$..showdescription)
            self$.addOption(private$..yield)
            self$.addOption(private$..vars)
            self$.addOption(private$..choosemodel)
            self$.addOption(private$..showcox)
            self$.addOption(private$..xaxis)
            self$.addOption(private$..choosevars)
            self$.addOption(private$..showresidual)
            self$.addOption(private$..showmixture)
            self$.addOption(private$..thetaval)
            self$.addOption(private$..phival)
            self$.addOption(private$..colorselection)
            self$.addOption(private$..latentvar)
            self$.addOption(private$..latentplane)
            self$.addOption(private$..writeexcel)
        }),
    active = list(
        showdescription = function() private$..showdescription$value,
        yield = function() private$..yield$value,
        vars = function() private$..vars$value,
        choosemodel = function() private$..choosemodel$value,
        showcox = function() private$..showcox$value,
        xaxis = function() private$..xaxis$value,
        choosevars = function() private$..choosevars$value,
        showresidual = function() private$..showresidual$value,
        showmixture = function() private$..showmixture$value,
        thetaval = function() private$..thetaval$value,
        phival = function() private$..phival$value,
        colorselection = function() private$..colorselection$value,
        latentvar = function() private$..latentvar$value,
        latentplane = function() private$..latentplane$value,
        writeexcel = function() private$..writeexcel$value),
    private = list(
        ..showdescription = NA,
        ..yield = NA,
        ..vars = NA,
        ..choosemodel = NA,
        ..showcox = NA,
        ..xaxis = NA,
        ..choosevars = NA,
        ..showresidual = NA,
        ..showmixture = NA,
        ..thetaval = NA,
        ..phival = NA,
        ..colorselection = NA,
        ..latentvar = NA,
        ..latentplane = NA,
        ..writeexcel = NA)
)

designmixturemodelResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "designmixturemodelResults",
    inherit = jmvcore::Group,
    active = list(
        description = function() private$.items[["description"]],
        doetable = function() private$.items[["doetable"]],
        doetable2 = function() private$.items[["doetable2"]],
        doeanova = function() private$.items[["doeanova"]],
        anovatable = function() private$.items[["anovatable"]],
        estitabletitle = function() private$.items[["estitabletitle"]],
        estitable = function() private$.items[["estitable"]],
        designcoded = function() private$.items[["designcoded"]],
        maintable = function() private$.items[["maintable"]],
        designuncoded = function() private$.items[["designuncoded"]],
        scalingtable = function() private$.items[["scalingtable"]],
        cols = function() private$.items[["cols"]],
        ternarydata = function() private$.items[["ternarydata"]],
        coxplot = function() private$.items[["coxplot"]],
        residualplot = function() private$.items[["residualplot"]],
        mixture4kplot = function() private$.items[["mixture4kplot"]],
        writeexcel = function() private$.items[["writeexcel"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Mixture Design")
            self$add(jmvcore::Html$new(
                options=options,
                name="description",
                title="description",
                visible="(showdescription)"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="doetable",
                title="Experimental and Predicted Responses Table"))
            self$add(jmvcore::Table$new(
                options=options,
                name="doetable2",
                title="Experimental Design Table",
                columns=list()))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="doeanova",
                title="ANOVA Table"))
            self$add(jmvcore::Table$new(
                options=options,
                name="anovatable",
                title="`Analysis of Variance Table - Response: ${yield}`",
                columns=list()))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="estitabletitle",
                title="Model Parameter Estimation Table"))
            self$add(jmvcore::Table$new(
                options=options,
                name="estitable",
                title="Parameter Estimation Table",
                columns=list()))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="designcoded",
                title="Mixture Model Prediction"))
            self$add(jmvcore::Table$new(
                options=options,
                name="maintable",
                title="`Mixture Model - response: ${yield}`",
                rows=1,
                columns=list(
                    list(
                        `name`="equ1", 
                        `title`="Mixture model equation", 
                        `type`="text"),
                    list(
                        `name`="Rsq1", 
                        `title`="R<sup>2</sup>", 
                        `type`="number"),
                    list(
                        `name`="Ad1", 
                        `title`="R<sub>A</sub><sup>2</sup>", 
                        `type`="number"),
                    list(
                        `name`="Mse1", 
                        `title`="MSE", 
                        `type`="number"))))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="designuncoded",
                title="Mixture Proportions"))
            self$add(jmvcore::Table$new(
                options=options,
                name="scalingtable",
                title="Mixture Scaling Table",
                columns=list()))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="cols"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="ternarydata"))
            self$add(jmvcore::Image$new(
                options=options,
                name="coxplot",
                title="Main Effects Plot",
                width=960,
                height=480,
                renderFun=".coxplot",
                requiresData=TRUE,
                visible="(showcox)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="residualplot",
                title="Residual Plot",
                width=960,
                height=480,
                renderFun=".residualplot",
                requiresData=TRUE,
                visible="(showresidual)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="mixture4kplot",
                title="Mixture Plots",
                width=960,
                height=480,
                renderFun=".mixture4kplot",
                requiresData=TRUE,
                visible="(showmixture)"))
            self$add(jmvcore::Output$new(
                options=options,
                name="writeexcel",
                title="Write Design Model",
                initInRun=TRUE,
                clearWith=list(
                    "vars",
                    "yield",
                    "choosemodel")))}))

designmixturemodelBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "designmixturemodelBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "FoodieStats",
                name = "designmixturemodel",
                version = c(1,0,0),
                options = options,
                results = designmixturemodelResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'none')
        }))

#' Mixture Design
#'
#' 
#' @param data .
#' @param showdescription .
#' @param yield .
#' @param vars .
#' @param choosemodel .
#' @param showcox .
#' @param xaxis .
#' @param choosevars .
#' @param showresidual .
#' @param showmixture .
#' @param thetaval .
#' @param phival .
#' @param colorselection .
#' @param latentvar .
#' @param latentplane .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$description} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$doetable} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$doetable2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$doeanova} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$anovatable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$estitabletitle} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$estitable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$designcoded} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$maintable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$designuncoded} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$scalingtable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$cols} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$ternarydata} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$coxplot} \tab \tab \tab \tab \tab the main effects plot \cr
#'   \code{results$residualplot} \tab \tab \tab \tab \tab a residual plot \cr
#'   \code{results$mixture4kplot} \tab \tab \tab \tab \tab Mixture plot for 3 and 4 variables \cr
#'   \code{results$writeexcel} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$doetable2$asDF}
#'
#' \code{as.data.frame(results$doetable2)}
#'
#' @export
designmixturemodel <- function(
    data,
    showdescription = FALSE,
    yield,
    vars,
    choosemodel = "1",
    showcox = FALSE,
    xaxis = "centroid",
    choosevars = "original",
    showresidual = FALSE,
    showmixture = FALSE,
    thetaval = -25,
    phival = 20,
    colorselection = "SpectralStylish",
    latentvar = "1",
    latentplane = "0") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("designmixturemodel requires jmvcore to be installed (restart may be required)")

    if ( ! missing(yield)) yield <- jmvcore::resolveQuo(jmvcore::enquo(yield))
    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(yield), yield, NULL),
            `if`( ! missing(vars), vars, NULL))

    for (v in vars) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- designmixturemodelOptions$new(
        showdescription = showdescription,
        yield = yield,
        vars = vars,
        choosemodel = choosemodel,
        showcox = showcox,
        xaxis = xaxis,
        choosevars = choosevars,
        showresidual = showresidual,
        showmixture = showmixture,
        thetaval = thetaval,
        phival = phival,
        colorselection = colorselection,
        latentvar = latentvar,
        latentplane = latentplane)

    analysis <- designmixturemodelClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

