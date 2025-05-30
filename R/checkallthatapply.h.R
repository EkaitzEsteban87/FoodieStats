
# This file is automatically generated, you probably don't want to edit this

CATAOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "CATAOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            showdescription = FALSE,
            cons = NULL,
            produ = NULL,
            lik = NULL,
            attr = NULL,
            alphaval = 5,
            showattricheck = TRUE,
            showconsucheck = TRUE,
            showheatmap = TRUE,
            showproductable = FALSE,
            showmusthave = FALSE,
            showelicitation = FALSE, ...) {

            super$initialize(
                package="FoodieStats",
                name="CATA",
                requiresData=TRUE,
                ...)

            private$..showdescription <- jmvcore::OptionBool$new(
                "showdescription",
                showdescription,
                default=FALSE)
            private$..cons <- jmvcore::OptionVariable$new(
                "cons",
                cons,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..produ <- jmvcore::OptionVariable$new(
                "produ",
                produ,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..lik <- jmvcore::OptionVariable$new(
                "lik",
                lik,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..attr <- jmvcore::OptionVariables$new(
                "attr",
                attr,
                suggested=list(
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..alphaval <- jmvcore::OptionNumber$new(
                "alphaval",
                alphaval,
                default=5,
                min=1,
                max=25)
            private$..showattricheck <- jmvcore::OptionBool$new(
                "showattricheck",
                showattricheck,
                default=TRUE)
            private$..showconsucheck <- jmvcore::OptionBool$new(
                "showconsucheck",
                showconsucheck,
                default=TRUE)
            private$..showheatmap <- jmvcore::OptionBool$new(
                "showheatmap",
                showheatmap,
                default=TRUE)
            private$..showproductable <- jmvcore::OptionBool$new(
                "showproductable",
                showproductable,
                default=FALSE)
            private$..showmusthave <- jmvcore::OptionBool$new(
                "showmusthave",
                showmusthave,
                default=FALSE)
            private$..showelicitation <- jmvcore::OptionBool$new(
                "showelicitation",
                showelicitation,
                default=FALSE)

            self$.addOption(private$..showdescription)
            self$.addOption(private$..cons)
            self$.addOption(private$..produ)
            self$.addOption(private$..lik)
            self$.addOption(private$..attr)
            self$.addOption(private$..alphaval)
            self$.addOption(private$..showattricheck)
            self$.addOption(private$..showconsucheck)
            self$.addOption(private$..showheatmap)
            self$.addOption(private$..showproductable)
            self$.addOption(private$..showmusthave)
            self$.addOption(private$..showelicitation)
        }),
    active = list(
        showdescription = function() private$..showdescription$value,
        cons = function() private$..cons$value,
        produ = function() private$..produ$value,
        lik = function() private$..lik$value,
        attr = function() private$..attr$value,
        alphaval = function() private$..alphaval$value,
        showattricheck = function() private$..showattricheck$value,
        showconsucheck = function() private$..showconsucheck$value,
        showheatmap = function() private$..showheatmap$value,
        showproductable = function() private$..showproductable$value,
        showmusthave = function() private$..showmusthave$value,
        showelicitation = function() private$..showelicitation$value),
    private = list(
        ..showdescription = NA,
        ..cons = NA,
        ..produ = NA,
        ..lik = NA,
        ..attr = NA,
        ..alphaval = NA,
        ..showattricheck = NA,
        ..showconsucheck = NA,
        ..showheatmap = NA,
        ..showproductable = NA,
        ..showmusthave = NA,
        ..showelicitation = NA)
)

CATAResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "CATAResults",
    inherit = jmvcore::Group,
    active = list(
        description = function() private$.items[["description"]],
        attricheck = function() private$.items[["attricheck"]],
        consucheck = function() private$.items[["consucheck"]],
        heatmap = function() private$.items[["heatmap"]],
        heatmapdata = function() private$.items[["heatmapdata"]],
        productable = function() private$.items[["productable"]],
        attrCochranQ = function() private$.items[["attrCochranQ"]],
        contingencytable = function() private$.items[["contingencytable"]],
        musthaveplot = function() private$.items[["musthaveplot"]],
        elicitationplots = function() private$.items[["elicitationplots"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Check All That Apply")
            self$add(jmvcore::Html$new(
                options=options,
                name="description",
                title="description",
                visible="(showdescription)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="attricheck",
                title="Validation of checked attributes",
                width=640,
                height=320,
                renderFun=".attricheck",
                visible="(showattricheck)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="consucheck",
                title="Validation of checked attributes for each consumer",
                width=960,
                height=320,
                renderFun=".consucheck",
                visible="(showconsucheck)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="heatmap",
                title="Checked percentage for each consumer and each attribute",
                width=640,
                height=640,
                renderFun=".heatmap",
                visible="(showheatmap)"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="heatmapdata"))
            self$add(jmvcore::Table$new(
                options=options,
                name="productable",
                title="Product Percentage Table",
                visible="(showproductable)",
                columns=list()))
            self$add(jmvcore::Table$new(
                options=options,
                name="attrCochranQ",
                title="Cochran Q test for each attribute",
                rows="(attr)",
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Attributes", 
                        `type`="text", 
                        `content`="($key)", 
                        `combineBelow`=TRUE),
                    list(
                        `name`="attrQ", 
                        `title`="Cochran Q <sub>value</sub>", 
                        `type`="number"),
                    list(
                        `name`="attrQp", 
                        `title`="p <sub>value</sub>", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="commQ", 
                        `title`="Comments", 
                        `type`="text"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="contingencytable",
                title="Contingency Table for Test of independence between the products and attributes",
                columns=list()))
            self$add(jmvcore::Image$new(
                options=options,
                name="musthaveplot",
                title="Penalty analysis plot (Must have - Nice to have)",
                width=640,
                height=640,
                renderFun=".musthaveplot",
                visible="(showmusthave)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="elicitationplots",
                title="Elicitation difference for each product",
                width=640,
                height=640,
                renderFun=".elicitationplots",
                visible="(showelicitation)"))}))

CATABase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "CATABase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "FoodieStats",
                name = "CATA",
                version = c(1,0,0),
                options = options,
                results = CATAResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Check All That Apply
#'
#' 
#' @param data .
#' @param showdescription .
#' @param cons .
#' @param produ .
#' @param lik .
#' @param attr .
#' @param alphaval .
#' @param showattricheck .
#' @param showconsucheck .
#' @param showheatmap .
#' @param showproductable .
#' @param showmusthave .
#' @param showelicitation .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$description} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$attricheck} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$consucheck} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$heatmap} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$heatmapdata} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$productable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$attrCochranQ} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$contingencytable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$musthaveplot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$elicitationplots} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$productable$asDF}
#'
#' \code{as.data.frame(results$productable)}
#'
#' @export
CATA <- function(
    data,
    showdescription = FALSE,
    cons,
    produ,
    lik,
    attr,
    alphaval = 5,
    showattricheck = TRUE,
    showconsucheck = TRUE,
    showheatmap = TRUE,
    showproductable = FALSE,
    showmusthave = FALSE,
    showelicitation = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("CATA requires jmvcore to be installed (restart may be required)")

    if ( ! missing(cons)) cons <- jmvcore::resolveQuo(jmvcore::enquo(cons))
    if ( ! missing(produ)) produ <- jmvcore::resolveQuo(jmvcore::enquo(produ))
    if ( ! missing(lik)) lik <- jmvcore::resolveQuo(jmvcore::enquo(lik))
    if ( ! missing(attr)) attr <- jmvcore::resolveQuo(jmvcore::enquo(attr))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(cons), cons, NULL),
            `if`( ! missing(produ), produ, NULL),
            `if`( ! missing(lik), lik, NULL),
            `if`( ! missing(attr), attr, NULL))

    for (v in cons) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in produ) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in attr) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- CATAOptions$new(
        showdescription = showdescription,
        cons = cons,
        produ = produ,
        lik = lik,
        attr = attr,
        alphaval = alphaval,
        showattricheck = showattricheck,
        showconsucheck = showconsucheck,
        showheatmap = showheatmap,
        showproductable = showproductable,
        showmusthave = showmusthave,
        showelicitation = showelicitation)

    analysis <- CATAClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

