
fcoseDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = 'fcose', version = '1.0.0',
      src = system.file('htmlwidgets/lib/fcose', package = 'cytoscape'),
      script = c('cytoscape-fcose.js')
    )
  )
}

numericDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = 'numeric', version = '1.2.6',
      src = system.file('htmlwidgets/lib/numeric-1.2.6', package = 'cytoscape'),
      script = c('numeric-1.2.6.js')
    )
  )
}

coseBaseDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = 'cose-base', version = '1.0.0',
      src = system.file('htmlwidgets/lib/cose-base', package = 'cytoscape'),
      script = c('cose-base.js')
    )
  )
}

layoutBaseDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = 'layout-base', version = '1.0.0',
      src = system.file('htmlwidgets/lib/layout-base', package = 'cytoscape'),
      script = c('layout-base.js')
    )
  )
}

#' @title Cola Layout
#'
#' @param cytoscape
#' @param name
#' @param quality
#' @param randomize
#' @param animate
#' @param animationDuration
#' @param animationEasing
#' @param fit
#' @param padding
#' @param nodeDimensionsIncludeLabels
#' @param uniformNodeDimensions
#' @param packComponents
#' @param samplingType
#' @param sampleSize
#' @param nodeSeparation
#' @param piTol
#' @param nodeRepulsion
#' @param idealEdgeLength
#' @param edgeElasticity
#' @param nestingFactor
#' @param numIter
#' @param tile
#' @param tilingPaddingVertical
#' @param tilingPaddingHorizontal
#' @param gravity
#' @param gravityRangeCompound
#' @param gravityCompound
#' @param gravityRange
#' @param initialEnergyOnIncremental
#' @param ready
#' @param stop
#' @param ...
#'
#' @importFrom jsonlite toJSON
#' @importFrom htmlwidgets JS
#' @return
#' @export
#'
#' @examples
fcose_layout <- function(cytoscape,
                         name = 'fcose',
                         quality = 'default',
                         randomize = TRUE,
                         animate = TRUE,
                         animationDuration = 1000,
                         animationEasing = NULL,
                         fit = TRUE,
                         padding = 30,
                         nodeDimensionsIncludeLabels = FALSE,
                         uniformNodeDimensions = FALSE,
                         packComponents = TRUE,

                         # spectral layout options
                         samplingType = TRUE,
                         sampleSize = 25,
                         nodeSeparation = 75,
                         piTol = 0.0000001,

                        # incremental layout options
                        nodeRepulsion = 4500,
                        idealEdgeLength = 50,
                        edgeElasticity = 0.45,
                        nestingFactor = 0.1,
                        numIter = 2500,
                        tile = TRUE,
                        tilingPaddingVertical = 10,
                        tilingPaddingHorizontal = 10,
                        gravity = 0.25,
                        gravityRangeCompound = 1.5,
                        gravityCompound = 1.0,
                        gravityRange = 3.8,
                        initialEnergyOnIncremental = 0.3,

                        # layout event callbacks
                        ready = htmlwidgets::JS('() => {}'),
                        stop = htmlwidgets::JS('() => {}'),
                        ...){

   layout <- list(
       name = 'fcose',
       quality = quality,
       randomize = randomize,
       animate = animate,
       animationDuration = animationDuration,
       animationEasing = animationEasing,
       fit = fit,
       padding = padding,
       nodeDimensionsIncludeLabels = nodeDimensionsIncludeLabels,
       uniformNodeDimensions = uniformNodeDimensions,
       packComponents = packComponents,

       # spectral layout options
       samplingType = samplingType,
       sampleSize = sampleSize,
       nodeSeparation = nodeSeparation,
       piTol = piTol,

      # incremental layout options
      nodeRepulsion = nodeRepulsion,
      idealEdgeLength = idealEdgeLength,
      edgeElasticity = edgeElasticity,
      nestingFactor = nestingFactor,
      numIter = numIter,
      tile = tile,
      tilingPaddingVertical = tilingPaddingVertical,
      tilingPaddingHorizontal = tilingPaddingHorizontal,
      gravity = gravity,
      gravityRangeCompound = gravityRangeCompound,
      gravityCompound = gravityCompound,
      gravityRange = gravityRange,
      initialEnergyOnIncremental = initialEnergyOnIncremental,

      # layout event callbacks
      ready = ready,
      stop = stop
    )

    layout <- modifyList(layout, list(...))
    layout <- Filter(Negate(function(x) is.null(unlist(x))), layout)

    cytoscape$x$layout <- layout

    # add fcose dependencies here
    if (is.null(cytoscape$dependencies)) {
      cytoscape$dependencies <- c(fcoseDependency(), numericDependency(), coseBaseDependency(), layoutBaseDependency())
    } else {
      cytoscape$dependencies <- c(fcoseDependency(), numericDependency(), coseBaseDependency(), layoutBaseDependency(), cytoscape$dependencies)
    }

    return(cytoscape)
}
