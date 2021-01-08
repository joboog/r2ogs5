
#' input_add_pqc_skeybloc
#' @description Adds a keyword specific **pqc** sub-bloc to an *ogs5* simulation
#'   object to define the input of the chemical solver [Phreeqc]
#'   (https://www.usgs.gov/software/phreeqc-version-3/). For further
#'   documentation see [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/pqc.html)
#' @param x *ogs5* simulation object.
#' @param skey Phreeqc keyword: c("EQUILIBRIUM_PHASES", "EXCHANGE", "GAS_PHASE",
#'   "ISOTOPES", "KINETICS", "KNOBS", "RATES", "REACTION", "SOLID_SOLUTION",
#'   "SOLUTION", "SURFACE"). For Phreeqc documentation [see]
#'   (https://water.usgs.gov/water-resources/software/PHREEQC/documentation/phreeqc3-html/phreeqc3.htm)
#' @param bloc_text *character* defining the sub-bloc.
#'
#' @return Updated *ogs5* object.
#' @export
input_add_pqc_skeybloc <-

  function(
    x = list(),
    skey = c("EQUILIBRIUM_PHASES",
             "EXCHANGE",
             "GAS_PHASE",
             "ISOTOPES",
             "KINETICS",
             "KNOBS",
             "RATES",
             "REACTION",
             "SOLID_SOLUTION",
             "SOLUTION",
             "SURFACE"
            ),
    bloc_text = character(NULL)

  ){

    bloc_name <- skey

    # match function arguments
    skey <- match.arg(skey)

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$pqc exists and valid, otherwise create
    if (!("pqc" %in% names(x$input))) {
      x$input$pqc <- create_ogs5_pqc()
    } else {

      valid_ogs5_pqc(x$input$pqc)

      if (bloc_name %in% names(x$input$pqc)) {
        stop("similar keyword bloc already defined", call. = FALSE)
      }
    }

    # create and add sublist to pqc-list
    x$input$pqc[[paste(bloc_name)]] <- bloc_text %>%
                                        structure(class = "ogs5_pqc_skeybloc")

    valid_ogs5_pqc_skeybloc(x$input$pqc[[paste(bloc_name)]])

    return(x)

  }


#' input_add_pqcfile_as_pqc_bloc
#' @description Add an entire ***.pqc** input file as **pqc** bloc to an *ogs5*
#'   simulation object to define the input of the chemical solver [Phreeqc]
#'   (https://www.usgs.gov/software/phreeqc-version-3/). For further
#'   documentation see [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/pqc.html)
#' @param x *ogs5* simulation object.
#' @param pqc_filename Path to the ***.pqc** input file. *character*
#'
#' @return Updated *ogs5* object.
#' @export
input_add_pqcfile_as_pqc_bloc <-

  function(x = list(), pqc_filename = character(NULL)){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$pqc exists and valid, otherwise create
    if ("pqc" %in% names(x$input)) {
      stop("pqc bloc already defined", call. = FALSE)
    }

    # read file
    pqc_file = read_file(file = pqc_filename)

    # create pqc-sublist
    x$input$pqc <- pqc_file %>% structure(class = "ogs5_pqc_filebloc")

    valid_ogs5_pqc_filebloc(x$input$pqc)

    return(x)
  }
