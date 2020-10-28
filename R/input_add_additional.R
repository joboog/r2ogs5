
input_add_file_as_additional_bloc <-

    function(x = list(), filepath = character(NULL)){

        # add a complete file as bloc to the class 'ogs5_additional'
        # bloc will a one element list of a character()
        # input: ogs5-obj
        # output: updated ogs5-obj

        filename = basename(filepath)

        # validate input
        valid_ogs5(x)

        # look if ogs5-obj$input$additional exists and valid, otherwise create
        if (!("additional" %in% names(x$input))) {
            x$input$additional <- create_ogs5_additional()
        } else {

            valid_ogs5_additional(x$input$additional)

            if (filename %in% names(x$input$additional)) {
                stop(paste0("additional_bloc '", filename,
                            "' does already exist"), call. = FALSE)
            }
        }

        # read file
        bloc_vector = readr::read_file(file = filepath) %>%
                        stringr::str_split("\n")

        # create additional-sublist
        x$input$additional[[paste(filename)]] <-
            bloc_vector %>% structure(class = "ogs5_additional_bloc")

        valid_ogs5_additional_bloc(x$input$additional[[paste(filename)]])

        return(x)
}
