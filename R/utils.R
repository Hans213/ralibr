
# Check -------------------------------------------------------------------


is_list_of <- function (object, class) {
  is.list(object) && Reduce(`&&`, Map(is, object, class))
}

is_valid_stub <- function (stub) {
        all(stub %in% c("short_front", "short_back", "long_front", "long_back"))
}


# Assertion Helpers -------------------------------------------------------


assertthat::on_failure(is_valid_bdc) <- function (call, env) {
        paste0(deparse(call$bdc), " contains invalid business day convention.")
}

assertthat::on_failure(is_valid_day_basis) <- function(call, env) {
        paste0(deparse(call$bdc), " contains invalid business day convention.")
}

assertthat::on_failure(is_list_of) <- function(call, env) {
        paste0("All elements of ", deparse(call$object), " are not objects of class ",
                deparse(call$class), ".")
}



# Frequency conversion ----------------------------------------------------

#' Convert a numeric Tenor to a String
#'
#' @param input
#' @param atomic
#'
#' @return
#' @export
#'
#' @examples
convert_frequency <- function(input, atomic = FALSE){
        if (is.numeric(input)){
                if (atomic){
                        if (input == 1) output <- months(1)
                        if (input == 3) output <- months(3)
                        if (input == 6) output <- months(6)
                        if (input == 12) output <- lubridate::years(1)
                } else{
                        if (input == 1) output <- "M"
                        if (input == 3) output <- "Q"
                        if (input == 6) output <- "QA"
                        if (input == 12) output <- "A"
                }
        }
        if (is.character(input)){
                if (atomic){
                        if (toupper(input) == "M") output <- months(1)
                        if (toupper(input) == "Q") output <- months(3)
                        if (toupper(input) == "SA") output <- months(6)
                        if (toupper(input) == "A") output <- lubridate::years(1)
                        if (toupper(input) == "D") output <- lubridate::days(1)
                } else{
                        if (toupper(input) == "M") output <- 1
                        if (toupper(input) == "Q") output <- 3
                        if (toupper(input) == "SA") output <- 6
                        if (toupper(input) == "A") output <- 12
                }
        }
        return(output)
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
        condition <- eval(substitute(condition), .data, envir)
        .data[condition, ] <- .data[condition, ] %>% mutate(...)
        .data
}
