#' Trailing
#'
#' This function calculates the trailing trend in a vector.
#' @param days Number of days trend should cover. Defaults to 28.
#' @keywords trailing
#' @export
#' @examples
#' trailing()
 
trailing <- function(v, days=7) {
	res <- c()
	for (i in days:length(v)) {
		res[i] <- sum(v[(i-days + 1):i])
	}
	return(res)
}