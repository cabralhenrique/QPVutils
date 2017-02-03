
#' Function to time operation and print start and finish message
#'
#' @param expr an object to be evaluated and timed.
#' @param msg string. A message to be printed
#' @return prints out message and time of process. Returns the result of expr
#' @examples timeIt({sum(rnorm(1000))}, 'Processing...')
#' @author Henrique Cabral
#' @export
timeIt <- function(expr, msg) {
    # print message
    print(msg)

    # register starting time
    st <- Sys.time()

    # evaluate expression
    x <- eval(expr)

    # get time difference
    t_diff <- as.numeric(difftime(Sys.time(), st, units = 'secs'))

    # if more than a minute, get time diff in minutes
    if (t_diff > 60) {
        t_diff <- as.numeric(difftime(Sys.time(), st, units = 'mins'))
        t_unit <- 'minutes'
    } else {
        t_unit <- 'seconds'
    }

    # print done message with time
    print(sprintf('Done in %0.2f %s', t_diff, t_unit))
    return(x)
}
