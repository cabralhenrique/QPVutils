#' executes a query to an oracle database
#'
#' @param query_string character. can be either a sql query, the path to an sql file or the name of a table (the latter one detected by the presence of a single word in the string)
#' @param ora_file character. Location to the tnsnames.ora file. If NULL (default), script will look for it in
#'      "C:\\OracleClient\\tnsnames.ora" or in "~/.tnsnames.ora"
#' different environments should be seperated by a blank line. Ex:
#' "ENVIRONMENT = (DESCRIPTION =(ADDRESS = (PROTOCOL = TCP)(HOST = XXX)(PORT = XXX))(CONNECT_DATA =(SERVER = XXX)(SERVICE_NAME = NETWORK_ALIAS)))
#'
#'  ENVIRONMENT2 = (DESCRIPTION =(ADDRESS = (PROTOCOL = TCP)(HOST = XXX)(PORT = XXX))(CONNECT_DATA =(SERVER = XXX)(SERVICE_NAME = NETWORK_ALIAS2)))"
#' @param access_file list or character. Either a list with fields username and password, under an ENVIRONMENT field (ex:
#'       list(ENVIRONMENT = list(username = USERNAME, password = PASSWORD))),
#'either a character string indicating the location of the file. If NULL (default) the script will look for the file .access_db.yaml in the home directory. A minimal yaml file should contain:
#' ENVIRONMENT:
#'      username: 'USERNAME'
#'      password: 'PASSWORD'
#' @param network_alias character. If tnsnames.ora file has more than one connection, which network_alias should it use? Default is EDWPDEV
#' @param sample_table integer. Whether to query a sample of a table. Default is 3
#' @param quiet logical. Be silent? Defauls is FALSE
#' @return the output of the query as a data.table object
#' @details You can also insert data into a table by passing an insert query and a data structure to your input files (see example below)
#' @examples
#'  getQuery("select * from MYTABLE")
#'  getQuery("sql/my_query.sql")
#'  getQuery("insert into TABLE_NAME values(:1, :2)", data = ds)
#' @author Henrique Cabral
#' @export

getQuery <- function(query_string,
    ora_file = NULL,
    access_file = NULL,
    network_alias = NULL,
    sample_table = 3,
    quiet = FALSE,
    ...
) {

    # time process
    time1 <- Sys.time()

    # check if query_string is a file
    file_exists <- file.exists(query_string)

    # check if sample_table
    if (length(strsplit(query_string,' ')[[1]]) == 1 & !file_exists) {
        query_string <- sprintf(
            "select * from %s where rownum < %d",
            query_string,
            sample_table
        )
    } else if (file_exists) { # check if query_string is file and read if so
        query_string <- paste(readLines(query_string),collapse = '\n')
    }

    if (!quiet){
        # print message
        cat(query_string)
        cat('\n')
    }
    print('querying data....')

    # find ora file
    if (is.null(ora_file)) {
        if (file.exists('C:\\OracleClient\\tnsnames.ora')) {
            ora_file <- 'C:\\OracleClient\\tnsnames.ora'
        } else if (file.exists(paste0(Sys.getenv("HOME"),'/.tnsnames.ora'))) {
            ora_file <- paste0(Sys.getenv("HOME"),'/.tnsnames.ora')
        } else {
            stop('Please provide an tnsnames.ora file location')
        }
    }

    # find credentials file
    if (is.null(access_file) & !is.list(access_file)) {
        if (file.exists(paste0(Sys.getenv("HOME"),'/.access_db.yaml'))) {
            access_file <- paste0(Sys.getenv("HOME"),'/.access_db.yaml')
        } else {
            stop('Please provide an .access_db.yaml file location')
        }
    }

    # load file, if it's a string
    if (is.character(access_file)) {
        access_file <- yaml::yaml.load_file(access_file)
    }

    # validate credentials file
    if (!is.list(access_file)) {
        stop('Please provide a valid credentials file object')
    }

    # define network_alias
    if (is.null(network_alias)) { # use first available one
        network_alias <- names(access_file)[1]
    }

    access_file <- access_file[[network_alias]]

    # validade credentials
    if (sum(c('username','password') %in% names(access_file)) != 2) {
        stop('Credentials file must contain fields <username> and <password>')
    }

    # read connection
    dbname <- readLines(ora_file)

    # replace empty items by split character
    dbname[nchar(dbname) == 0] <- '<SPLIT>'

    # now collapse text
    dbname <- strwrap(paste(dbname, collapse = ''),1000)

    # check if there are more than one connection and split, if so
    db_split <- strsplit(dbname, '<SPLIT>')[[1]]
    if (length(db_split) > 1) {
        id_network_alias <- grep(network_alias,db_split)
        if (!length(id_network_alias)) {
            stop('Network alias not found in tnsnames.ora file')
        } else if (length(id_network_alias) > 1) {
            stop('Multiple network alias found in tnsnames.ora file')
        } else {
            dbname <- db_split[id_network_alias]

            # keep only what's in parentesis
            dbname <- substr(
                dbname,
                regexpr('[(]',dbname),
                max(gregexpr('[)]',dbname)[[1]])
            )
        }
    }

    # set driver
    drv <- ROracle::Oracle()

    # make connection
    con <- ROracle::dbConnect(
        drv,
        username = access_file$username,
        password = access_file$password,
        dbname = dbname
    )

        # make query
    df <- ROracle::dbGetQuery(con, query_string, ...)

    # convert to data.table and return
    df <- data.table::as.data.table(df)

    # lower case columns
    data.table::setnames(df, tolower(colnames(df)))

    # disconnect
    ROracle::dbDisconnect(con)


    if (!quiet) {
        # time process
        dt <- difftime(Sys.time(),time1, units = 'secs')
        if (dt > 60) {units(dt) <- 'mins'}
        print(sprintf('done in %0.2f %s',as.numeric(dt), units(dt)))
    }

    return(df)
}
