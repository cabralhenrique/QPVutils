
#' Function to remove non-UTF8 characters
#'
#' @param x character vector.
#' @return character vector
#' @examples rmNonUTF8('hell\023o')
#' @author Henrique Cabral
#' @export
rmNonUTF8 <- function(x) {

    # set encoding to UTF-8
    Encoding(x) <- 'UTF-8'

    # convert character between encodings and set unknowns to ''
    x <-  iconv(x, "UTF-8", "UTF-8",sub='')

    # check for further unknown characters and remove them
    bad_enc <- c(
        '\001','\002','\003','\004','\005','\006','\007',
        '\016','\017','\018','\019','\020','\021','\022','\023','\024','\025',
        '\026','\027','\028','\029','\030','\031','\032','\033','\034','\035',
        '\036','\037','\038','\039','\048','\049','\058','\059','\068','\069',
        "[\\]",'/bx'
    )
    x <- gsub(paste(bad_enc, collapse = '|'),"",x)
    # for (be in bad_enc) {
    #     x <- gsub(be,"",x)
    # }

    return(x)

}


#' Function to keep only alphanum characters
#'
#' @param x string.
#' @param replace string. replace non alpha-num characters with it. default = ''
#' @return character vector
#' @examples keepAlphaNum('hello.baby')
#' @author Henrique Cabral
#' @export
keepAlphaNum <- function(x, replace = '') {

   # keep only alphanumeric charactes
    x <- gsub('[^[:alnum:]]','',x)

    # remove unwanted characters (accents, cedila)
    x <- iconv(x, to='ASCII//TRANSLIT')

    return(x)

}

#' Function to convert number to words
#'
#' @param x numeric vector or character vector with digits.
#' @return character vector
#' @examples num2word('245885')
#' @author Henrique Cabral
#' @export
num2word <- function(x) {

    # convert to character if needed
    if (!is.character(x)) {
        x <- as.character(x)
    }

    # split into individual numbers
    x <- strsplit(x,'')[[1]]

    # for each number, get the corresponding letter of the alphabet
    # (add one for 0)
    paste(sapply(x, function(x) letters[as.numeric(x) + 1]), collapse = '')
}

#' Function to make link in pdf file
#'
#' @param x Name to be displayed in link
#' @param x_link if missing, x is used
#' @param cap numeric. truncate x to CAP characters
#' @param target logical. is link FALSE. or link target (TRUE, default)
#' @param prefix character. add prefix to make link unique
#' @param type characte. markdown/excel (XLConnect format)
#' @param keep_alphanum logical. Remove non-alphanumeric characters(default: TRUE)
#' @return character vector
#' @examples myLinker('hellothere', x_link = 'hello_link', cap = 5)
#' @author Henrique Cabral
#' @export
myLinker <- function(
    x,
    x_link = NULL,
    cap = NULL,
    target = F,
    prefix = NULL,
    type = 'markdown',
    keep_alphanum = TRUE
    ) {

    # check if a link is provided and keep only alpha numeric characters
    sub_pattern <- ifelse(keep_alphanum, "[^[:alnum:]]", "")
    if (!is.null(x_link)) {
        x_alpha_num <- gsub(sub_pattern, "", x_link)
    } else {
        x_alpha_num <- gsub(sub_pattern, "", x)
    }

    # add prefix
    if (!is.null(prefix)){
        x_alpha_num <- paste0(prefix,x_alpha_num)
    }

    if (is.null(cap)) { # define cap length of name
        cap <- nchar(as.character(x))
    }

    if (! target){
        if (type == 'markdown') {
            sprintf(
                '[%s](#%s)',
                substr(x, 1, cap),
                x_alpha_num
            )
        } else if (type == 'excel') {
            sprintf(
                'HYPERLINK("#%s!A1","%s")',
                x_alpha_num,
                substr(x, 1, cap)
            )
        }

    } else {
        sprintf(
            '%s {#%s}',
            substr(x, 1, cap),
            x_alpha_num
        )
    }
}

#' Function to format text for pretty display
#'
#' @param x string or vector of strings.
#' @param cap integer. max number of characters. Default = 20
#' @param capitalize character. one of 'all','none','first' (default)
#' @param sep_words character. separate words. Default = '_' (use NULL not to separate)
#' @return string or vector of strings.
#' @examples formatText('hello', capitalize = 'none')
#' @author Henrique Cabral
#' @export
formatText <- function(x, cap = NULL, capitalize = 'first', sep_words = '_') {

    # convert to string if it's not
    if (class(x) != 'character') {
        x <- as.character(x)
    }

    # loop through each string
    for (i in seq(length(x))) {
        # cap string
        if (!is.null(cap)) {
            x0 <- substr(x[i],1,cap-3)
            # add ... if text was capped
            if (x0 != x[i] | is.na(x0 != x[i])) {
                x[i] <- paste0(x0, '...')
            } else {x[i] <- x0}
        }

        # separate words
        if (!is.null(sep_words)) {x[i] <- gsub(sep_words,' ',x[i])}

        # capitalize
        if (capitalize == 'all') {x[i] <- toupper(x[i])}
        if (capitalize == 'none') {x[i] <- tolower(x[i])}
        if (capitalize == 'first') {
            x[i] <- paste0(
                toupper(substr(x[i],1,1)),
                tolower(substr(x[i], 2, nchar(x[i])))
                )
        }
    }

    return(x)
}

#' Function to format column names
#'
#' @param x string. vector of names
#' @param cap_all logical. Capitalize all words or only first one? Default is TRUE
#' @return x with replaced '_' and capitalized first letter(s)
#' @examples formatColumnNames('my_column_name')
#' @author Henrique Cabral
#' @export
formatColumnNames <- function(x, cap_all = TRUE) {

    # loop through each element
    for (i in seq(length(x))) {
        # split by '_'
        x0 <- strsplit(x[i], '_')[[1]]
        if (cap_all) { # capitalize all first letters
            for (j in seq(length(x0))) {
                x0[j] <- paste0(
                    toupper(substr(x0[j],1,1)),
                    substr(x0[j],2, nchar(x0[j]))
                )
            }
        } else { # capitalize first word
            x0[1] <- paste0(
                toupper(substr(x0[1],1,1)),
                substr(x0[1],2, nchar(x0[1]))
            )
        }
        # collapse together
        x[i] <- paste(x0, collapse = ' ')
    }

    return(x)

}
