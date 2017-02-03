#' make n-way ANOVA and Tukey Post-hocs
#'
#' @param frml formula. Formula to be passed to aov
#' @param ds data.frame. Data for aov
#' @param which_level character or numeric vector. which levels to compare in
#'      post-hoc. default = 1
#' @param quiet logical. should the results of the anova be printed? default is FALSE
#' @param just_aov logical. Should the post-hoc test be done? Default is TRUE
#' @param ... further arguments to be passed to aov
#' @return ANOVA p-value and Tukey's Post-hoc p-val per comparison
#' @examples doAnova(formula = x ~ grp * gr2, which = 'grp:grp2')
#' @author Henrique Cabral
#' @export

doANOVA <- function(frml, ds, which_levels = 1, quiet = F, post_hoc = TRUE, ...) {
    # run ANOVA
    fit <- aov(frml, data = ds, ...)

    # print ANOVA results

    fit_sum <- summary(fit)

    if (!quiet) {
        cat('ANOVA:\n')
        print(fit_sum)
    }

    if (post_hoc) {
        # run Tukey's post-hoc
        ph <- TukeyHSD(fit, which_levels)

        # loop through each level and compile group comparisons
        tk <- data.table(
            level = character(),
            grp1 = character(), grp2 = character(),
            pval = numeric()
        )
        for (i in names(ph)) {
            # get this comparison
            comp <- strsplit(rownames(ph[[i]]), '-')

            tk <- rbind(
                tk,
                data.table(
                    level = i,
                    grp1 = sapply(comp,'[[', 1),
                    grp2 = sapply(comp,'[[', 2),
                    pval = ph[[i]][,'p adj'])
            )
        }

        return(list(aov_sum = fit_sum, tk = tk))
    } else {
        return(fit_sum)
    }

}
