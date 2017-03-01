library(magrittr)

#' Population Stability Index
#'
#' @param original The original set of a measurement, should be a factor or numeric
#' @param current  The current set of a measurement, should be a factor or numeric
#' @param cut.points It won't work if original and current are factors, and it cannot be NULL if original and current are numerical. This function use this argument to binning original and current with left-closed right-open interval.
#' @param simplify If TRUE then just return a number, else return a psi object.
#'
#' @return PSI value or a psi object
#'
#' @export
psi <- function(original, current, cut.points = NULL, simplifiy = FALSE) {
    # binning numeric
    label.numeric <- function(x, cuts, na.level = NULL) {
        cuts <- sort(cuts)
        n_cut <- length(cuts)
        level.names <- paste0('<= ', cuts) %>% c(paste0('> ', cuts[n_cut]))

        if (any(is.null(na.level))) {
            na.level <- any(is.na(x))
        }
        if (na.level) level.names <- c('Missing', level.names)
        else {
            if(any(is.na(x))) stop('x has NA value while na.level is FALSE')
        }

        y <- vector('integer', length(x))

        for (i in n_cut:1) {
            y[x <= cuts[i]] <- i
        }
        y[x > cuts[n_cut]] <- n_cut + 1

        if (na.level) {
            y <- level.names[y + 1]
            y[is.na(x)] <- 'Missing'
        } else {
            y <- level.names[y]
        }

        factor(y, level.names)
    }

    res <- list()
    class(res) <- 'psi'

    # try to convert original & current to factor
    if (is.numeric(original) & is.numeric(current)) {
        if (is.null(cut.points)) {
            stop('When original and current is numeric, cut.points should not be NULL.')
        }
        na.level <- any(is.na(c(original, current)))
        res$original.num <- original
        res$current.num <- current
        original <- label.numeric(original, cut.points, na.level)
        current  <- label.numeric(current, cut.points, na.level)
    }

    if (!is.factor(original) | !is.factor(current)) {
        stop('original and current should be numeric or factor simultaneously.')
    }
    if (any(levels(original) != levels(current))) {
        stop('original and current do not share the same levels.')
    }

    res$original <- original
    res$current  <- current

    levels.name <- levels(original)
    org.stat.tbl <- tapply(X = original,
                           INDEX = original,
                           FUN = length,
                           simplify = TRUE) %>%
        sapply(function(x) ifelse(is.na(x), 0, x))
    cur.stat.tbl <- tapply(X = current,
                           INDEX = current,
                           FUN = length,
                           simplify = FALSE)%>%
        sapply(function(x) ifelse(is.na(x), 0, x))

    tbl <- data.frame(Levels = levels.name,
                      OrgCnt = org.stat.tbl,
                      CurCnt = cur.stat.tbl,
                      OrgPct = org.stat.tbl / sum(org.stat.tbl),
                      CurPct = cur.stat.tbl / sum(cur.stat.tbl))
    tbl$Index <- (tbl$CurPct - tbl$OrgCnt) * log(tbl$CurPct / tbl$OrgCnt)

    psi <- sum(tbl$Index)
    res$psi <- psi
    tbl <- rbind(tbl, data.frame(Levels = 'Total',
                                 OrgCnt = sum(org.stat.tbl),
                                 CurCnt = sum(cur.stat.tbl),
                                 OrgPct = 1,
                                 CurPct = 1,
                                 Index = psi))
    rownames(tbl) <- NULL

    tbl$OrgPct <- round(tbl$OrgPct, 4)
    tbl$CurPct <- round(tbl$CurPct, 4)
    tbl$Index  <- round(tbl$Index,  4)

    res$tbl <- tbl

    if (simplifiy) {
        return(res$psi)
    } else {
        return(res)
    }
}

print.psi <- function(X) {
    cat('PSI is', round(X$psi, 4), '\n')
}


summary.psi <- function(X) {
    cat('PSI is', round(X$psi, 4), '\n\n')
    print(X$tbl)
}
