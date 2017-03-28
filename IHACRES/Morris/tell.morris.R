function (x, y = NULL, ...) 
{
    id <- deparse(substitute(x))
    if (!is.null(y)) {
        x$y <- y
    }
    else if (is.null(x$y)) {
        stop("y not found")
    }
    X <- x$X
    y <- x$y
    if (x$scale) {
        Binf <- matrix(x$binf, nrow = nrow(X), ncol = length(x$binf), 
            byrow = TRUE)
        Bsup <- matrix(x$bsup, nrow = nrow(X), ncol = length(x$bsup), 
            byrow = TRUE)
        X <- (X - Binf)/(Bsup - Binf)
    }
    if (x$design$type == "oat") {
        x$ee <- ee.oat(X, y)
    }
    else if (x$design$type == "simplex") {
        x$ee <- ee.simplex(X, y)
    }
    assign(id, x, parent.frame())
}
<environment: namespace:sensitivity