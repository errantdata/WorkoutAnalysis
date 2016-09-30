mins <- function(x){
    # input: vector of strings in "hh:mm:ss" format representing duration,
    # (not time of day)
    # output: vector with duration in minutes, numerical.
    # will return NA if x = ""

    # set up numeric vector to store results
    # (if store them in x they are converted back to character!)
    z <- numeric()

    for (i in 1:length(x)) {
        # Split the string at each ":"
        y <- strsplit(x, ":")
        # # [[1]]
        # # [1] "01" "02" "03"

        # assign the pieces of splitstring to appropriate variables and
        # convert to numeric
        hours <- as.numeric(y[[i]][1])
        minutes <- as.numeric(y[[i]][2])
        seconds <- as.numeric(y[[i]][3])

        #calculate the time in minutes
        z[i] <- ((hours*60) + (minutes) + (seconds/60))
    next
    }
    z
}