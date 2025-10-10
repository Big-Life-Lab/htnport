BMI_fun <- function(height, weight) {
    if(is.na(height) | is.na(weight)) {
        return('NA::b')
    }

    height <- height / 100
    return(weight/(height*height))
}