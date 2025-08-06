#' @title Estimated glomerular filtration rate (GFR)
#'
#' @description This function calculates the estimated glomerular filtration rate (GFR) according to Finlay's formula,
#'              where serum creatine is in mg/dL. The calculation takes into account the respondent's ethnicity, sex, and age.
#'
#' @param LAB_BCRE Blood creatine (µmol/L). It should be a numeric value.
#' @param PGDCGT Ethnicity (13 categories). It should be an integer value.
#' @param CLC_SEX Sex (Male = 1, Female = 2). It should be an integer value.
#' @param CLC_AGE Age (years). It should be a numeric value.
#'
#' @return The calculated GFR as a numeric value. If any of the input parameters (LAB_BCRE, PGDCGT, CLC_SEX, CLC_AGE)
#'         are non-response values (LAB_BCRE >= 996, PGDCGT >= 96, CLC_SEX >= 6, CLC_AGE >= 996), the GFR will be NA(b)
#'         (Not Available).
#'
#' @details The function uses the serum creatine level (LAB_BCRE) in µmol/L to calculate the estimated GFR. First, it
#'          checks if any of the input parameters are non-response values. If any non-response values are found, the GFR
#'          will be set to NA, and the function will return immediately. Otherwise, it proceeds with the calculation by
#'          converting the serum creatine to mg/dL (serumcreat = LAB_BCRE / 88.4). Based on the respondent's ethnicity
#'          (PGDCGT), sex (CLC_SEX), and age (CLC_AGE), the appropriate formula is applied to calculate the GFR. The
#'          formula used for each combination of ethnicity and sex is as follows:
#'
#'          - Female and Black (PGDCGT == 2, CLC_SEX == 2): GFR = 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) *
#'                                                         (0.742) * (1.210)
#'          - Female and not Black (PGDCGT != 2, CLC_SEX == 2): GFR = 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) *
#'                                                             (0.742)
#'          - Male and Black (PGDCGT == 2, CLC_SEX == 1): GFR = 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) *
#'                                                       (1.210)
#'          - Male and not Black (PGDCGT != 2, CLC_SEX == 1): GFR = 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203))
#'
#' @examples
#'
#' # Example 1: Calculate GFR for a 45-year-old white female with serum creatine of 80 µmol/L.
#' calculate_GFR(LAB_BCRE = 80, PGDCGT = 1, CLC_SEX = 2, CLC_AGE = 45)
#' # Output: GFR = 67.27905
#'
#' # Example 2: Calculate GFR for a 35-year-old black female with serum creatine of 70 µmol/L.
#' calculate_GFR(LAB_BCRE = 70, PGDCGT = 2, CLC_SEX = 2, CLC_AGE = 35)
#' # Output: GFR = 99.94114
#'
#' @export
calculate_GFR <- function(LAB_BCRE, PGDCGT, CLC_SEX, CLC_AGE) {
  GFR <- 0
  serumcreat <- 0

  if (any(!LAB_BCRE %in% 0:9995) || (any(!CLC_SEX %in% c(1, 2)) || any(!PGDCGT %in% 1:13)) || any(!CLC_AGE %in% 0:995)) {
    GFR <- haven::tagged_na("b") # GFR is NA if any non-responses found
  } else {
    serumcreat <- LAB_BCRE / 88.4 # Proceeds without non-responses

    if (!is.na(CLC_SEX) && !is.na(PGDCGT) && serumcreat != 0) {
      if (CLC_SEX == 2 && PGDCGT == 2) {
        GFR <- 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) * (0.742) * (1.210) # female and black
      } else if (CLC_SEX == 2 && PGDCGT != 2) {
        GFR <- 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) * (0.742) # female and not black
      } else if (CLC_SEX == 1 && PGDCGT == 2) {
        GFR <- 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) * (1.210) # male and black
      } else if (CLC_SEX == 1 && PGDCGT != 2) {
        GFR <- 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) # male and not black
      }
    } else {
      GFR <- haven::tagged_na("b") # Handle case where CLC_SEX or PGDCGT is NA or serumcreat is 0
    }
  }

  return(GFR)
}

#' @title Chronic kidney disease (CKD) derived variable
#'
#' @description This function categorizes individuals' glomerular filtration rate (GFR) into stages of Chronic Kidney Disease (CKD).
#'
#' @param GFR Numeric value representing the glomerular filtration rate.
#'
#' @return A categorical value indicating the CKD stage:
#'   - 1: GFR of 60 or below (indicating CKD)
#'   - 2: GFR above 60 (not indicating CKD)
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Categorize a GFR of 45
#' categorize_GFR_to_CKD(45)
#' # Output: 1
#'
#' # Example 2: Categorize a GFR of 75
#' categorize_GFR_to_CKD(75)
#' # Output: 2
#'
#' @export
categorize_GFR_to_CKD <- function(GFR) {
  CKD <- 0

  if (is.na(GFR) || GFR < 0) {
    CKD <- haven::tagged_na("b")
  } else {
    if (GFR <= 60) {
      CKD <- 1
    } else {
      CKD <- 2
    }
  }
  return(CKD)
}
