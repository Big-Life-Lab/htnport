#' @title Estimated glomerular filtration rate (GFR)
#'
#' @description This function calculates the estimated glomerular filtration rate (GFR) according to Finlay's formula,
#'              where serum creatine is in mg/dL. The calculation takes into account the respondent's ethnicity, sex, and age.
#'
#' @param LAB_BCRE [numeric] Blood creatine (µmol/L). It should be a numeric between 14 and 785.
#' @param PGDCGT [integer] Ethnicity (13 categories). It should be an integer between 1 and 13.
#' @param CLC_SEX [integer] Sex (Male = 1, Female = 2). It should be an integer of either 1 or 2.
#' @param CLC_AGE [numeric] Age (years). It should be a numeric between 3 and 79.
#'
#' @return [numeric] The calculated GFR. If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details This function implements the Modification of Diet in Renal Disease (MDRD) equation
#'          to estimate glomerular filtration rate, a key indicator of kidney function.
#'
#'          **Clinical Significance:**
#'          GFR estimates are essential for:
#'          - Chronic kidney disease (CKD) classification
#'          - Medication dosing adjustments
#'          - Cardiovascular risk assessment
#'
#'          **Formula Application:**
#'          Base: GFR = 175 × (creatinine^-1.154) × (age^-0.203)
#'          Adjustments:
#'          - Female: × 0.742
#'          - Black ethnicity: × 1.210
#'
#'          **Unit Conversion:**
#'          Serum creatinine converted from µmol/L to mg/dL (÷ 88.4)
#'
#'          **Missing Data Codes:**
#'          - `LAB_BCRE`: `9996` (Not applicable), `9997-9999` (Missing)
#'          - `PGDCGT`: `96` (Not applicable), `97-99` (Missing)
#'          - `CLC_SEX`: `6` (Not applicable), `7-9` (Missing)
#'          - `CLC_AGE`: `96` (Not applicable), `97-99` (Missing)
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Calculate GFR for a 45-year-old white female with serum creatine of 80 µmol/L.
#' calculate_GFR(LAB_BCRE = 80, PGDCGT = 1, CLC_SEX = 2, CLC_AGE = 45)
#' # Output: 67.27905
#'
#' # Example 2: Respondent has non-response values for all inputs.
#' result <- calculate_GFR(LAB_BCRE = 9998, PGDCGT = 98, CLC_SEX = 8, CLC_AGE = 98)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' calculate_GFR(
#'   LAB_BCRE = c(80, 70, 90), PGDCGT = c(1, 2, 1),
#'   CLC_SEX = c(2, 2, 1), CLC_AGE = c(45, 35, 50)
#' )
#' # Returns: c(67.27905, 99.94114, 70.38001)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(gfr = calculate_GFR(LAB_BCRE, PGDCGT, CLC_SEX, CLC_AGE))
#'
#' @seealso [categorize_GFR_to_CKD()] for CKD classification based on GFR values
#' @export
calculate_GFR <- function(LAB_BCRE, PGDCGT, CLC_SEX, CLC_AGE) {
  # Convert serum creatinine from µmol/L to mg/dL
  serumcreat <- LAB_BCRE / 88.4

  # Calculate GFR using the MDRD equation
  GFR <- dplyr::case_when(
    # Valid skip
    LAB_BCRE == 9996 | CLC_SEX == 6 | PGDCGT == 96 | CLC_AGE == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    LAB_BCRE %in% 9997:9999 | CLC_SEX %in% 7:9 | PGDCGT %in% 97:99 | CLC_AGE %in% 997:999 ~ haven::tagged_na("b"),

    # Handle out of range values
    !LAB_BCRE %in% 14:785 | !CLC_SEX %in% c(1, 2) | !PGDCGT %in% 1:13 | !CLC_AGE %in% 3:79 ~ haven::tagged_na("b"),

    # Apply the MDRD equation with adjustments for sex and ethnicity
    CLC_SEX == 2 & PGDCGT == 2 ~ 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) * (0.742) * (1.210),
    CLC_SEX == 2 & PGDCGT != 2 ~ 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) * (0.742),
    CLC_SEX == 1 & PGDCGT == 2 ~ 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)) * (1.210),
    CLC_SEX == 1 & PGDCGT != 2 ~ 175 * ((serumcreat)^(-1.154)) * ((CLC_AGE)^(-0.203)),

    # Default to missing if no other condition is met
    .default = haven::tagged_na("b")
  )

  return(GFR)
}

#' @title Chronic kidney disease (CKD) derived variable
#'
#' @description This function categorizes individuals' glomerular filtration rate (GFR) into stages of Chronic Kidney Disease (CKD).
#'
#' @param GFR [numeric] A numeric representing the glomerular filtration rate.
#'
#' @return [integer] The CKD stage:
#'   - 1: GFR of 60 or below (indicating CKD)
#'   - 2: GFR above 60 (not indicating CKD)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function applies the Kidney Disease: Improving Global Outcomes (KDIGO) guideline to classify Chronic Kidney Disease (CKD) based on GFR.
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `GFR`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Categorize a GFR of 45
#' categorize_GFR_to_CKD(45)
#' # Output: 1
#'
#' # Example 2: Categorize a GFR of 75
#' categorize_GFR_to_CKD(75)
#' # Output: 2
#'
#' # Example 3: Respondent has a non-response value for GFR.
#' result <- categorize_GFR_to_CKD(haven::tagged_na("b"))
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' categorize_GFR_to_CKD(c(45, 75, 60))
#' # Returns: c(1, 2, 1)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(ckd = categorize_GFR_to_CKD(gfr))
#'
#' @seealso [calculate_GFR()]
#' @references Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group. (2013). KDIGO 2012 clinical practice guideline for the evaluation and management of chronic kidney disease. Kidney international supplements, 3(1), 1-150.
#' @export
categorize_GFR_to_CKD <- function(GFR) {
  # Categorize GFR into CKD stages
  CKD <- dplyr::case_when(
    # Valid skip
    haven::is_tagged_na(GFR, "a") ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    haven::is_tagged_na(GFR, "b") | GFR < 0 ~ haven::tagged_na("b"),

    # Categorize CKD based on GFR
    GFR <= 60 ~ 1,
    GFR > 60 ~ 2,

    # Handle any other cases
    .default = haven::tagged_na("b")
  )
  return(CKD)
}
