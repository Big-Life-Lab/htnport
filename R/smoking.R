#' @title Pack Years Calculation for Smoking History
#'
#' @description This function calculates an individual's smoking pack-years based on various CHMS smoking variables. Pack years is a measure used by researchers to quantify lifetime exposure to cigarette use.
#'
#' @param SMKDSTY An integer representing the smoking status of the respondent:
#'   - 1: Daily smoker
#'   - 2: Occasional smoker (former daily)
#'   - 3: Occasional smoker (never daily)
#'   - 4: Former daily smoker (non-smoker now)
#'   - 5: Former occasional smoker (non-smoker now) who smoked at least 100 cigarettes in their lifetime
#'   - 6: Non-smoker (never smoked more than 100 cigarettes)
#' @param CLC_AGE A numeric value representing the respondent's age.
#' @param SMKDSTP A numeric value representing the number of years since the respondent stopped smoking completely.
#' @param SMK_52 A numeric value representing the respondent's age when they first started smoking daily.
#' @param SMK_31 An numeric representing the number of cigarettes smoked per day for daily smokers.
#' @param SMK_41 A numeric value representing the number of cigarettes smoked per day for occasional smokers.
#' @param SMK_53 A numeric value representing the number of cigarettes smoked per day for former daily smokers.
#' @param SMK_23 A numeric value representing the number of days per month the respondent smoked at least 1 cigarette (for former daily or occasional smokers).
#' @param SMK_21 A numeric value representing the respondent's age when they first started smoking occasionally.
#' @param SMK_11 An integer representing whether the respondent has smoked at least 100 cigarettes in their lifetime:
#'   - 1: Yes
#'   - 2: No
#'
#' @return A numeric value representing the pack years for the respondent's smoking history.
#'   - If the input parameters are invalid or missing, the function returns NA.
#'   - For different smoking statuses, the function calculates pack years as follows:
#'       - Daily smoker: \code{ (CLC_AGE - SMK_52) * (SMK_31 / 20) } (minimum value 0.0137)
#'       - Occasional smoker (former daily): \code{ ((CLC_AGE - SMK_52 - SMKDSTP) * (SMK_53 / 20)) + (pmax((SMK_41 * (SMK_23 * 12) / 30), 1) * SMKDSTP) }
#'       - Occasional smoker (never daily): \code{ (pmax((SMK_41 * (SMK_23 * 12) / 30), 1) / 20) * (CLC_AGE - SMK_21) }
#'       - Former daily smoker (non-smoker now): \code{ pmax(((CLC_AGE - SMK_52 - SMKDSTP) * (SMK_53 / 20)), 0.0137) }
#'       - Former occasional smoker (non-smoker now) who smoked at least 100 cigarettes lifetime: 0.0137
#'       - Former occasional smoker (non-smoker now) who have not smoked at least 100 cigarettes lifetime: 0.007
#'       - Non-smoker: 0
#'
#' @examples
#'
#' # Example 1: Age = 40, daily smoker, started smoking at 20, and smokes 30 cigs/day (1.5 packs/day).
#' pack_years_fun(SMKDSTY = 1, CLC_AGE = 40, SMK_52 = 20, SMK_31 = 30)
#' # Output: 30 (pack years)
#'
#' # Example 2: A former occasional smoker who smoked at least 100 cigarettes in their lifetime.
#' pack_years_fun(SMKDSTY = 5, CLC_AGE = 50, SMKDSTP = 40, SMK_52 = 18, SMK_31 = NA, 
#' SMK_41 = 15, SMK_53 = NA, SMK_23 = 3, SMK_21 = 25, SMK_11 = 1)
#' # Output: 0.0137 (pack years)
#'
#' @export
#'
#' @seealso https://big-life-lab.github.io/cchsflow/reference/pack_years_fun.html
pack_years_fun <- function(SMKDSTY, CLC_AGE, SMKDSTP, SMK_52, SMK_31, SMK_41, SMK_53, SMK_23, SMK_21, SMK_11) {
  # Age verification
  if (is.na(CLC_AGE)) {
    return(haven::tagged_na("b"))
  } else if (CLC_AGE < 0) {
    return(haven::tagged_na("b"))
  }
  
  # PackYears for Daily Smoker
  pack_years <- 
    ifelse(
      SMKDSTY == 1, pmax(((CLC_AGE - SMK_52) *
                            (SMK_31 / 20)), 0.0137),
      # PackYears for Occasional Smoker (former daily)
      ifelse(
        SMKDSTY == 2, pmax(((CLC_AGE - SMK_52 -
                               SMKDSTP) * (SMK_53 / 20)), 0.0137) +
          (pmax((SMK_41 * SMK_23 * (SMKDSTP / 12) / 30), 1) * SMKDSTP),
        # PackYears for Occasional Smoker (never daily)
        ifelse(
          SMKDSTY == 3, (pmax((SMK_41 * SMK_23 * ((CLC_AGE - SMK_21) / 12) / 30), 1) / 20) *
            (CLC_AGE - SMK_21),
          # PackYears for former daily smoker (non-smoker now)
          ifelse(
            SMKDSTY == 4, pmax(((CLC_AGE - SMK_52 -
                                   SMKDSTP) *
                                  (SMK_53 / 20)), 0.0137),
            # PackYears for former occasional smoker (non-smoker now) who
            # smoked at least 100 cigarettes lifetime
            ifelse(
              SMKDSTY == 5 & SMK_11 == 1, 0.0137,
              # PackYears for former occasional smoker (non-smoker now) who 
              # have not smoked at least 100 cigarettes lifetime
              ifelse(
                SMKDSTY == 5 & SMK_11 == 2, 0.007,
                # Non-smoker
                ifelse(SMKDSTY == 6, 0,
                       # Account for NA(a)
                       ifelse(SMKDSTY == "NA(a)", haven::tagged_na("a"),
                              haven::tagged_na("b"))
                )
              )
            )
          )
        )
      )
    )
  return(pack_years)
}