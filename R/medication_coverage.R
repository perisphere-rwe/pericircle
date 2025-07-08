
#' todo - document
#'
#' @param dt todo
#' @param max_carry todo
#' @param id_col todo
#' @param med_col todo
#' @param fill_date_col todo
#' @param supply_col todo
#'
#' @returns a `data.table`
#' @export
#'
dt_add_coverage_dates <- function(dt,
                                  max_carry = Inf,
                                  id_col = "id",
                                  med_col = "medication",
                                  fill_date_col = "date_fill",
                                  supply_col = "supply") {

  # Ensure input is a data.table
  setDT(dt)

  # Rename for internal processing
  setnames(dt,
           old = c(id_col, med_col, fill_date_col, supply_col),
           new = c("id", "med", "date_fill", "supply"))

  # Coerce to expected types
  dt[, date_fill := as.IDate(date_fill)]
  dt[, supply := as.integer(supply)]

  # Order by id, med, date_fill
  setorder(dt, id, med, date_fill)

  # Initialize variables
  dt[, `:=`(start = date_fill, stop = date_fill + supply - 1L)]

  # Identify leftover supply
  dt[, next_fill := shift(date_fill, type = 'lead'),
     by = .(id, med)]

  dt[, next_supply := stop - next_fill + 1L]

  dt[, leftover := count_leftovers(next_supply),
     by = .(id, med)]

  if(!is.infinite(max_carry)){
    dt[, leftover := pmin(leftover, max_carry)]
  }

  # Use leftover supply to adjust start and stop dates
  dt[, .leftover := shift(leftover, type = 'lag', fill = 0),
     by = .(id, med)]

  dt[, `:=`(start = start + .leftover,
            stop  = stop  + .leftover)]

  # only keep the original leftover column (most intuitive)
  dt[, `:=`(next_fill = NULL,
            next_supply = NULL,
            .leftover = NULL)]

  # Restore original column names if needed
  setnames(dt, c("id", "med", "date_fill", "supply", "leftover"),
           c(id_col, med_col, fill_date_col, supply_col, "supply_carryover"))

  dt[] # silent print after modifying in place

  dt

}
