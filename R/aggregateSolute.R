#' Aggregate loads by the time periods specified by the user
#'
#' This will aggregate the total loads or mean concentrations per aggregation
#' interval, specified by \code{agg.by}. The time frame specified by
#' \code{agg.by} can be "unit," "day," "month," "water year," "calendar year,"
#' "total," or the name of a column in newdata that can be used to group the
#' data.
#'
#' This also calculates the uncertainty in the sum over a regular time series
#' (loads) with known standard errors (loadsSEs) for each short-term load
#' estimate.
#'
#' The general equation for propagation of error in a sum of potentially
#' autocorrelated values is:
#'
#' sum_t(var(x[t])) + 2*sum_a,b(cov(x_a,a_t+l))
#'
#' where we will assume something about the covariance matrix.
#'
#' However, we will deviate from the above equation to accommodate the lognormal
#' distribution of each flux prediction.
#'
#' @importFrom dplyr %>% group_by_ summarise filter n n_groups
#' @importFrom lubridate tz
#' @importFrom smwrBase waterYear
#' @importFrom unitted u v get_units
#' @importFrom methods is
#' @importFrom stats aggregate qt qnorm setNames
#' @param preds A data.frame containing at least the columns "fit" (predicted
#'   instantaneous fluxes or concentrations) and "date". If agg.by includes
#' @param metadata A metadata object describing the model
#' @param format character. The desired format of the aggregated values. If
#'   "conc", preds is assumed to already be formatted as "conc". If "flux" or
#'   "flux rate", preds is assumed to already be formatted as "flux rate". If
#'   preds has a "units" attribute, that attribute is checked for consistency
#'   with \code{format} and \code{metadata}, but if preds has no "units"
#'   attribute then no such checks can be made.
#' @param agg.by character. The date interval or grouping column by which to
#'   aggregate. If agg.by="unit", values will be returned unaggregated but in
#'   the standard post-aggregation format. If agg.by is one of "day", "month",
#'   "water year", or "calendar year", the preds$date vector will be split into
#'   periods corresponding to those intervals, and the flux or concentration
#'   will be computed for each period. If agg.by="total", \code{preds$date} will
#'   be ignored and the entire vector \code{preds} will be aggregated. If agg.by
#'   contains custom names, those names are expected to be column names in
#'   \code{preds} and will be used to determine aggregation groups. If
#'   agg.by="unit", aggregation will occur for each unique value in
#'   \code{preds$dates}, i.e., no aggregation will occur.
#' @param agg.cols logical. Should the output data.frame include a column or
#'   columns specifying the aggregation group/s for each row? TRUE is
#'   recommended.
#' @param count logical. Should a count of the number of observations per group
#'   be included? For most values of agg.by, when count=TRUE there will be a new
#'   column called count.
#' @param na.rm logical. Should NA values be ignored during aggregation (TRUE),
#'   or should NA be returned for intervals that contain one or more NA
#'   predictions (FALSE)?
#' @param attach.units logical. If true, units will be attached as an attribute
#'   of the second column of the returned data.frame.
#' @param ... Defunct and ignored arguments. Defunct arguments include
#'   'se.preds', 'ci.agg', 'deg.free', 'ci.distrib', 'se.agg', and
#'   'cormat.function'.
#' @return A data.frame with 2+ columns. The first column or set of columns
#'   contains the aggregation period or custom aggregation unit and is named
#'   after the value of \code{agg.by}. The second contains the aggregate flux or
#'   concentration estimates and is named after the value of \code{format}. The
#'   values in the second column will be in the units specified by
#'   \code{metadata}.
#'
#' @examples
#' \dontrun{
#' data(eg_metadata)
#' metadata_example <- updateMetadata(eg_metadata, dates="date")
#' preds_example <- data.frame(fit=abs(rnorm(365, 5, 2)),
#'   date=seq(as.Date("2018-05-15"), as.Date("2019-05-14"), by=as.difftime(1, units="days")))
#' loadflex:::aggregateSolute(preds_example, metadata=metadata_example, format="conc", agg.by="month")
#' loadflex:::aggregateSolute(preds_example, metadata=metadata_example, format="flux rate", agg.by="month")
#' loadflex:::aggregateSolute(preds_example, metadata=metadata_example, format="flux rate", agg.by="month", attach.units=TRUE)
#'
#' # with a custom aggregation group
#' preds_regrouped <- transform(preds_example, simple.season=ordered(
#'   c("winter","spring","summer","fall")[floor(((as.numeric(strftime(date, "%m"))+0)%%12)/3)+1],
#'   c("winter","spring","summer","fall")))
#' loadflex:::aggregateSolute(preds_example, metadata=metadata_example, format="conc",
#'   agg.by="simple.season", custom=preds_regrouped)
#' loadflex:::aggregateSolute(preds_example, metadata=metadata_example, format="flux",
#'   agg.by="simple.season", custom=preds_regrouped)
#' }
aggregateSolute <- function(
  preds, metadata, format=c("conc", "flux rate"),
  agg.by=c("unit", "day", "month", "water year", "calendar year", "total", "[custom]"),
  na.rm=FALSE, attach.units=FALSE, agg.cols=TRUE, count=TRUE,
  ...) {

  # Check for defunct arguments or argument values
  dots <- names(eval(substitute(alist(...))))
  defunct_args <- dots[which(dots %in% c('se.preds','ci.agg','deg.free','ci.distrib','se.agg','cormat.function'))]
  if(length(defunct_args) > 0) {
    warning(sprintf(
      "ignoring these defunct arguments: %s",
      if(length(defunct_args) > 1) 's' else '',
      paste(defunct_args, collapse=', ')
    ))
  }
  if(format == "flux total") {
    warning("format=\"flux total\" is no longer supported. Set agg.by='total' and multiply flux.rate by duration to get total flux")
  }
  if(!is.data.frame(preds)) stop("preds must be a data.frame") # we used to allow a vector, plus dates vector. now we don't.

  # Validate arguments
  format <- match.arg.loadflex(format, c("conc", "flux rate"))
  attach.units <- match.arg.loadflex(attach.units)
  default_agg.by <- c("unit", "day", "month", "water year", "calendar year", "total")
  for(abi in 1:length(agg.by)) {
    agg.by[abi] <- match.arg.loadflex(agg.by[abi], c(default_agg.by, colnames(preds)))
  }

  # Check for required columns in preds. (We've already checked for the presence
  # of any custom agg.by columns above)
  need_col <- c("date", "fit")
  missing_col <- need_col[!need_col %in% colnames(preds)]
  if(length(missing_col) > 0)
    stop(paste0("missing column[s] ", paste0("'", missing_col, "'", collapse=' & '), " in the preds data.frame"))

  # Check that dates contains actual dates
  if(!(is(preds$date, "POSIXt") || is(preds$date, "Date") || is(preds$date, "chron"))) {
    stop("Unexpected format for preds$date - must be POSIXt, Date, or chron")
  }

  # If possible, check the units of preds against the units implied by format and metadata
  pred_units <- get_units(preds$fit)
  if(!is.na(pred_units)) {
    expected_pred_units <- switch(
      format,
      "conc"=metadata@conc.units,
      "flux rate"=metadata@load.rate.units
    )
    if(pred_units != expected_pred_units) {
      stop(paste0("The units of preds should be ", expected_pred_units,
                  ", given the metadata and format==", format))
    }
  }

  # Decide on the aggregation vector (the usual case) or list of vectors
  # (uncommon, but possible for "custom")
  if(any(agg.by %in% default_agg.by)) {
    if(length(agg.by) != 1) {
      stop("Only one standard value of agg.by is allowed; multiple group variables must be specified as all custom colnames")
    }
    aggregate_by <- setNames(
      data.frame(
        switch(
          agg.by,
          "unit"=1:length(preds),
          "day"=strftime(preds$date, "%Y-%m-%d", tz=tz(preds$date)),
          "month"=strftime(preds$date, "%Y-%m", tz=tz(preds$date)),
          "water year"=waterYear(preds$date),
          "calendar year"=strftime(preds$date, "%Y", tz=tz(preds$date)),
          "total"=rep(1,length(preds)),
          stop(""))),
      agg.by)
  }  else {
    aggregate_by <- preds[agg.by]
  }

  # Combine data and aggregation labels
  preds_df <- data.frame(fit=preds$fit, aggregate_by, check.names=FALSE)

  # Remove rows with NAs if requested
  if(isTRUE(na.rm)) preds_df <- preds_df[!is.na(preds_df$fit), ]

  # Group the estimates as requested
  preds_grp <- group_by_at(preds_df, agg.by)

  # Compute the means and counts in each group
  agg_preds <- summarise(
    preds_grp,
    Value = mean(fit),
    count = n()) %>%
    as.data.frame # if we allow tibbles, units get complicated

  # If requested, determine the new units for the conc/fluxrate and count columns.
  # Other columns (e.g., Period, Duration) are either non-unitted or already
  # have units attached.
  if(attach.units) {
    new_units <- unitted::get_units(u(agg_preds)) # recovers any units of custom agg columns
    new_units[names(agg_preds) == "Value"] <- switch(
      format,
      "conc"=metadata@conc.units,
      "flux rate"=metadata@load.rate.units
    )
    new_units[names(agg_preds) == "count"] <- "raw.preds"
    retDF <- u(agg_preds, new_units)
  } else {
    # Shake off any pre-existing units attached to individual columns. Use
    # spaced_names to get around a bug where v(agg_preds) uses the check.names
    # feature of data.frame somewhere inside, causing renames of any columns
    # with spaces, which we want to avoid
    spaced_names <- names(agg_preds)
    retDF <- v(agg_preds)
    names(retDF) <- spaced_names
  }

  # Replace "Value" with "conc" or "flux.rate"
  names(retDF)[match("Value", names(retDF))] <- .reSpace(format, ".")

  # Exclude any un-requested names
  all_names <- names(retDF)
  drop_names <- c(
    if(!agg.cols) agg.by,
    if(!count) "count")
  keep_names <- setdiff(all_names, drop_names)
  retDF <- retDF[ , keep_names] # drops to vector if the only thing in keep_names is the Value column

  # Return
  return(retDF)
}
