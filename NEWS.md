# 2.0.0

* aggregateSolute is deprecated. Use agg.by option in predictSolute instead.

* predictSolute labels the predictions as "conc" or "flux" rather than "fit".

* "flux" predictions are always flux rates, not total fluxes, and are reported
in the units specified for `load.rate.units` in the load model metadata.

* Uncertainty estimates for aggregated concentrations or fluxes are only
provided for loadReg2 models; aggregate values for other models are unavailable.

# 1.1.0 - 1.1.20 or so

* New function: `plotEGRET`. Generates plots of loadflex inputs and outputs 
using code already written and refined in the EGRET load estimation package.

* New function: `summarizeModel`. Available for all loadModel classes.

* Expanded list of fields available in `metadata` class, for improved plotting
and summarization.

* Lightly altered the arguments and features of `getUnits` and `getInfo`.


# 1.0.1

* This version is consistent with the description in Appling, A. P., M. C. Leon,
and W. H. McDowell. 2015. Reducing bias and quantifying uncertainty in watershed
flux estimates: the R package loadflex. Ecosphere 6(12):269. 
https://doi.org/10.1890/ES14-00517.1.
