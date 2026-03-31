# Note that if the order of iteration in C++ code changes, this must change accordingly
# dimnames must contain "t" for time and "u" for utility components. Rest are control params
# Normally t must be in first position and u in last position
# Utilnames must contain "profit"
# MARGIN is the dimensions we want to keep
standardized_utils = function(res, utilnames){
    dimnames = c("t", "h", "lmin", "T", "u")  # Must be consistent with C++ code
    dimsizes = dim(res)
    
    names(dimsizes) = dimnames  # Vector of dim sizes named with dimnames - to get size of dim by name
    dims = 1:length(dimsizes)   # index in dim vector at which dimname is present
    names(dims) = dimnames      # dims = [t=1, h=2, ..., u=5] etc
    dims_tavg = dims[dimnames != "t"]  # dim indices without t  [h=2, ..., u=5]
    dims_nou = dims[dimnames != "u"]   # dim indices without u  [t=1, h=2, ...] 
    profit_index = which(utilnames == "profit")

    ## Utils with profit masking
    # averge utils over t
    utils_r = res |>   # res is [t, c, u]
        apply(MARGIN=dims_tavg, FUN=mean)   # Preserve all dimensions except time [c, u]

    profit_mask = utils_r[,,,profit_index] < 0   # profit mask is [c]. This line requires that dim(res) == c("t", "h", "lmin", "T", "u")
    profit_mask = profit_mask |>
        rep(dimsizes[dims["u"]]) |>    # repeat block along outer u dimension
        array(dim=dimsizes[dims_tavg])  # profit mask is now [c, u]

    res_masked = utils_r    # res_masked is [c, u]
    res_masked[profit_mask] = NA  # mask all utils at [c] where profit is negative

    utils_max_avg_masked = res_masked |> # res_mased is [c, u]
        apply(MARGIN=which(names(dims_tavg) == "u"), FUN=max, na.rm=T)  # max over c, utils_max_avg_masked is [u]

    utils_r_rep = utils_max_avg_masked |> 
        rep(each = prod(dimsizes[dims_nou])) |>  # repeat u along inner dimensions (hence repeat each u N times) 
        array(dim=dimsizes)   # [t, c, u]

    utils_std = (res/utils_r_rep)  # [t, c, u]

    utils_std_tavg = utils_std |> 
        apply(MARGIN=dims_tavg, FUN=mean)  # [c, u]

    list(
        utils_std=utils_std,
        utils_std_tavg=utils_std_tavg,
        profit_mask=profit_mask
    )
}
