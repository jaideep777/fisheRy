
# weights must be nstakeholders x nutils :
# c(  0.0, 0.3, 0.0, 0.7,	  # industrial
#     0.3, 0.5, 0.1, 0.1,	  # artisanal
#     0.3, 0.2, 0.5, 0.0,	  # employment-maximizing policymakers
#     0.2, 0.2, 0.0, 0.6,	  # profit-maximizing policymakers
#     0.5, 0.1, 0.2, 0.2	  # conservationists
# )
stakeholder_satisfaction = function(utils_std, weights, profit_mask, utilnames, nstake = 5, nu = 4){
    nstake = 5
    nu = 4

    wvec = weights |> array(dim = c(nu,nstake))  # reshape wvec into [u, S]

    dimnames = c("t", "h", "lmin", "T", "u")  # Must be consistent with C++ code
    dimsizes = dim(utils_std)
    
    names(dimsizes) = dimnames  # Vector of dim sizes named with dimnames - to get size of dim by name
    dims = 1:length(dimsizes)   # index in dim vector at which dimname is present
    names(dims) = dimnames      # dims = [t=1, h=2, ..., u=5] etc  (t, c, u)
    dims_tavg = dims[dimnames != "t"]  # dim indices without t  [h=2, ..., u=5] (c, u)
    dims_nou = dims[dimnames != "u"]   # dim indices without u  [t=1, h=2, ...] (t, c)
    profit_index = which(utilnames == "profit")

    dimsizes_st = c(dimsizes[dims_nou], u=nu, S=nstake) # dim sizes including stakeholders dim: [t, c, u, S], but with only 4 util components as specified
    dimnames_st = names(dimsizes_st)   
    dims_st = 1:length(dimsizes_st)   
    names(dims_st) = dimnames_st      # dims_st = [t=1, h=2, ..., u=5, S=6] etc 
    dims_st_tavg = dims_st[names(dims_st) != "t"]  # dim indices witout t but including S = [h=2, ..., u=5, S=6] etc 

    wvec_rep = wvec |>    # wvec is [u, S]
        rep(each = prod(dimsizes[dims_nou])) |>  # repeat [u, S] each along inner dimensions 
        array(dim = dimsizes_st)          # wvec_rep is [t, c, u, S]

    profit_mask_t = profit_mask |>    # profit mask is [c, u]
        rep(each = dimsizes["t"]) |>  # repeat it along inner dimension
        array(dim = dimsizes)         # profit mask is now [t, c, u]

    utils_std[profit_mask] = NA       # mask utils_std 

    # ALERT: below line requires that dim(res) == c("t", "h", "lmin", "T", "u") - must change if C++ code changes
    utils_S = utils_std[,,,,1:4, drop = FALSE] |>  # This assumes that desired utility components are 1st 4 in column vector
        rep(times=nstake) |>          # repeat full utils block along outer S dimension
        array(dim = dimsizes_st)      # utils_S is now [t, c, u, S]

    # Sum over all standardized utilities
    stakeholder_utility_t = (wvec_rep*utils_S) |>   # [t, c, u, S]
        apply(MARGIN=which(names(dimsizes_st) != "u"), sum, na.rm=T) # [t, c, S]

    # Average total utility over time
    stakeholder_utility = stakeholder_utility_t |>
        apply(MARGIN=which(names(dim(stakeholder_utility_t)) != "t"), mean, na.rm=T) # [c, S]

    su_max_c = stakeholder_utility |> 
        apply(MARGIN=which(names(dim(stakeholder_utility)) == "S"), FUN=max, na.rm=T) |>   # max over c / retain S dim [S]
        rep(each = prod(dimsizes_st[c("h","lmin","T")])) |>    # Repeat S along inner c dimensions --> [c, S]
        array(dim = dimsizes_st[c("h","lmin","T", "S")])       # [c, S]
  
    # Calculate normalized stakeholder satisfaction
    stakeholder_satisfaction = (stakeholder_utility/su_max_c)  # [c, S]
    
    return(stakeholder_satisfaction)
}
