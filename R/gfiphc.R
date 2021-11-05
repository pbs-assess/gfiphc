#' gfiphc package
#'
#' gfiphc package
#'
#' @docType package
#' @name gfiphc
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom boot boot.ci boot
#' @importFrom stats t.test
#' @importFrom utils read.csv
#' @importFrom graphics legend lines par points
NULL

# Think this is for when things are used within dplyr functions. Most of these
# are from original gfplot and just got carried over; could remove them.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "SURVEY_SERIES_TYPE_CODE", "trip_start_date", "SPECIES_CODE",
    "SPECIES_COMMON_NAME", "SPECIES_DESC", "SPECIES_SCIENCE_NAME",
    "SURVEY_SERIES_DESC", "SURVEY_SERIES_ID", "SURVEY_SERIES_TYPE_CODE",
    "discarded_kg", "discarded_pcs", "gear", "landed_kg",
    "landed_pcs", "species_common_name", "species_desc", "species_science_name",
    "trip_start_date", "year",

    # weighting:
    "month", "freq", "trip_id", "sample_id", "survey_id", "value", "prop",
    "annual_prop", "quarter", "sum_freq",

    "area_km2",
    "catch_weight",
    "density_kgpm2",
    "fe_end_date",
    "fishing_event_id",
    "grouping1",
    "grouping_code",
    "landed_kg_quarter",
    "landed_kg_year",
    "samp_catch_weight_quarter",
    "samp_trip_catch_weight",
    "sum_weighted_freq1",
    "weight",
    "weighted_freq1",
    "weighted_freq1_scaled",
    "weighted_freq2",
    "weighted_freq2_scaled",
    "weighting1",
    "weighting1_total",
    "weighting2",
    "weighting2_total",

    # plotting:

    "age",
    "maturity_code",
    "n_spp",
    "sex",
    "survey",
    "variable",
    "survey_series_desc",
    "survey_abbrev",

    # lengths:
    "bin_size", "counts", "proportion", "total",

    # aging precision:
    "age_reading_id", "age_reading_type_code", "ageing_method_desc",
    "ageing_param", "employee_id", "female", "glmm", "has_precision", "mature",
    "mature_at", "maturity_convention_desc", "maturity_convention_description",
    "maturity_convention_maxvalue", "n_employee", "p", "species_code",
    "specimen_id", "specimen_sex_code",

    # maturity:
    "n_scaled",

    # cpue:
    "best_date", "best_depth", "fe_start_date", "hours_fished", "latitude",
    "locality_code", "longitude", "n_trips_per_year", "n_years", "pos_catch",
    "scrambled_vessel", "species_category_code", "spp_catch", "spp_in_fe",
    "spp_in_row", "sum_catch", "total_positive_tows", "trips_over_thresh",
    "trips_over_treshold_this_year", "vessel_name",
    "est", "est_log", "lwr", "se_log", "upr", "est_link", "model", "se_link",
    "n_date", "pars", "par_name", "par_group", "par_name_short", "se",
    "vessel", "pred", "term",

    "PID", "SID", "nepacLLhigh", "isobath",

    # surveys:
    "Var1", "Var2", "X", "Y", "akima_depth", "depth", "depth_m",
    "depth_mean", "depth_scaled", "depth_sd", "present", "species", "start_lat",
    "start_lon", "x", "y", "z", "bctopo",

    # survey ts:
    "num_pos_sets", "biomass", "lowerci", "mean_cv", "num_pos_sets",
    "num_sets", "re", "surv_order", "survey_name", "upperci",

    "maturity_convention_code",

    "total_month", "month_jitter", "maturity_name_short", "survey_series_id",
    ".n", "maturity", "maturity_name",

    "ageing_method", "length_bin", "weighted_prop", "year_jitter",

    "vessel_effect", "year_effect",

    "sample_source_code", "keeper", "area",

    "totcatch_kg", "fyear", "SURVEY_ABBREV", "vessel_registration_number",
    "sampling_desc", "mean_num_pos_sets", "cv", "sets",
    "true_b", "true", "mean_num_sets",

    # IPHC survey calculations
    "C_it", "C_it20", "E_it", "E_it20", "H_it", "N_it", "N_it20", "bait",
    "block", "chumCountPerSkate", "chumCountPerSkate20", "chumObsHooksPerSkate",
    "chumObsHooksPerSkate20", "countPerSkate", "countPerSkate20",
    "deplHooksPerSkate", "effSkateIPHC", "firstHook", "hook", "hook20",
    "hooksChumRatio", "hooksChumRatio20", "iphcUsabilityCode", "lastHook",
    "lastHookTemp", "lat", "long", "numOnHook", "numOnHook20", "obsHooksPerSkate",
    "obsHooksPerSkate20", "usable", "setID", "skateID",
    "I_t20BootHigh", "I_t20BootLow", "I_t20BootMean", "I_t20SampleMean",
    "I_tBootCV", "I_tBootHigh", "I_tBootLow", "I_tBootMean", "I_tSampleMean",
    "num_pos", "num_pos20", "num_pos_sets",
    "everything", "prop_empty_sets", "wcvi",
    "catchCount", "effSkate",
    "lon", "spNameIPHC", "specCount", "station",
    "countData1995", "countData2013", "data1996to2002",
    "setData1995", "setData2013", "spNameIPHC", "Sets",
    "standard", "nepacLL", "nepacLLhigh", "bcBathymetry", "setDataExpansion",
    "setDataExpansion",
    "C_it20_sum", "C_it_sum", "EID", "HG_herring_pred_area",
    "N_it20_sum N_it_sum",
    "Year", "axis", "countData2020", "in_area", "tail", "text", "title",
    "write.csv", "setData2020", "countData2021", "setData2021",


    # others
    "parent_rsty_id", "parent_taxonomic_unit", "row_version", "rsty_id",
    "species_grouping", "taxonomic_rank",

    # other
    "usability_code",

    ".time_diff",
    ".year_start_date",
    "action_start_date",
    "common_df",
    "count_surveys_since_2008",

    # historical CPUE tidy
    "best_depth_m", "locality", "locality_description", "mean_depth",

    "species_ageing_group"
  ))
}
