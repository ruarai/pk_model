
pk_merged <- read_csv('data/raw/occurrence/Pk_Merged_MT.csv')

pk_merged_col_names_clean <- c("ID",
                               "Start_month",
                               "Start_year",
                               "End_month",
                               "End_year",
                               "Publication_year",
                               "Sampling_motivation",
                               "Host",
                               "Community",
                               "No_gen_tested",
                               "No_Pk_positive",
                               "Method_gen_tested_calculated",
                               "Diagnostic_1",
                               "Diagnostic_2",
                               "Diagnostic_3",
                               "Presence",
                               "Exclusion_code",
                               "Notes_study",
                               "Site_country",
                               "Site_name",
                               "Notes_site",
                               "Geometry_type",
                               "Latitude",
                               "Longitude",
                               "Admin_level",
                               "Gaul_code",
                               "Polygon_code",
                               "Centroid_latitude",
                               "Centroid_longitude",
                               "DOI",
                               "Source_primary",
                               "Source_secondary",
                               "Map_point_code",
                               "Inclusion")

colnames(pk_merged) <- pk_merged_col_names_clean

pk_merged <- pk_merged[,1:(ncol(pk_merged)-5)]

pk_merged %>%
  write_csv("data/raw/occurrence/Pk_Merged_MT_clean.csv")

