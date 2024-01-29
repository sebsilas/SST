SST_dict_df <- readxl::read_excel("data-raw/SST.xlsx")

SST_dict <- musicassessr::dict(additional_dict = SST_dict_df)

da <- SST_dict$as.data.frame()

usethis::use_data(SST_dict_df, SST_dict, overwrite = TRUE, internal = TRUE)

