kinlabels = c(
  `1_1` = "PO",
  `0_1` = "PO",
  `10_1` = "HS",
  `10_2` = "FS",
  `11_1` = "GG",
  `00_1` = "GG",
  `111_1` = "GGG",
  `000_1` = "GGG",
  `110_1` = "HUN",
  `100_1` = "HUN",
  `110_2` = "UN",
  `100_2` = "UN",
  `1100_1` = "HC",
  `1100_2` = "C",
  `1110_1` = "HGUN",
  `1000_1` = "HGUN",
  `1110_2` = "GUN",
  `1000_2` = "GUN"
)

usethis::use_data(kinlabels, internal = TRUE, overwrite = TRUE, compress = "gzip")
