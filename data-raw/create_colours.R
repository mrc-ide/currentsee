
options <- expand.grid(
  cm = 1,
  itn = 0:1,
  irs = 0:1,
  chem = 0:1,
  vaccine = 0:1
)

combos <- apply(options, 1, function(x){
  paste0(names(options)[x == 1], collapse = ", ")
})
combos <- unique(c(combos, stringr::str_replace(combos, "itn", "itn+")))

package_id <- data.frame(
  package = combos,
  package_id = as.character(as.numeric(factor(combos))),
  colour =c(
    "#AEC6CF", "#FFB347", "#77DD77", "#FF6961", "#CBAACB", "#F49AC2",
    "#FFD1DC", "#B5EAD7", "#FFFACD", "#ADD8E6", "#E0BBE4", "#F7CAC9",
    "#BFD8B8", "#FFDAC1", "#E2F0CB", "#B5B9FF", "#FF9AA2", "#FFB7B2",
    "#FFDAC1", "#E2F0CB", "#B5EAD7", "#C7CEEA", "#D5AAFF", "#F3FFE3",
    "#EAD5E6", "#C1E1C1", "#FFCCF9", "#FCF6BD", "#A0CED9", "#FFB5E8",
    "#B9FBC0", "#AFF8DB"
  )[1:24]
)

usethis::use_data(package_id, overwrite = TRUE)
