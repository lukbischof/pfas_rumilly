options(timeout = 600)

urls <- c(
  "https://static.data.gouv.fr/resources/demandes-de-valeurs-foncieres/20251018-234902/valeursfoncieres-2025-s1.txt.zip",
  "https://static.data.gouv.fr/resources/demandes-de-valeurs-foncieres/20251018-234857/valeursfoncieres-2024.txt.zip",
  "https://static.data.gouv.fr/resources/demandes-de-valeurs-foncieres/20251018-234851/valeursfoncieres-2023.txt.zip",
  "https://static.data.gouv.fr/resources/demandes-de-valeurs-foncieres/20251018-234844/valeursfoncieres-2022.txt.zip",
  "https://static.data.gouv.fr/resources/demandes-de-valeurs-foncieres/20251018-234836/valeursfoncieres-2021.txt.zip",
  "https://static.data.gouv.fr/resources/demandes-de-valeurs-foncieres/20251018-234831/valeursfoncieres-2020-s2.txt.zip"
)

out_dir <- file.path(project_dir, "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (u in urls) {
  message("Downloading: ", u)
  
  zip_path <- file.path(out_dir, basename(u))
  if (!file.exists(zip_path)) {
    download.file(u, destfile = zip_path, mode = "wb", quiet = TRUE)
  } else {
    message("  -> already exists, skipping: ", zip_path)
  }
  
  zlist <- unzip(zip_path, list = TRUE)
  txt_in_zip <- zlist$Name[grepl("\\.txt$", zlist$Name, ignore.case = TRUE)]
  if (length(txt_in_zip) == 0) stop("No .txt found inside zip: ", zip_path)
  
  txt_in_zip <- txt_in_zip[1]
  txt_path <- file.path(out_dir, basename(txt_in_zip))
  
  message("Extracting: ", txt_in_zip)
  unzip(zip_path, files = txt_in_zip, exdir = out_dir, overwrite = TRUE)
  
  message("  -> extracted to: ", file.path(out_dir, txt_in_zip))
}

rm(zlist)
