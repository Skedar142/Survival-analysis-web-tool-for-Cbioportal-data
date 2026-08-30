#!/usr/bin/env Rscript
# ============================================================================
#  KM Survival Analyzer  -  cBioPortal edition
#  Downloads mRNA / MUT / CNA + clinical data straight from the cBioPortal
#  public REST API (no Bioconductor packages required) and runs a
#  Kaplan-Meier + log-rank + Cox analysis.
#
#  CNA note: GISTIC discrete calls are -2/-1/0/1/2.  The default --cna_mode
#  is now "all", which keeps every category as its own curve:
#     -2 Deep deletion | -1 Shallow deletion | 0 Diploid | 1 Gain | 2 Amplification
#  Use --cna_mode three|amp|del|altered|strict for the collapsed views.
# ============================================================================

options(timeout = 600, warn = 1)
Sys.setenv(TZ = "UTC")

suppressPackageStartupMessages({
  library(optparse)
  library(survival)
})

# --- lightweight deps, installed from CRAN only if genuinely missing --------
ensure_pkg <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("[setup] installing missing package: ", p)
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  requireNamespace(p, quietly = TRUE)
}
has_httr     <- ensure_pkg("httr")
has_jsonlite <- ensure_pkg("jsonlite")
has_survminer <- requireNamespace("survminer", quietly = TRUE)

if (has_httr) {
  # force IPv4 - DNS64/IPv6 stalls are a common cause of "download never finishes"
  httr::set_config(httr::config(ipresolve = 1L))
}

# ============================================================================
# 1. COMMAND LINE
# ============================================================================
# --download_cbio is a *flag*, but web front-ends routinely send it as
# `--download_cbio TRUE` or `--download_cbio=true`.  With plain optparse that
# either errors out or silently leaves the flag FALSE - which is exactly the
# "it never downloads, it just runs the analysis" symptom.  So we normalise
# the argument vector before optparse ever sees it.
raw_args <- commandArgs(trailingOnly = TRUE)
is_true  <- function(x) toupper(trimws(x)) %in% c("TRUE","T","1","YES","Y","ON")
is_false <- function(x) toupper(trimws(x)) %in% c("FALSE","F","0","NO","N","OFF","")

clean_args <- character(0)
dl_flag    <- NA
i <- 1L
while (i <= length(raw_args)) {
  a <- raw_args[i]
  if (grepl("^(--download_cbio|--download-cbio|--download|-d)=", a)) {
    dl_flag <- !is_false(sub("^[^=]*=", "", a))
  } else if (a %in% c("--download_cbio", "--download-cbio", "--download", "-d")) {
    nxt <- if (i < length(raw_args)) raw_args[i + 1L] else NA_character_
    if (!is.na(nxt) && (is_true(nxt) || is_false(nxt))) {
      dl_flag <- is_true(nxt); i <- i + 1L
    } else dl_flag <- TRUE
  } else {
    clean_args <- c(clean_args, a)
  }
  i <- i + 1L
}

option_list <- list(
  make_option(c("-i","--input"),  type="character", default=NULL,
              help="Path to the CSV holding merged data (written here when downloading)"),
  make_option(c("-g","--gene"),   type="character", default=NULL,
              help="HGNC gene symbol (e.g. WWC1)"),
  make_option(c("-t","--time"),   type="character", default="time",
              help="Name of the survival-time column [default %default]"),
  make_option(c("-s","--status"), type="character", default="status",
              help="Name of the event column, 1=event 0=censored [default %default]"),
  make_option(c("-m","--split_method"), type="character", default="median",
              help="mRNA split: 'median', 'mean', 'optimal', 'quartile', or 'tertile' [default %default]"),
  make_option(c("-c","--cases"),  type="character", default=NULL,
              help="File path or comma-separated list of sample IDs to keep"),
  make_option(c("-u","--id_col"), type="character", default="sample_id",
              help="ID column used with --cases [default %default]"),
  make_option(c("--study_id"),    type="character", default=NULL,
              help="cBioPortal study ID (e.g. luad_tcga_pan_can_atlas_2018)"),
  make_option(c("--datatype"),    type="character", default="mRNA",
              help="mRNA | MUT | CNA [default %default]"),
  make_option(c("--endpoint"),    type="character", default="OS",
              help="OS | PFS | DFS [default %default]"),
  make_option(c("--profile_type"),type="character", default=NULL,
              help="Molecular profile id or suffix (auto-detected if omitted)"),
  make_option(c("--cna_mode"),    type="character", default="all",
              help="CNA grouping: 'all' (Deep deletion/Shallow deletion/Diploid/Gain/Amplification), 'three' (Diploid/Amp/Del), 'amp' or 'del' (vs rest), 'altered' [default %default]"),
  make_option(c("--time_unit"),   type="character", default="months",
              help="Unit for the survival-time column: 'months' or 'days' [default %default]"),
  make_option(c("--min_group"),   type="integer",   default=5,
              help="Minimum samples per group [default %default]"),
  make_option(c("--api"),         type="character", default="https://www.cbioportal.org/api",
              help="cBioPortal API base URL [default %default]"),
  make_option(c("--force"),       action="store_true", default=FALSE,
              help="Re-download even if the input CSV already exists"),
  make_option(c("-o","--out_plot"),    type="character", default="km_plot.pdf",
              help="Plot output; .pdf, .png or .svg [default %default]"),
  make_option(c("-r","--out_results"), type="character", default="survival_results.txt",
              help="Text summary output [default %default]"),
  make_option(c("-j","--out_json"),    type="character", default="survival_results.json",
              help="Machine-readable JSON summary for the web front-end [default %default]")
)

opt_parser <- OptionParser(option_list = option_list,
                           description = "Kaplan-Meier survival analysis of cBioPortal data")
opt <- parse_args(opt_parser, args = clean_args)
opt$download_cbio <- if (is.na(dl_flag)) FALSE else dl_flag

# --- normalise free-text options -------------------------------------------
dt <- toupper(trimws(opt$datatype))
opt$datatype <- (
  if (dt %in% c("MRNA","RNA","RNASEQ","RNA_SEQ","EXPRESSION","EXPR","MRNA_EXPRESSION")) "mRNA"
  else if (dt %in% c("MUT","MUTATION","MUTATIONS","SNV","MUTATION_EXTENDED"))            "MUT"
  else if (dt %in% c("CNA","CNV","COPY_NUMBER","COPY_NUMBER_ALTERATION","GISTIC"))       "CNA"
  else stop("--datatype must be one of mRNA, MUT, CNA (got '", opt$datatype, "')")
)
opt$endpoint     <- toupper(trimws(opt$endpoint))
opt$split_method <- tolower(trimws(opt$split_method))
if (identical(opt$split_method, "auto")) opt$split_method <- "optimal"
opt$time_unit    <- tolower(trimws(opt$time_unit))

if (is.null(opt$gene) || !nzchar(trimws(opt$gene)))
  stop("--gene is required.", call. = FALSE)
opt$gene <- toupper(trimws(opt$gene))

# a gene like HLA-A must not leak a '-' into a formula or a data.frame name
gene_col  <- make.names(opt$gene)
group_col <- make.names(paste0(opt$gene, "_group"))

# Auto-download when a study is given and we have nothing on disk: even if the
# front-end forgets the flag, the tool does the right thing instead of silently
# re-analysing a stale CSV.
if (!is.null(opt$study_id) && nzchar(opt$study_id)) {
  if (is.null(opt$input) || !nzchar(opt$input))
    opt$input <- paste0(opt$study_id, "_", opt$gene, "_", opt$datatype, "_", opt$endpoint, ".csv")
  if (opt$force || !file.exists(opt$input)) opt$download_cbio <- TRUE
} else if (is.null(opt$input)) {
  stop("Provide --input <csv> or --study_id <cBioPortal study> .", call. = FALSE)
}

cat("=== KM Survival Analyzer ===\n")
cat("gene      :", opt$gene, "\n")
cat("datatype  :", opt$datatype, "\n")
cat("endpoint  :", opt$endpoint, "\n")
cat("study     :", if (is.null(opt$study_id)) "(local file)" else opt$study_id, "\n")
cat("download  :", opt$download_cbio, "\n")
cat("data file :", opt$input, "\n\n")

# ============================================================================
# 2. REST HELPERS
# ============================================================================
API <- sub("/+$", "", opt$api)

api_call <- function(path, method = "GET", body = NULL, query = list(), tries = 3L) {
  if (!has_httr || !has_jsonlite)
    stop("Packages 'httr' and 'jsonlite' are required for downloading. ",
         "Install them with: install.packages(c('httr','jsonlite'))")
  url <- paste0(API, path)
  last <- NULL
  for (k in seq_len(tries)) {
    resp <- try(
      if (method == "GET")
        httr::GET(url, query = query, httr::timeout(180),
                  httr::user_agent("KM-Survival-Analyzer/2.0"))
      else
        httr::POST(url, query = query, httr::timeout(180),
                   httr::user_agent("KM-Survival-Analyzer/2.0"),
                   httr::content_type_json(),
                   body = jsonlite::toJSON(body, auto_unbox = TRUE)),
      silent = TRUE)

    if (inherits(resp, "try-error")) {
      last <- paste("network error:", conditionMessage(attr(resp, "condition")))
    } else if (httr::status_code(resp) >= 400) {
      last <- paste0("HTTP ", httr::status_code(resp), " on ", path, " :: ",
                     substr(httr::content(resp, "text", encoding = "UTF-8"), 1, 300))
      if (httr::status_code(resp) < 500) break   # client error: retrying won't help
    } else {
      txt <- httr::content(resp, "text", encoding = "UTF-8")
      if (!nzchar(txt)) return(NULL)
      return(jsonlite::fromJSON(txt, flatten = TRUE))
    }
    if (k < tries) Sys.sleep(2 * k)
  }
  stop("cBioPortal API request failed: ", last)
}

as_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  if (is.list(x) && length(x) && !is.null(names(x))) return(as.data.frame(x, stringsAsFactors = FALSE))
  data.frame()
}

# long clinical records -> wide table, one row per entity
pivot_clinical <- function(cl, id_field) {
  ids   <- unique(cl[[id_field]])
  attrs <- unique(cl$clinicalAttributeId)
  m <- matrix(NA_character_, nrow = length(ids), ncol = length(attrs),
              dimnames = list(NULL, attrs))
  m[cbind(match(cl[[id_field]], ids), match(cl$clinicalAttributeId, attrs))] <-
    as.character(cl$value)
  d <- as.data.frame(m, stringsAsFactors = FALSE)
  d$ENTITY_ID <- ids
  d
}

# "1:DECEASED" / "DECEASED" / "1" / "0:LIVING" / "1:PROGRESSION" -> 1 or 0
parse_event_status <- function(x) {
  xu <- toupper(trimws(as.character(x)))
  xu[xu %in% c("", "NA", "[NOT AVAILABLE]", "[UNKNOWN]", "NOT AVAILABLE", "UNKNOWN")] <- NA
  out <- rep(NA_integer_, length(xu))
  # cBioPortal encodes the event as the digit before the colon - trust it first
  lead <- suppressWarnings(as.integer(sub("^([01]):.*$", "\\1", xu)))
  ok   <- grepl("^[01]:", xu)
  out[ok] <- lead[ok]
  out[is.na(out) & xu %in% c("1","DECEASED","DEAD","DIED","YES","TRUE","EVENT",
                             "PROGRESSION","PROGRESSED","RECURRED","RECURRENCE",
                             "RECURRED/PROGRESSED")] <- 1L
  out[is.na(out) & xu %in% c("0","LIVING","ALIVE","NO","FALSE","CENSORED",
                             "DISEASEFREE","DISEASE FREE","DISEASE-FREE",
                             "PROGRESSIONFREE","PROGRESSION FREE","NOT PROGRESSED")] <- 0L
  out
}

resolve_endpoint_cols <- function(cols, endpoint) {
  sets <- list(
    OS  = list(t = c("OS_MONTHS","OVERALL_SURVIVAL_MONTHS","OS_TIME","SURVIVAL_MONTHS"),
               s = c("OS_STATUS","OVERALL_SURVIVAL_STATUS","VITAL_STATUS")),
    PFS = list(t = c("PFS_MONTHS","PROGRESSION_FREE_SURVIVAL_MONTHS","PFS_TIME"),
               s = c("PFS_STATUS","PROGRESSION_FREE_SURVIVAL_STATUS")),
    DFS = list(t = c("DFS_MONTHS","RFS_MONTHS","DISEASE_FREE_SURVIVAL_MONTHS"),
               s = c("DFS_STATUS","RFS_STATUS","DISEASE_FREE_SURVIVAL_STATUS"))
  )
  order <- switch(endpoint,
                  PFS = c("PFS","DFS","OS"),
                  DFS = c("DFS","PFS","OS"),
                  c("OS","PFS","DFS"))
  for (e in order) {
    tc <- intersect(sets[[e]]$t, cols)[1]
    sc <- intersect(sets[[e]]$s, cols)[1]
    if (!is.na(tc) && !is.na(sc)) {
      if (e != endpoint)
        cat("Note: no usable", endpoint, "columns - falling back to", e, "\n")
      return(list(time_col = tc, status_col = sc, endpoint = e))
    }
  }
  list(time_col = NA, status_col = NA, endpoint = NA)
}

# ============================================================================
# 3. DOWNLOAD
# ============================================================================
meta <- list(study = opt$study_id, gene = opt$gene, datatype = opt$datatype,
             endpoint_requested = opt$endpoint)

if (isTRUE(opt$download_cbio)) {

  if (is.null(opt$study_id) || !nzchar(opt$study_id))
    stop("--study_id is required when downloading.", call. = FALSE)

  cat("--- Step 1/6: study --------------------------------------------\n")
  study <- tryCatch(api_call(paste0("/studies/", utils::URLencode(opt$study_id, TRUE))),
                    error = function(e) NULL)
  if (is.null(study) || is.null(study$studyId)) {
    hits <- tryCatch(as_df(api_call("/studies", query = list(keyword = opt$study_id,
                                     pageSize = 20, projection = "SUMMARY"))),
                     error = function(e) data.frame())
    if (nrow(hits)) {
      cat("Study '", opt$study_id, "' not found. Closest matches:\n", sep = "")
      print(utils::head(hits[, intersect(c("studyId","name"), colnames(hits))], 10))
    }
    stop("Unknown cBioPortal study id: '", opt$study_id, "'")
  }
  cat("OK:", study$studyId, "-", study$name, "\n")

  cat("--- Step 2/6: molecular profile --------------------------------\n")
  profiles <- as_df(api_call(paste0("/studies/", opt$study_id, "/molecular-profiles")))
  if (!nrow(profiles)) stop("No molecular profiles in study ", opt$study_id)

  pick_profile <- function() {
    if (!is.null(opt$profile_type) && nzchar(trimws(opt$profile_type))) {
      hit <- profiles$molecularProfileId[
        profiles$molecularProfileId == opt$profile_type |
        grepl(opt$profile_type, profiles$molecularProfileId, fixed = TRUE)]
      if (length(hit)) return(hit[1])
      cat("Note: profile '", opt$profile_type, "' not found - auto-detecting.\n", sep = "")
    }
    if (opt$datatype == "mRNA") {
      p <- profiles[profiles$molecularAlterationType == "MRNA_EXPRESSION", , drop = FALSE]
      # prefer real expression values over z-scores, and z-scores over nothing
      for (sfx in c("rna_seq_v2_mrna$", "rna_seq_mrna$", "mrna_seq_v2_rsem$", "mrna_seq_rsem$",
                    "mrna_seq_tpm$", "mrna_seq_cpm$", "mrna_seq_fpkm$",
                    "rna_seq_v2_mrna_median_Zscores$", "mrna_median_Zscores$",
                    "rna_seq_v2_mrna_median_all_sample_Zscores$", "mrna$")) {
        hit <- p$molecularProfileId[grepl(sfx, p$molecularProfileId, ignore.case = TRUE)]
        if (length(hit)) return(hit[1])
      }
      if (nrow(p)) return(p$molecularProfileId[1])
    } else if (opt$datatype == "MUT") {
      p <- profiles[profiles$molecularAlterationType %in% c("MUTATION_EXTENDED","MUTATION"), , drop = FALSE]
      if (nrow(p)) return(p$molecularProfileId[1])
    } else {
      p <- profiles[profiles$molecularAlterationType == "COPY_NUMBER_ALTERATION", , drop = FALSE]
      pd <- p[!is.na(p$datatype) & p$datatype == "DISCRETE", , drop = FALSE]
      if (nrow(pd)) return(pd$molecularProfileId[1])
      if (nrow(p))  return(p$molecularProfileId[1])
    }
    NULL
  }
  profile_id <- pick_profile()
  if (is.null(profile_id)) {
    print(profiles[, intersect(c("molecularProfileId","name","molecularAlterationType","datatype"),
                               colnames(profiles))])
    stop("No ", opt$datatype, " profile available in ", opt$study_id)
  }
  is_zscore <- grepl("zscore", profile_id, ignore.case = TRUE)
  cat("OK:", profile_id, if (is_zscore) "(z-scores)" else "", "\n")

  cat("--- Step 3/6: gene ---------------------------------------------\n")
  g <- tryCatch(api_call(paste0("/genes/", utils::URLencode(opt$gene, TRUE))),
                error = function(e) NULL)
  entrez <- if (!is.null(g)) suppressWarnings(as.integer(g$entrezGeneId[1])) else NA_integer_
  if (is.na(entrez)) stop("Gene symbol '", opt$gene, "' not found in cBioPortal (check the HGNC symbol).")
  cat("OK:", opt$gene, "-> Entrez", entrez, "\n")

  cat("--- Step 4/6: samples ------------------------------------------\n")
  lists <- as_df(api_call(paste0("/studies/", opt$study_id, "/sample-lists")))
  want  <- switch(opt$datatype,
                  MUT  = c("_sequenced", "_mutations", "_all"),
                  CNA  = c("_cna", "_acgh", "_all"),
                  c("_rna_seq_v2_mrna", "_rna_seq_mrna", "_mrna", "_all"))
  list_id <- NA_character_
  for (sfx in want) {
    hit <- lists$sampleListId[endsWith(lists$sampleListId, sfx)]
    if (length(hit)) { list_id <- hit[1]; break }
  }
  if (is.na(list_id)) list_id <- lists$sampleListId[which.max(lists$sampleCount)]
  cat("sample list:", list_id, "\n")

  samples <- as_df(api_call(paste0("/studies/", opt$study_id, "/samples"),
                            query = list(projection = "SUMMARY", pageSize = "100000")))
  if (!nrow(samples)) stop("Could not list samples for ", opt$study_id)
  sample_map <- data.frame(sample_id  = as.character(samples$sampleId),
                           patient_id = as.character(samples$patientId),
                           sample_type = if ("sampleType" %in% names(samples))
                                           as.character(samples$sampleType) else NA_character_,
                           stringsAsFactors = FALSE)

  in_list <- as.character(api_call(paste0("/sample-lists/",
                                          utils::URLencode(list_id, TRUE), "/sample-ids")))
  if (length(in_list)) sample_map <- sample_map[sample_map$sample_id %in% in_list, , drop = FALSE]
  # drop normals - they have no survival meaning and skew any expression cutoff
  if (any(!is.na(sample_map$sample_type)))
    sample_map <- sample_map[is.na(sample_map$sample_type) |
                             !grepl("normal", sample_map$sample_type, ignore.case = TRUE), , drop = FALSE]
  cat("samples in scope:", nrow(sample_map), "\n")

  cat("--- Step 5/6: molecular data -----------------------------------\n")
  if (opt$datatype == "MUT") {
    mut <- as_df(api_call(paste0("/molecular-profiles/", utils::URLencode(profile_id, TRUE),
                                 "/mutations/fetch"),
                          method = "POST", query = list(projection = "DETAILED"),
                          body = list(entrezGeneIds = I(entrez), sampleListId = list_id)))
    if (!nrow(mut))
      stop("No Mutation Found")
    mutated_ids <- character(0)
    if (nrow(mut)) {
      keep <- rep(TRUE, nrow(mut))
      if ("mutationType" %in% names(mut))
        keep <- !grepl("silent|synonymous|3'UTR|5'UTR|3'Flank|5'Flank|IGR|Intron|RNA",
                       mut$mutationType, ignore.case = TRUE)
      if (!any(keep)) keep <- rep(TRUE, nrow(mut))   # only silent hits: keep them rather than lose the arm
      mutated_ids <- unique(as.character(mut$sampleId[keep]))
      lab <- if ("mutationType" %in% names(mut))
        tapply(as.character(mut$mutationType[keep]), as.character(mut$sampleId[keep]),
               function(v) paste(sort(unique(v)), collapse = ";")) else NULL
    }
    # THE KEY FIX: every profiled sample that is not in the mutation list is
    # wild-type.  Querying only mutations gives you a cohort of 100% mutants
    # and survdiff then dies with "there is only 1 group".
    mol <- data.frame(sample_id = sample_map$sample_id,
                      value = ifelse(sample_map$sample_id %in% mutated_ids, "Mutated", "Wild-type"),
                      stringsAsFactors = FALSE)
    if (exists("lab") && !is.null(lab))
      mol$mutation_detail <- ifelse(mol$sample_id %in% names(lab),
                                    lab[mol$sample_id], "None")
    cat("mutated:", length(mutated_ids), "/", nrow(mol), "profiled samples\n")

  } else {
    md <- as_df(api_call(paste0("/molecular-profiles/", utils::URLencode(profile_id, TRUE),
                                "/molecular-data/fetch"),
                         method = "POST", query = list(projection = "SUMMARY"),
                         body = list(entrezGeneIds = I(entrez), sampleListId = list_id)))
    if (!nrow(md))
      stop("No ", opt$datatype, " data returned for ", opt$gene, " in ", profile_id)
    val <- suppressWarnings(as.numeric(md$value))
    mol <- data.frame(sample_id = as.character(md$sampleId), value = val,
                      stringsAsFactors = FALSE)
    mol <- mol[!is.na(mol$value), , drop = FALSE]
    cat("values returned:", nrow(mol), "samples;",
        "range", paste(signif(range(mol$value), 4), collapse = " .. "), "\n")
  }

  cat("--- Step 6/6: clinical + merge ---------------------------------\n")
  pat <- as_df(api_call(paste0("/studies/", opt$study_id, "/clinical-data"),
                        query = list(clinicalDataType = "PATIENT",
                                     projection = "SUMMARY", pageSize = "10000000")))
  smp <- as_df(api_call(paste0("/studies/", opt$study_id, "/clinical-data"),
                        query = list(clinicalDataType = "SAMPLE",
                                     projection = "SUMMARY", pageSize = "10000000")))
  clin_pat <- if (nrow(pat)) pivot_clinical(pat, "patientId") else data.frame()
  clin_smp <- if (nrow(smp)) pivot_clinical(smp, "sampleId")  else data.frame()

  df <- merge(sample_map, mol, by = "sample_id", all.x = (opt$datatype == "MUT"), all.y = FALSE)
  if (!nrow(df)) stop("Molecular sample IDs did not match the study sample list.")

  # survival attributes usually live at patient level; some studies put them on samples
  cols_all <- unique(c(colnames(clin_pat), colnames(clin_smp)))
  res <- resolve_endpoint_cols(cols_all, opt$endpoint)
  if (is.na(res$time_col) || is.na(res$status_col)) {
    cat("Available clinical attributes:\n"); print(sort(cols_all))
    stop("No survival time/status attributes found in study ", opt$study_id)
  }
  cat("endpoint used  :", res$endpoint, "\n")
  cat("time attribute :", res$time_col, "\n")
  cat("status attribute:", res$status_col, "\n")

  take <- function(tab, key, id_target) {
    if (!nrow(tab)) return(NULL)
    keep <- intersect(c(res$time_col, res$status_col), colnames(tab))
    if (!length(keep)) return(NULL)
    out <- tab[, c("ENTITY_ID", keep), drop = FALSE]
    names(out)[1] <- id_target
    out
  }
  add_p <- take(clin_pat, "patient", "patient_id")
  add_s <- take(clin_smp, "sample",  "sample_id")

  if (!is.null(add_s)) {
    df <- merge(df, add_s, by = "sample_id", all.x = TRUE)
  } else if (!is.null(add_p)) {
    df <- merge(df, add_p, by = "patient_id", all.x = TRUE)
  }
  if (!all(c(res$time_col, res$status_col) %in% colnames(df)) && !is.null(add_p))
    df <- merge(df, add_p, by = "patient_id", all.x = TRUE)
  if (!all(c(res$time_col, res$status_col) %in% colnames(df)))
    stop("Could not attach survival columns to the samples.")

  months <- suppressWarnings(as.numeric(df[[res$time_col]]))
  df$os_months <- months
  df$os_days   <- months * 30.4375
  df$event     <- parse_event_status(df[[res$status_col]])

  df[[gene_col]]    <- df$value
  df[[opt$time]]    <- if (opt$time_unit == "days") df$os_days else df$os_months
  df[[opt$status]]  <- df$event

  # one row per patient: duplicate samples inflate n and break the log-rank test
  before <- nrow(df)
  ord <- order(is.na(df[[opt$time]]), df$sample_id)
  df <- df[ord, , drop = FALSE]
  df <- df[!duplicated(df$patient_id), , drop = FALSE]
  if (nrow(df) < before) cat("deduplicated", before - nrow(df), "extra samples per patient\n")

  usable <- sum(!is.na(df[[opt$time]]) & !is.na(df[[opt$status]]) & !is.na(df[[gene_col]]))
  cat("rows:", nrow(df), "| usable for survival:", usable, "\n")
  if (usable == 0) stop("Merged table has no usable survival rows.")

  df[] <- lapply(df, function(x) if (is.list(x)) vapply(x, function(y) paste(y, collapse=";"), "") else x)
  write.csv(df, file = opt$input, row.names = FALSE)
  cat("saved ->", opt$input, "\n\n")

  meta$profile   <- profile_id
  meta$is_zscore <- is_zscore
  meta$endpoint  <- res$endpoint
  meta$time_attr <- res$time_col
}

# ============================================================================
# 4. LOAD + FILTER
# ============================================================================
if (!file.exists(opt$input))
  stop("Data file not found: ", opt$input,
       "\nRun with --study_id <study> --download_cbio to fetch it first.")

cat("Reading:", opt$input, "\n")
df <- read.csv(opt$input, check.names = FALSE, stringsAsFactors = FALSE)
# check.names=FALSE keeps names like HLA-A intact; map to the safe name we use
if (!gene_col %in% names(df) && opt$gene %in% names(df)) names(df)[names(df) == opt$gene] <- gene_col

if (!is.null(opt$cases) && nzchar(opt$cases)) {
  if (!opt$id_col %in% colnames(df)) stop("ID column '", opt$id_col, "' not in the dataset.")
  cases <- if (file.exists(opt$cases)) paste(readLines(opt$cases, warn = FALSE), collapse = ",") else opt$cases
  cases <- trimws(unlist(strsplit(cases, "[,;[:space:]]+")))
  cases <- cases[nzchar(cases)]
  before_ids <- df[[opt$id_col]]
  df <- df[df[[opt$id_col]] %in% cases, , drop = FALSE]
  cat("case filter: kept", nrow(df), "of", length(before_ids), "rows\n")
  if (!nrow(df)) {
    cat("[debug] requested :", paste(utils::head(cases, 5), collapse = ", "), "\n")
    cat("[debug] in dataset:", paste(utils::head(before_ids, 5), collapse = ", "), "\n")
    stop("No samples left after --cases filtering (ID format mismatch?).")
  }
}

need <- c(opt$time, opt$status, gene_col)
miss <- setdiff(need, colnames(df))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "),
                       "\nColumns present: ", paste(colnames(df), collapse = ", "))

df[[opt$time]]   <- suppressWarnings(as.numeric(df[[opt$time]]))
df[[opt$status]] <- parse_event_status(df[[opt$status]])
df <- df[!is.na(df[[opt$time]]) & !is.na(df[[opt$status]]) & df[[opt$time]] >= 0, , drop = FALSE]
if (opt$datatype != "MUT") df[[gene_col]] <- suppressWarnings(as.numeric(df[[gene_col]]))
df <- df[!is.na(df[[gene_col]]) & df[[gene_col]] != "", , drop = FALSE]
cat("analysable samples:", nrow(df), "\n")
if (nrow(df) < 2 * opt$min_group) stop("Too few samples with complete data: ", nrow(df))

# ============================================================================
# 5. STRATIFY
# ============================================================================
x <- df[[gene_col]]
cutoff <- NA_real_
best_p <- NA_real_

group_of <- function(cut) factor(ifelse(x > cut, "High", "Low"), levels = c("Low","High"))

if (opt$datatype == "MUT") {
  mutated <- toupper(trimws(as.character(x))) %in% c("MUTATED","MUT","1","TRUE","YES")
  if (!any(mutated))  # legacy CSVs stored the mutation type itself, blanks = WT
    mutated <- !is.na(x) & nzchar(trimws(as.character(x))) &
               !toupper(trimws(as.character(x))) %in% c("WILD-TYPE","WILDTYPE","WT","NA","NONE","0")
  df[[group_col]] <- factor(ifelse(mutated, "Mutated", "Wild-type"),
                            levels = c("Wild-type","Mutated"))

} else if (opt$datatype == "CNA") {
  v <- as.numeric(x)
  df[[group_col]] <- switch(opt$cna_mode,
    amp     = factor(ifelse(v >=  2, "Amplified", "Not amplified"), levels = c("Not amplified","Amplified")),
    del     = factor(ifelse(v <= -2, "Deleted",   "Not deleted"),   levels = c("Not deleted","Deleted")),
    altered = factor(ifelse(v != 0,  "Altered",   "Diploid"),       levels = c("Diploid","Altered")),
    three   = factor(ifelse(v > 0, "Amplified", ifelse(v < 0, "Deleted", "Diploid")),
                     levels = c("Diploid","Amplified","Deleted")),
    strict  = factor(ifelse(v <= -2, "Deep deletion", ifelse(v >= 2, "Amplification", ifelse(v == 0, "Diploid", NA))),
                     levels = c("Deep deletion", "Diploid", "Amplification")),
    {
      cna_labels <- c("-2" = "Deep deletion", "-1" = "Shallow deletion", "0" = "Diploid", "1" = "Gain", "2" = "Amplification")
      mapped <- cna_labels[as.character(round(v))]
      factor(mapped, levels = c("Deep deletion", "Shallow deletion", "Diploid", "Gain", "Amplification"))
    })

} else {
  if (opt$split_method == "optimal") {
    cat("scanning cutpoints between the 25th and 75th percentile...\n")
    lo <- stats::quantile(x, .25, na.rm = TRUE); hi <- stats::quantile(x, .75, na.rm = TRUE)
    cand <- sort(unique(x[x >= lo & x <= hi]))
    best_p <- 1; cutoff <- stats::median(x, na.rm = TRUE)
    for (cut in cand) {
      g <- group_of(cut)
      if (nlevels(droplevels(g)) < 2 || min(table(g)) < opt$min_group) next
      sd <- tryCatch(survdiff(Surv(df[[opt$time]], df[[opt$status]]) ~ g), error = function(e) NULL)
      if (!is.null(sd)) {
        p <- stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
        if (!is.na(p) && p < best_p) { best_p <- p; cutoff <- cut }
      }
    }
    cat("best cutpoint:", signif(cutoff, 5), " (nominal p =", signif(best_p, 4), ")\n")
    cat("NOTE: an optimal cutpoint is selected on the outcome - the p-value is\n",
        "     optimistic and needs validation or a permutation correction.\n")
  } else {
    cutoff <- switch(opt$split_method,
      median   = stats::median(x, na.rm = TRUE),
      mean     = mean(x, na.rm = TRUE),
      quartile = stats::quantile(x, .75, na.rm = TRUE),
      tertile  = stats::quantile(x, 2/3, na.rm = TRUE),
      stop("--split_method must be median, mean, optimal, quartile or tertile"))
    cat("cutpoint (", opt$split_method, "):", signif(cutoff, 5), "\n")
  }
  df[[group_col]] <- group_of(cutoff)
}

df[[group_col]] <- droplevels(df[[group_col]])
tab <- table(df[[group_col]])
print(tab)
if (nlevels(df[[group_col]]) < 2)
  stop("Only one group after stratification (", paste(names(tab), tab, collapse = ", "),
       "). With ", opt$datatype, " data try a different --split_method / --cna_mode, ",
       "or a study where this gene is actually altered.")
if (min(tab) < opt$min_group)
  cat("WARNING: smallest group has only", min(tab), "samples - the result is fragile.\n")

# ============================================================================
# 6. FIT
# ============================================================================
fml <- stats::as.formula(paste0("Surv(`", opt$time, "`, `", opt$status, "`) ~ `", group_col, "`"))
km  <- survfit(fml, data = df)
km$call$formula <- fml

sd    <- survdiff(fml, data = df)
p_val <- stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
cat("log-rank p =", signif(p_val, 5), "\n")

hr <- hr_lo <- hr_hi <- hr_p <- NA_real_
if (nlevels(df[[group_col]]) == 2) {
  cx <- tryCatch(coxph(fml, data = df), error = function(e) NULL)
  if (!is.null(cx)) {
    s <- summary(cx)
    hr    <- unname(s$conf.int[1, "exp(coef)"])
    hr_lo <- unname(s$conf.int[1, "lower .95"])
    hr_hi <- unname(s$conf.int[1, "upper .95"])
    hr_p  <- unname(s$coefficients[1, ncol(s$coefficients)])
    cat("HR =", signif(hr, 4), " 95% CI", signif(hr_lo, 4), "-", signif(hr_hi, 4),
        " (Cox p =", signif(hr_p, 4), ")\n")
  }
}

med <- summary(km)$table
if (is.null(dim(med))) med <- t(as.matrix(med))
unit_lab <- if (opt$time_unit == "days") "Days" else "Months"

# ============================================================================
# 7. OUTPUTS
# ============================================================================
con <- file(opt$out_results, open = "wt")
sink(con); sink(con, type = "message")
cat("=== Survival Analysis Results ===\n")
cat("Study        :", if (is.null(opt$study_id)) "local file" else opt$study_id, "\n")
cat("Gene         :", opt$gene, "\n")
cat("Data type    :", opt$datatype, "\n")
cat("Profile      :", if (is.null(meta$profile)) "n/a" else meta$profile, "\n")
cat("Endpoint     :", if (is.null(meta$endpoint)) opt$endpoint else meta$endpoint, "\n")
cat("Grouping     :", if (opt$datatype == "mRNA") opt$split_method else
                      if (opt$datatype == "CNA") opt$cna_mode else "mutation status",
    if (is.na(cutoff)) "" else paste0("(cutoff = ", signif(cutoff, 6), ")"), "\n")
cat("Samples      :", nrow(df), "\n")
cat("Log-rank p   :", signif(p_val, 5), "\n")
if (!is.na(hr)) cat("Hazard ratio :", signif(hr, 4), " (95% CI ",
                    signif(hr_lo, 4), "-", signif(hr_hi, 4), ", Cox p ", signif(hr_p, 4), ")\n", sep = "")
cat("\n--- Group sizes ---\n"); print(tab)
cat("\n--- Kaplan-Meier (time in ", tolower(unit_lab), ") ---\n", sep = ""); print(km)
sink(type = "message"); sink(); close(con)
cat("wrote", opt$out_results, "\n")

if (has_jsonlite) {
  grp_names <- rownames(med); if (is.null(grp_names)) grp_names <- levels(df[[group_col]])
  groups <- lapply(seq_len(nrow(med)), function(k) list(
    name    = sub("^.*=", "", grp_names[k]),
    n       = unname(med[k, "records"]),
    events  = unname(med[k, "events"]),
    median  = unname(med[k, "median"])))
  js <- list(
    study = opt$study_id, gene = opt$gene, datatype = opt$datatype,
    profile = meta$profile, endpoint = if (is.null(meta$endpoint)) opt$endpoint else meta$endpoint,
    split_method = opt$split_method, cutoff = if (is.na(cutoff)) NULL else unname(cutoff),
    time_unit = opt$time_unit, n = nrow(df),
    logrank_p = unname(p_val), hazard_ratio = if (is.na(hr)) NULL else unname(hr),
    hr_ci_low = if (is.na(hr_lo)) NULL else unname(hr_lo),
    hr_ci_high = if (is.na(hr_hi)) NULL else unname(hr_hi),
    cox_p = if (is.na(hr_p)) NULL else unname(hr_p),
    groups = groups, plot = opt$out_plot, data = opt$input,
    warnings = if (min(tab) < opt$min_group) "small group size" else NULL)
  writeLines(jsonlite::toJSON(js, auto_unbox = TRUE, null = "null", na = "null",
                              pretty = TRUE), opt$out_json)
  cat("wrote", opt$out_json, "\n")
}

# --- plot -------------------------------------------------------------------
ext <- tolower(tools::file_ext(opt$out_plot))
open_dev <- function() {
  if (ext == "png") grDevices::png(opt$out_plot, width = 2000, height = 1500, res = 200)
  else if (ext == "svg") grDevices::svg(opt$out_plot, width = 9, height = 7)
  else grDevices::pdf(opt$out_plot, width = 9, height = 7, onefile = FALSE)
}
n_lev <- nlevels(df[[group_col]])
pal   <- if (n_lev == 2) c("#2E6FDB", "#D62828") else
         c("#6B7280", "#D62828", "#2E6FDB", "#0E9F6E", "#F59E0B")[seq_len(n_lev)]
sub_t <- paste0(nrow(df), " patients | log-rank p = ", signif(p_val, 3),
                if (!is.na(hr)) paste0(" | HR = ", signif(hr, 3)) else "")
if (opt$datatype == "CNA")
  sub_t <- paste0(sub_t, "\nGISTIC classes (--cna_mode ", opt$cna_mode, ")")

open_dev()
ok <- try({
  if (has_survminer) {
    suppressPackageStartupMessages(library(survminer))
    p <- survminer::ggsurvplot(
      km, data = df, pval = TRUE, conf.int = (n_lev <= 3), conf.int.alpha = 0.12,
      risk.table = TRUE, risk.table.height = min(0.42, 0.18 + 0.055 * n_lev),
      risk.table.y.text = TRUE, tables.theme = survminer::theme_cleantable(),
      censor.shape = "|", censor.size = 3,
      xlab = paste0("Time (", unit_lab, ")"), ylab = "Survival probability",
      title = paste(c(opt$gene, opt$datatype, opt$study_id), collapse = " - "),
      subtitle = sub_t, legend.title = opt$gene, legend = "right",
      legend.labs = levels(df[[group_col]]), palette = pal,
      ggtheme = ggplot2::theme_bw())
    print(p)
  } else {
    plot(km, col = pal, lty = 1, lwd = 2, mark.time = TRUE,
         main = paste(c(opt$gene, opt$datatype, opt$study_id), collapse = " - "),
         xlab = paste0("Time (", unit_lab, ")"), ylab = "Survival probability")
    legend("bottomleft", legend = levels(df[[group_col]]), col = pal, lty = 1, lwd = 2, bty = "n", cex = 0.85)
    mtext(sub_t, side = 3, line = 0.2, cex = 0.8)
  }
}, silent = TRUE)
invisible(grDevices::dev.off())
if (inherits(ok, "try-error")) {
  cat("Plot rendering failed:", conditionMessage(attr(ok, "condition")), "\n")
} else cat("wrote", opt$out_plot, "\n")

cat("Analysis complete.\n")
