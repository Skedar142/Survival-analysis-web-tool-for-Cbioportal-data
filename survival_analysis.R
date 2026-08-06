#!/usr/bin/env Rscript

# suppressPackageStartupMessages is used to keep the terminal output clean
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(survival))

# Define command line options
option_list <- list(
  make_option(c("-i", "--input"), type="character", default=NULL, 
              help="Path to input dataset (CSV format)", metavar="character"),
  make_option(c("-g", "--gene"), type="character", default=NULL, 
              help="Name of the gene column in the dataset", metavar="character"),
  make_option(c("-t", "--time"), type="character", default="time", 
              help="Name of the time column [default= %default]", metavar="character"),
  make_option(c("-s", "--status"), type="character", default="status", 
              help="Name of the event status column (1=event, 0=censored) [default= %default]", metavar="character"),
  make_option(c("-m", "--split_method"), type="character", default="median", 
              help="Method to split gene expression: 'median' or 'mean' [default= %default]", metavar="character"),
  make_option(c("-c", "--cases"), type="character", default=NULL, 
              help="File path or comma-separated list of specific case/sample IDs to keep", metavar="character"),
  make_option(c("-u", "--id_col"), type="character", default="sample_id", 
              help="Name of the column containing case/sample IDs (used with --cases) [default= %default]", metavar="character"),
  make_option(c("-d", "--download_tcga"), action="store_true", default=FALSE, 
              help="Flag to download TCGA data. Saves the downloaded data to the path in --input."),
  make_option(c("-p", "--tcga_project"), type="character", default=NULL, 
              help="TCGA Project ID (e.g., 'TCGA-BRCA'). Required if --download_tcga is used.", metavar="character"),
  make_option(c("--gdc_dir"), type="character", default="GDCdata", 
              help="Directory where GDCdata is stored [default= %default]", metavar="character"),
  make_option(c("-o", "--out_plot"), type="character", default="km_plot.pdf", 
              help="Output path for the Kaplan-Meier plot (PDF) [default= %default]", metavar="character"),
  make_option(c("-r", "--out_results"), type="character", default="survival_results.txt", 
              help="Output path for the statistical summary [default= %default]", metavar="character")
)

opt_parser <- OptionParser(option_list=option_list, description="Survival Analysis for Cell Lines based on Gene Expression")
opt <- parse_args(opt_parser)

# Check required arguments
if (is.null(opt$gene)){
  print_help(opt_parser)
  stop("Gene column name (--gene) is required. See help.", call.=FALSE)
}
if (!opt$download_tcga && is.null(opt$input)) {
  print_help(opt_parser)
  stop("Input file (--input) is required unless --download_tcga is specified.", call.=FALSE)
}
if (opt$download_tcga && is.null(opt$tcga_project)) {
  print_help(opt_parser)
  stop("--tcga_project must be provided when using --download_tcga (e.g., 'TCGA-BRCA').", call.=FALSE)
}
if (opt$download_tcga && is.null(opt$input)) {
  opt$input <- paste0(opt$tcga_project, "_", opt$gene, "_data.csv")
}

# Load survminer if available for better plotting
has_survminer <- requireNamespace("survminer", quietly = TRUE)

# Download TCGA data if requested
if (opt$download_tcga) {
  if (!requireNamespace("TCGAbiolinks", quietly = TRUE)) {
    cat("Installing TCGAbiolinks from Bioconductor...\n")
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager", repos = "http://cran.us.r-project.org")
    BiocManager::install("TCGAbiolinks", ask=FALSE, update=FALSE)
  }
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    cat("Installing SummarizedExperiment from Bioconductor...\n")
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager", repos = "http://cran.us.r-project.org")
    BiocManager::install("SummarizedExperiment", ask=FALSE, update=FALSE)
  }
  
  suppressPackageStartupMessages(library(TCGAbiolinks))
  suppressPackageStartupMessages(library(SummarizedExperiment))
  
  cat("\n=== Step 1: Downloading RNA-Seq and Clinical Data for", opt$tcga_project, "===\n")
  query_rna <- GDCquery(
    project = opt$tcga_project,
    data.category = "Transcriptome Profiling",
    data.type = "Gene Expression Quantification",
    workflow.type = "STAR - Counts"
  )
  cat("Checking GDC directory:", opt$gdc_dir, "...\n")
  GDCdownload(query_rna, directory = opt$gdc_dir)
  se <- GDCprepare(query_rna, directory = opt$gdc_dir)
  
  gene_idx <- which(rowData(se)$gene_name == opt$gene)
  if (length(gene_idx) == 0) {
    stop(paste("Gene", opt$gene, "not found in the TCGA RNA-Seq data. Check HGNC symbol."))
  }
  
  assay_names <- assayNames(se)
  if ("tpm_unstrand" %in% assay_names) {
    expr_vals <- assay(se, "tpm_unstrand")[gene_idx[1], ]
  } else if ("unstranded" %in% assay_names) {
    expr_vals <- assay(se, "unstranded")[gene_idx[1], ]
  } else {
    expr_vals <- assay(se, 1)[gene_idx[1], ]
  }
  cat("\n=== Step 2: Parsing Clinical and Expression Data ===\n")
  clinical <- as.data.frame(colData(se))
  
  clinical$os_time <- as.numeric(ifelse(!is.na(clinical$days_to_death), clinical$days_to_death, clinical$days_to_last_follow_up))
  clinical$os_status <- ifelse(clinical$vital_status == "Dead", 1, 
                               ifelse(clinical$vital_status == "Alive", 0, NA))
  
  sample_ids <- colnames(se)
  patient_ids <- substr(sample_ids, 1, 12)
  
  merged_df <- data.frame(
    sample_id = sample_ids,
    patient_id = patient_ids,
    gene_expr = as.numeric(expr_vals),
    stringsAsFactors = FALSE
  )
  
  merged_df <- cbind(merged_df, clinical)
  
  merged_df[[opt$time]] <- merged_df$os_time
  merged_df[[opt$status]] <- merged_df$os_status
  merged_df[[opt$gene]] <- merged_df$gene_expr
  
  cat("\n=== Step 3: Saving Merged Data ===\n")
  
  # Flatten any list columns to strings (write.csv crashes on lists)
  merged_df[] <- lapply(merged_df, function(x) {
    if (is.list(x)) {
      sapply(x, function(y) paste(y, collapse = "; "))
    } else {
      x
    }
  })
  
  cat("Saving to:", opt$input, "\n\n")
  write.csv(merged_df, file = opt$input, row.names = FALSE)
}

# Read the data
cat("Reading data from:", opt$input, "\n")
df <- read.csv(opt$input)

# Filter by specific cases if provided
if (!is.null(opt$cases)) {
  if (!opt$id_col %in% colnames(df)) {
    stop(paste("ID column", opt$id_col, "not found in dataset. Cannot filter by cases."))
  }
  

  if (file.exists(opt$cases)) {
    cat("Reading cases to keep from file:", opt$cases, "
")
    # Read file, paste all lines into one string, then split by commas or spaces
    raw_text <- paste(readLines(opt$cases), collapse = ",")
    cases_to_keep <- trimws(unlist(strsplit(raw_text, "[,[:space:]]+")))
    cases_to_keep <- cases_to_keep[cases_to_keep != ""]
  } else {

    cat("Parsing cases to keep from string...\n")
    cases_to_keep <- trimws(unlist(strsplit(opt$cases, "[,[:space:]]+")))
    cases_to_keep <- cases_to_keep[cases_to_keep != ""]
  }
  
  initial_rows <- nrow(df)
  df <- df[df[[opt$id_col]] %in% cases_to_keep, ]
  cat("Filtered data to specific cases. Kept", nrow(df), "out of", initial_rows, "rows.\n")
  
  if (nrow(df) == 0) {
    cat("\n[DEBUG] Could not match any cases!\n")
    cat("First 5 cases you requested:\n")
    print(head(cases_to_keep, 5))
    cat("First 5 IDs in the dataset column '", opt$id_col, "':\n", sep="")
    print(head(df[[opt$id_col]], 5))
    stop("No data left after filtering. Please check your case IDs and ID column.")
  }
}

# Check if columns exist
required_cols <- c(opt$time, opt$status, opt$gene)
missing_cols <- setdiff(required_cols, colnames(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in dataset:", paste(missing_cols, collapse=", ")))
}

# Remove rows with NA in the required columns
df <- df[complete.cases(df[, required_cols]), ]
cat("Number of complete observations to analyze:", nrow(df), "\n")

# Extract the gene expression vector
gene_expr <- df[[opt$gene]]

# Split the group based on the specified method
cat("Splitting subjects into High/Low groups based on", opt$split_method, "of", opt$gene, "expression...\n")
if (opt$split_method == "median") {
  cutoff <- median(gene_expr, na.rm=TRUE)
} else if (opt$split_method == "mean") {
  cutoff <- mean(gene_expr, na.rm=TRUE)
} else {
  stop("Invalid split method. Use 'median' or 'mean'.")
}

# Create a new categorical variable for the gene
group_col <- paste0(opt$gene, "_group")
df[[group_col]] <- ifelse(gene_expr > cutoff, "High", "Low")
df[[group_col]] <- factor(df[[group_col]], levels=c("Low", "High"))

# Fit the survival model
formula_str <- paste0("Surv(", opt$time, ", ", opt$status, ") ~ ", group_col)
surv_formula <- as.formula(formula_str)

cat("Fitting Kaplan-Meier model...\n")
km_fit <- survfit(surv_formula, data = df)
km_fit$call$formula <- surv_formula

# Perform Log-Rank test
surv_diff <- survdiff(surv_formula, data = df)
p_val <- 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
cat("Log-rank test p-value:", p_val, "\n")

# Save results
cat("Saving summary to:", opt$out_results, "\n")
sink(opt$out_results)
cat("=== Survival Analysis Results ===\n")
cat("Gene Analyzed:", opt$gene, "\n")
cat("Split Method:", opt$split_method, "( Cutoff =", cutoff, ")\n")
cat("Log-rank p-value:", signif(p_val, digits=5), "\n\n")
print(km_fit)
sink()

# Generate Plot
cat("Generating plot: ", opt$out_plot, "\n")
pdf(opt$out_plot, width=8, height=6, onefile=FALSE)
if (has_survminer) {
  suppressPackageStartupMessages(library(survminer))
  p <- ggsurvplot(
    km_fit,
    data = df,
    pval = TRUE,
    conf.int = TRUE,
    risk.table = TRUE,
    title = paste("Kaplan-Meier Curve based on", opt$gene, "Expression"),
    legend.title = paste(opt$gene, "Group"),
    palette = c("blue", "red")
  )
  print(p)
} else {
  # Base R plot fallback if survminer is not installed
  plot(km_fit, col=c("blue", "red"), lty=1:2, 
       main=paste("Kaplan-Meier Curve based on", opt$gene, "Expression"),
       xlab="Time", ylab="Survival Probability")
  legend("bottomleft", legend=c("Low", "High"), col=c("blue", "red"), lty=1:2)
  
  # Try to add a p-value annotation
  x_pos <- max(df[[opt$time]], na.rm=TRUE) * 0.05
  y_pos <- 0.1
  text(x=x_pos, y=y_pos, labels=paste("Log-rank p-value =", signif(p_val, 4)), adj=0)
}
invisible(dev.off())

cat("Analysis complete!\n")
