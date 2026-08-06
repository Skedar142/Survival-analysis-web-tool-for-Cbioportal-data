@echo off
echo Installing required R packages (optparse, survival, survminer)...
Rscript -e "if (!require('optparse', character.only=TRUE)) install.packages('optparse', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('survival', character.only=TRUE)) install.packages('survival', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('survminer', character.only=TRUE)) install.packages('survminer', repos='http://cran.us.r-project.org')"

echo Installing Bioconductor and TCGAbiolinks...
Rscript -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos='http://cran.us.r-project.org')"
Rscript -e "BiocManager::install(c('TCGAbiolinks', 'SummarizedExperiment'), ask = FALSE)"

echo.
echo ======================================
echo All R dependencies installed successfully!
echo You can now run: python server.py
echo ======================================
pause
