@echo off
echo Installing required R packages (optparse, survival, survminer, httr, jsonlite, ggplot2)...
Rscript -e "if (!require('optparse', character.only=TRUE)) install.packages('optparse', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('survival', character.only=TRUE)) install.packages('survival', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('survminer', character.only=TRUE)) install.packages('survminer', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('httr', character.only=TRUE)) install.packages('httr', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('jsonlite', character.only=TRUE)) install.packages('jsonlite', repos='http://cran.us.r-project.org')"
Rscript -e "if (!require('ggplot2', character.only=TRUE)) install.packages('ggplot2', repos='http://cran.us.r-project.org')"

echo.
echo ======================================
echo All R dependencies installed successfully!
echo You can now run: python server.py
echo ======================================
pause
