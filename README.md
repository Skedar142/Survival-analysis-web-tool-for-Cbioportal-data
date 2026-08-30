# Survival Analysis Web Tool

![Compatibility](https://img.shields.io/badge/Platform-Windows%20%7C%20Mac%20%7C%20Linux-success)
![Python](https://img.shields.io/badge/Python-3.x-yellow)
![R](https://img.shields.io/badge/R-4.x-blue)

A lightweight, visually appealing web interface for running Kaplan-Meier survival analysis and log-rank tests locally. 

This tool is designed to run entirely on **your own machine** (using your local processor and memory) with zero external server dependencies, making it perfect for secure and private data analysis. **Fully compatible with Windows, Mac, and Linux.**

## ✨ Features
* **Cross-Platform Compatibility**: Works flawlessly on Windows, macOS, and Linux (including Zorin OS and Ubuntu).
* **Zero-Dependency Backend**: Uses Python 3's built-in HTTP server module. No need to install Node.js, `npm`, or heavy `pip` frameworks.
* **Beautiful UI**: A responsive, dark-mode glassmorphism interface built with Vanilla CSS and JS.
* **cBioPortal Integration**: Optionally download and analyze RNA-Seq, mutation, CNA, and clinical data directly from cBioPortal API.
* **Real-time Results**: View the generated Kaplan-Meier PDF plots and statistical log-rank summaries directly in the browser.

## ⚙️ Prerequisites

To run this tool on your machine, you must have the following installed:

1. [Python 3](https://www.python.org/downloads/) - To run the backend web server.
2. [R](https://cran.r-project.org/) - To execute the survival analysis script.

### 📦 Install R Dependencies

Before running the app for the first time, you need to install the required R packages (`optparse`, `survival`, `survminer`, `httr`, `jsonlite`, `ggplot2`).

**For Windows:**
Double-click the `install_dependencies.bat` file, or run it via Command Prompt:
```cmd
install_dependencies.bat
```

**For Mac / Linux (Ubuntu, Zorin OS, etc.):**
Open your terminal and run the shell script:
```bash
chmod +x install_dependencies.sh
./install_dependencies.sh
```

## 🚀 How to Run

1. **Clone or Download** this repository to your computer.
2. **Open a Terminal** (or Command Prompt on Windows) and navigate into the project folder.
3. **Start the Server**:
    * **Windows**:
      ```cmd
      python server.py
      ```
    * **Mac / Linux**:
      ```bash
      python3 server.py
      ```
4. **Open the App**: Open your web browser and go to [http://localhost:3000](http://localhost:3000). *(Do not open the `index.html` file directly in your browser, as it requires the server to communicate with the backend).*

## 💡 Usage

* **cBioPortal Download**: Select the "Download from cBioPortal" option to dynamically fetch study data. 
* *Note: Downloading data can take a moment depending on your internet connection and the size of the dataset.*

---
*Built for local, secure, and beautiful bioinformatics analysis.*
