document.addEventListener('DOMContentLoaded', () => {
    // --- UI Elements ---
    const form = document.getElementById('analysisForm');

    const casesSelect = document.getElementById('cases');
    const submitBtn = document.getElementById('submitBtn');
    const btnText = submitBtn.querySelector('.btn-text');
    const spinner = submitBtn.querySelector('.spinner');
    
    const resultsContainer = document.getElementById('resultsContainer');
    const tabBtns = document.querySelectorAll('.tab-btn');
    const tabContents = document.querySelectorAll('.tab-content');
    
    const pdfViewer = document.getElementById('pdfViewer');
    const downloadPdf = document.getElementById('downloadPdf');
    const statsText = document.getElementById('statsText');
    const downloadStats = document.getElementById('downloadStats');
    const consoleLogs = document.getElementById('consoleLogs');



    // --- Tab Switching ---
    tabBtns.forEach(btn => {
        btn.addEventListener('click', () => {
            // Remove active classes
            tabBtns.forEach(b => b.classList.remove('active'));
            tabContents.forEach(c => c.classList.remove('active'));
            
            // Add active class to clicked tab
            btn.classList.add('active');
            const targetId = btn.getAttribute('data-target');
            document.getElementById(targetId).classList.add('active');
        });
    });

    // --- Form Submission ---
    form.addEventListener('submit', async (e) => {
        e.preventDefault();
        
        // Prepare payload
        const formData = new FormData(form);
        const payload = {
            gene: formData.get('gene'),
            split_method: formData.get('split_method'),
        };

        payload.download_tcga = true;
        payload.tcga_project = formData.get('tcga_project');

        const casesVal = formData.get('cases');
        if (casesVal) {
            // Convert newlines (and spaces) to commas to support copy-pasting vertical lists
            payload.cases = casesVal.replace(/[\n\r\s]+/g, ',').split(',').filter(s => s).join(',');
            payload.id_col = formData.get('id_col');
        }

        // Set Loading State
        submitBtn.disabled = true;
        btnText.textContent = 'Processing...';
        spinner.classList.remove('hidden');
        resultsContainer.classList.add('hidden');
        consoleLogs.textContent = "Starting analysis...\n";

        try {
            const response = await fetch('/api/analyze', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(payload)
            });
            
            const result = await response.json();
            
            // Populate logs
            if (result.stdout) consoleLogs.textContent += result.stdout + "\n";
            if (result.stderr) consoleLogs.textContent += "STDERR:\n" + result.stderr + "\n";

            if (response.ok && result.status === 'success') {
                // Fetch text results
                const statsRes = await fetch('/api/results/survival_results.txt?t=' + Date.now());
                if (statsRes.ok) {
                    const text = await statsRes.text();
                    statsText.textContent = text;
                    downloadStats.href = '/api/results/survival_results.txt?t=' + Date.now();
                } else {
                    statsText.textContent = "Could not load statistics.";
                }

                // Update PDF viewer
                const pdfUrl = '/api/results/km_plot.pdf?t=' + Date.now();
                pdfViewer.src = pdfUrl;
                downloadPdf.href = pdfUrl;

                // Show results card and switch to plot tab
                resultsContainer.classList.remove('hidden');
                document.querySelector('[data-target="plotTab"]').click();
                resultsContainer.scrollIntoView({ behavior: 'smooth' });
                
            } else {
                throw new Error(result.message || "Unknown error occurred.");
            }
            
        } catch (error) {
            consoleLogs.textContent += "\nERROR: " + error.message;
            resultsContainer.classList.remove('hidden');
            document.querySelector('[data-target="logsTab"]').click();
            alert("Analysis failed. Check Console Logs for details.");
        } finally {
            // Restore button state
            submitBtn.disabled = false;
            btnText.textContent = 'Run Analysis';
            spinner.classList.add('hidden');
        }
    });
});
