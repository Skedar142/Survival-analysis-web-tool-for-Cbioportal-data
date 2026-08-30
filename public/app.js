document.addEventListener('DOMContentLoaded', () => {

    // =========================================================
    // Elements
    // =========================================================
    const form            = document.getElementById('analysisForm');
    const submitBtn       = document.getElementById('submitBtn');
    const runBtnText      = submitBtn.querySelector('.run-btn-text');
    const runSpinner      = submitBtn.querySelector('.run-spinner');
    const runBtnIcon      = submitBtn.querySelector('.run-btn-icon');

    const studyIdInput    = document.getElementById('study_id');
    const studyBadge      = document.getElementById('studyBadge');
    const studyBadgeText  = document.getElementById('studyBadgeText');
    const clearStudyBtn   = document.getElementById('clearStudy');
    const studyHint       = document.getElementById('studyHint');
    const openBrowserBtn  = document.getElementById('openBrowserBtn');
    const browserBtnLabel = document.getElementById('browserBtnLabel');

    const studyModal      = document.getElementById('studyModal');
    const modalOverlay    = document.getElementById('studyModalOverlay');
    const closeModalBtn   = document.getElementById('closeModalBtn');
    const categorySidebar = document.getElementById('categorySidebar');
    const studyListPanel  = document.getElementById('studyListPanel');
    const modalStudyCount = document.getElementById('modalStudyCount');
    const modalSearchInput= document.getElementById('modalSearchInput');

    const profileSelect   = document.getElementById('profile_type');
    const profileHint     = document.getElementById('profileHint');

    const filterRNA       = document.getElementById('filterRNA');
    const filterMUT       = document.getElementById('filterMUT');
    const filterCNA       = document.getElementById('filterCNA');

    const resultsPanel    = document.getElementById('resultsPanel');
    const emptyState      = document.getElementById('emptyState');
    const resultsContent  = document.getElementById('resultsContent');
    const tabs            = document.querySelectorAll('.tab');
    const tabPanes        = document.querySelectorAll('.tab-pane');

    const pdfViewer       = document.getElementById('pdfViewer');
    const downloadPdf     = document.getElementById('downloadPdf');
    const statsText       = document.getElementById('statsText');
    const downloadStats   = document.getElementById('downloadStats');
    const consoleLogs     = document.getElementById('consoleLogs');

    const progressOverlay = document.getElementById('progressOverlay');
    const progressMsg     = document.getElementById('progressMsg');
    const studyCountEl    = document.getElementById('studyCount');
    const totalSamplesEl  = document.getElementById('totalSamples');
    const studyCountDesc  = document.getElementById('studyCountDesc');

    const datatypeSelect  = document.getElementById('datatype');
    const endpointSelect  = document.getElementById('endpoint');
    const splitGroup      = document.getElementById('splitGroup');
    const cnaGroup        = document.getElementById('cnaGroup');

    // =========================================================
    // Cancer Type → Display Category Mapping
    // =========================================================
    const CATEGORY_MAP = {
        'Breast':               ['brca','mbc','acbc'],
        'Lung':                 ['luad','lusc','lung','nsclc','sclc'],
        'CNS / Brain':          ['gbm','lgg','difg','mbl','past','pcnsl','mng','brain'],
        'Bowel / Colorectal':   ['coadread','coad','read'],
        'Leukemia':             ['laml','aml','cll','all','mds','mpn','myeloid','mnm'],
        'Lymphoma':             ['dlbclnos','nhl','mcl','mbn','lymph','pcnsl'],
        'Prostate':             ['prad','prostate'],
        'Ovary':                ['ov','hgsoc','lgsoc','scco','ovary'],
        'Kidney':               ['kirc','kirp','kich','ccrcc','prcc','rcc','nccrcc','urcc'],
        'Liver / Biliary':      ['lihc','hcc','liad','hccihch','chol','ihch','biliary_tract'],
        'Esophagus / Stomach':  ['stad','esca','escc','egc','stes','stomach'],
        'Melanoma':             ['skcm','mel','desm','um'],
        'Sarcoma':              ['soft_tissue','sarc','lipo','es','rms','gist','stmyec','lms'],
        'Pancreas':             ['paad','paac','pact','pancreas','panet'],
        'Head & Neck':          ['hnsc','head_neck','npc','ohnca'],
        'Bladder / Urinary':    ['blca','utuc'],
        'Thyroid':              ['thpa','thyroid'],
        'Uterus / Cervix':      ['ucec','ucs','cesc','uccc','uec','usarc'],
        'Thymus / Pleura':      ['thym','tet','plmeso','meso'],
        'Testis':               ['tgct','nsgct','testis'],
        'Adrenal Gland':        ['acc','pcpg','mnet'],
        'Bone':                 ['bone','os'],
        'Eye':                  ['uvm','um'],
        'Pediatric':            ['nbl','wt','mrt','rbl','hdcn'],
        'Multiple Myeloma':     ['pcm'],
        'Skin (non-melanoma)':  ['cscc','skin','nfib'],
        'Nerve Sheath':         ['mpnst','schw','nst'],
        'Pituitary':            ['ptad'],
        'Mixed / Pan-Cancer':   ['mixed'],
    };

    // Special groups detected by study name/ID keywords
    const SPECIAL_GROUPS = [
        { label: 'PanCancer Studies',   test: s => /pan.?can|pancancer/i.test(s.studyId + s.name) || s.groups?.includes('PANCAN') },
        { label: 'Pediatric Studies',   test: s => /pediatr|target|childr/i.test(s.name) },
        { label: 'Cell Lines / PDX',    test: s => /cell.?line|ccle|pdx/i.test(s.name) },
        { label: 'Immunogenomics',      test: s => /immuno|immunother/i.test(s.name) },
    ];

    function getCategoryForStudy(study) {
        const id = (study.studyId || '').toLowerCase();
        const name = (study.name || '').toLowerCase();
        for (const [cat, types] of Object.entries(CATEGORY_MAP)) {
            for (const t of types) {
                if (id.includes(t) || id.startsWith(t)) return cat;
            }
        }
        for (const sg of SPECIAL_GROUPS) {
            if (sg.test(study)) return sg.label;
        }
        return 'Other';
    }

    // =========================================================
    // Load Studies
    // =========================================================
    let allStudies = [];
    let currentCategory = 'All';
    let currentQuickFilter = '';
    let currentSearchQuery = '';

    async function loadStudies() {
        studyHint.textContent = 'Loading studies…';
        try {
            const res = await fetch(`/api/cbio/studies?t=${Date.now()}`);
            if (!res.ok) throw new Error('fetch failed');
            allStudies = await res.json();

            // Assign category to each study
            allStudies.forEach(s => { s._category = getCategoryForStudy(s); });

            const total = allStudies.length;
            const samples = allStudies.reduce((a, s) => a + (s.allSampleCount || 0), 0);

            if (studyCountEl) studyCountEl.textContent = total.toLocaleString();
            if (totalSamplesEl) totalSamplesEl.textContent = (samples / 1000).toFixed(0) + 'K+';
            if (studyCountDesc) studyCountDesc.textContent = total + '+';
            studyHint.textContent = `${total} studies available — click to browse`;
            openBrowserBtn.disabled = false;
        } catch {
            studyHint.textContent = 'Could not load studies. Check your connection.';
        }
    }

    openBrowserBtn.disabled = true;
    loadStudies();

    // =========================================================
    // Study Browser Modal
    // =========================================================
    function openModal() {
        studyModal.classList.remove('hidden');
        document.body.style.overflow = 'hidden';
        buildSidebar();
        renderStudyList();
        modalSearchInput.focus();
    }

    function closeModal() {
        studyModal.classList.add('hidden');
        document.body.style.overflow = '';
        modalSearchInput.value = '';
        currentSearchQuery = '';
    }

    openBrowserBtn.addEventListener('click', openModal);
    closeModalBtn.addEventListener('click', closeModal);
    modalOverlay.addEventListener('click', closeModal);
    document.addEventListener('keydown', e => { if (e.key === 'Escape') closeModal(); });

    // Quick filter buttons
    document.querySelectorAll('.quick-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            document.querySelectorAll('.quick-btn').forEach(b => b.classList.remove('active'));
            btn.classList.add('active');
            currentQuickFilter = btn.dataset.filter;
            currentCategory = 'All';
            highlightSidebarItem('All');
            renderStudyList();
        });
    });

    // Modal search
    let searchTimer;
    modalSearchInput.addEventListener('input', () => {
        clearTimeout(searchTimer);
        searchTimer = setTimeout(() => {
            currentSearchQuery = modalSearchInput.value.trim().toLowerCase();
            currentCategory = 'All';
            highlightSidebarItem('All');
            renderStudyList();
        }, 150);
    });

    [filterRNA, filterMUT, filterCNA].forEach(cb => {
        if(cb) cb.addEventListener('change', () => {
            currentCategory = 'All';
            highlightSidebarItem('All');
            renderStudyList();
        });
    });

    function getFilteredStudies() {
        let list = allStudies;

        // Quick filter
        if (currentQuickFilter === 'tcga_pan_can_atlas') {
            list = list.filter(s => s.studyId.includes('tcga_pan_can_atlas'));
        } else if (currentQuickFilter === 'tcga') {
            list = list.filter(s => s.studyId.includes('tcga'));
        } else if (currentQuickFilter === 'msk') {
            list = list.filter(s => s.studyId.includes('msk') || s.studyId.includes('mskcc'));
        }

        // Category filter
        if (currentCategory !== 'All') {
            list = list.filter(s => s._category === currentCategory);
        }

        // Search filter
        if (currentSearchQuery) {
            list = list.filter(s =>
                s.studyId.toLowerCase().includes(currentSearchQuery) ||
                s.name.toLowerCase().includes(currentSearchQuery) ||
                (s.description || '').toLowerCase().includes(currentSearchQuery)
            );
        }

        // Data type filter
        if (filterRNA && filterRNA.checked) {
            list = list.filter(s => (s.mrnaRnaSeqV2SampleCount || 0) > 0 || (s.mrnaRnaSeqSampleCount || 0) > 0 || (s.mrnaMicroarraySampleCount || 0) > 0);
        }
        if (filterMUT && filterMUT.checked) {
            list = list.filter(s => (s.sequencedSampleCount || 0) > 0);
        }
        if (filterCNA && filterCNA.checked) {
            list = list.filter(s => (s.cnaSampleCount || 0) > 0);
        }

        return list;
    }

    function buildSidebar() {
        // Count studies per category
        const counts = {};
        allStudies.forEach(s => {
            counts[s._category] = (counts[s._category] || 0) + 1;
        });
        counts['All'] = allStudies.length;

        const sortedCats = ['All', ...Object.keys(counts).filter(c => c !== 'All').sort((a, b) => counts[b] - counts[a])];

        categorySidebar.innerHTML = sortedCats.map(cat => `
            <div class="sidebar-item ${cat === currentCategory ? 'active' : ''}" data-cat="${escHtml(cat)}">
                <span class="sidebar-item-label">${escHtml(cat)}</span>
                <span class="sidebar-item-count">${counts[cat] || 0}</span>
            </div>
        `).join('');

        categorySidebar.querySelectorAll('.sidebar-item').forEach(item => {
            item.addEventListener('click', () => {
                currentCategory = item.dataset.cat;
                currentSearchQuery = '';
                modalSearchInput.value = '';
                // Clear quick filter
                currentQuickFilter = '';
                document.querySelectorAll('.quick-btn').forEach(b => b.classList.remove('active'));
                highlightSidebarItem(currentCategory);
                renderStudyList();
            });
        });
    }

    function highlightSidebarItem(cat) {
        categorySidebar.querySelectorAll('.sidebar-item').forEach(item => {
            item.classList.toggle('active', item.dataset.cat === cat);
        });
    }

    function renderStudyList() {
        const filtered = getFilteredStudies();
        modalStudyCount.textContent = `${filtered.length.toLocaleString()} studies`;

        if (!filtered.length) {
            studyListPanel.innerHTML = '<p class="smodal-empty">No studies match your search.</p>';
            return;
        }

        // Group by category for display (unless filtered to a single category)
        let groups = {};
        if (currentCategory !== 'All') {
            groups[currentCategory] = filtered;
        } else {
            // Group alphabetically by category
            filtered.forEach(s => {
                if (!groups[s._category]) groups[s._category] = [];
                groups[s._category].push(s);
            });
        }

        const html = Object.entries(groups).sort(([a], [b]) => a.localeCompare(b)).map(([cat, studies]) => `
            <div class="smodal-group">
                <div class="smodal-group-header">${escHtml(cat)}<span class="smodal-group-count">${studies.length}</span></div>
                ${studies.map(s => `
                    <div class="smodal-study-row" data-id="${s.studyId}" data-name="${escHtml(s.name)}" data-samples="${s.allSampleCount || 0}">
                        <div class="smodal-study-info">
                            <span class="smodal-study-name">${escHtml(s.name)}</span>
                            <span class="smodal-study-id">${s.studyId}</span>
                        </div>
                        <span class="smodal-sample-badge">${(s.allSampleCount || 0).toLocaleString()} samples</span>
                    </div>
                `).join('')}
            </div>
        `).join('');

        studyListPanel.innerHTML = html;

        studyListPanel.querySelectorAll('.smodal-study-row').forEach(row => {
            row.addEventListener('click', () => {
                selectStudy(row.dataset.id, row.dataset.name, parseInt(row.dataset.samples));
            });
        });
    }

    function selectStudy(id, name, samples) {
        studyIdInput.value = id;
        studyBadgeText.textContent = `${name}  ·  ${(samples || 0).toLocaleString()} samples`;
        studyBadge.classList.remove('hidden');
        openBrowserBtn.classList.add('hidden');
        studyHint.classList.add('hidden');
        closeModal();
        loadProfiles(id);
    }

    clearStudyBtn.addEventListener('click', () => {
        studyIdInput.value = '';
        studyBadge.classList.add('hidden');
        openBrowserBtn.classList.remove('hidden');
        studyHint.classList.remove('hidden');
        resetProfiles();
    });

    // =========================================================
    // Load Profiles (dynamic by datatype)
    // =========================================================
    let currentProfiles = [];

    datatypeSelect.addEventListener('change', () => {
        splitGroup.classList.toggle('hidden', datatypeSelect.value !== 'mRNA');
        cnaGroup.classList.toggle('hidden', datatypeSelect.value !== 'CNA');
        renderProfiles();
    });

    function renderProfiles() {
        profileSelect.innerHTML = '<option value="">Auto-detect (recommended)</option>';
        profileHint.classList.add('hidden');
        if (!currentProfiles.length) return;

        const dt = datatypeSelect.value;
        let filtered = [];
        if (dt === 'mRNA') filtered = currentProfiles.filter(p => p.molecularAlterationType === 'MRNA_EXPRESSION');
        else if (dt === 'MUT') filtered = currentProfiles.filter(p => ['MUTATION_EXTENDED','MUTATION'].includes(p.molecularAlterationType));
        else if (dt === 'CNA') filtered = currentProfiles.filter(p => p.molecularAlterationType === 'COPY_NUMBER_ALTERATION');

        filtered.forEach(p => {
            const opt = document.createElement('option');
            opt.value = p.molecularProfileId;
            opt.textContent = p.name;
            profileSelect.appendChild(opt);
        });

        profileHint.textContent = filtered.length > 0
            ? `${filtered.length} profile${filtered.length > 1 ? 's' : ''} found`
            : `No ${dt} profiles found — auto-detection will try fallback`;
        profileHint.classList.remove('hidden');
    }

    async function loadProfiles(studyId) {
        profileSelect.innerHTML = '<option value="">Loading profiles…</option>';
        profileHint.classList.add('hidden');
        try {
            const res = await fetch(`/api/cbio/profiles?study_id=${encodeURIComponent(studyId)}`);
            currentProfiles = await res.json();
            renderProfiles();
        } catch {
            currentProfiles = [];
            profileSelect.innerHTML = '<option value="">Auto-detect (recommended)</option>';
            profileHint.textContent = 'Profile list unavailable — auto-detection will be used';
            profileHint.classList.remove('hidden');
        }
    }

    function resetProfiles() {
        currentProfiles = [];
        profileSelect.innerHTML = '<option value="">Auto-detect (recommended)</option>';
        profileHint.classList.add('hidden');
    }

    // =========================================================
    // Tab switching
    // =========================================================
    tabs.forEach(tab => {
        tab.addEventListener('click', () => {
            tabs.forEach(t => t.classList.remove('active'));
            tabPanes.forEach(p => p.classList.remove('active'));
            tab.classList.add('active');
            document.getElementById(tab.dataset.target).classList.add('active');
        });
    });

    // =========================================================
    // Progress cycling messages
    // =========================================================
    const progressMessages = [
        'Connecting to cBioPortal…',
        'Fetching molecular profiles…',
        'Resolving gene symbol…',
        'Downloading expression data…',
        'Fetching clinical survival data…',
        'Merging datasets…',
        'Fitting Kaplan-Meier model…',
        'Generating plot…',
    ];

    function cycleProgress() {
        let i = 0;
        return setInterval(() => {
            progressMsg.textContent = progressMessages[i % progressMessages.length];
            i++;
        }, 3500);
    }

    // =========================================================
    // Form Submit
    // =========================================================
    form.addEventListener('submit', async e => {
        e.preventDefault();

        const formData = new FormData(form);
        let studyId = formData.get('study_id');
        if (!studyId || !studyId.trim()) {
            openBrowserBtn.style.outline = '2px solid #ef4444';
            setTimeout(() => openBrowserBtn.style.outline = '', 1500);
            studyHint.textContent = '⚠ Please select a study first';
            studyHint.style.color = '#ef4444';
            return;
        }

        const geneVal = formData.get('gene');
        if (!geneVal || !geneVal.trim()) {
            document.getElementById('gene').focus();
            return;
        }

        // Build payload
        const payload = {
            download_cbio: true,
            study_id:      studyId.trim(),
            gene:          geneVal.trim().toUpperCase(),
            datatype:      datatypeSelect.value,
            endpoint:      endpointSelect.value,
            split_method:  formData.get('split_method') || 'median',
        };

        if (datatypeSelect.value === 'CNA') {
            payload.cna_mode = formData.get('cna_mode') || 'all';
        }

        const profileType = formData.get('profile_type');
        if (profileType && profileType.trim()) payload.profile_type = profileType.trim();

        // Show overlay
        progressMsg.textContent = progressMessages[0];
        progressOverlay.classList.remove('hidden');
        const progressInterval = cycleProgress();

        submitBtn.disabled = true;
        runBtnIcon.classList.add('hidden');
        runSpinner.classList.remove('hidden');
        runBtnText.textContent = 'Running…';
        consoleLogs.textContent = 'Starting analysis…\n';

        try {
            const response = await fetch('/api/analyze', {
                method:  'POST',
                headers: { 'Content-Type': 'application/json' },
                body:    JSON.stringify(payload),
            });

            const result = await response.json();
            if (result.stdout) consoleLogs.textContent += result.stdout + '\n';
            if (result.stderr) consoleLogs.textContent += 'STDERR:\n' + result.stderr + '\n';

            if (response.ok && result.status === 'success') {
                const plotUrl  = `/api/results/km_plot.pdf?t=${Date.now()}`;
                const statsUrl = `/api/results/survival_results.txt?t=${Date.now()}`;

                pdfViewer.src = plotUrl;
                downloadPdf.href = plotUrl;
                downloadStats.href = statsUrl;

                fetch(statsUrl).then(r => r.text()).then(txt => { statsText.textContent = txt; });

                emptyState.classList.add('hidden');
                resultsContent.classList.remove('hidden');
                tabs[0].click();
            } else {
                if (result.stderr && (result.stderr.includes('Only one group after stratification (Wild-type') || result.stderr.includes('No Mutation Found'))) {
                    alert('No Mutation Found');
                }
                consoleLogs.textContent += `\n❌ Error: ${result.error || 'Analysis failed'}\n`;
                tabs[2].click();
                emptyState.classList.remove('hidden');
                resultsContent.classList.add('hidden');
            }
        } catch (err) {
            consoleLogs.textContent += `\n❌ Network error: ${err.message}\n`;
        } finally {
            clearInterval(progressInterval);
            progressOverlay.classList.add('hidden');
            submitBtn.disabled = false;
            runBtnIcon.classList.remove('hidden');
            runSpinner.classList.add('hidden');
            runBtnText.textContent = 'Run Analysis';
        }
    });

    // =========================================================
    // Utility
    // =========================================================
    function escHtml(str) {
        return String(str || '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
    }
});
