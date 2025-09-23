(function () {
  const state = {
    apiBase: window.MFRM_CONFIG.apiBase,
    dataMode: 'paste',
    parsedData: [],
    rawText: '',
    fileData: null,
    columns: [],
    mapping: { person: '', response: '', facets: [] },
    jobId: null,
    pollTimer: null,
    result: null,
    template: null
  };

  const elements = {};

  const FALLBACK_SAMPLE_ROWS = [
    ['Person', 'Task', 'Rater', 'Criterion', 'Rating'],
    ['P01', 'Task1', 'Rater1', 'Content', '4'],
    ['P01', 'Task1', 'Rater2', 'Grammar', '3'],
    ['P01', 'Task2', 'Rater3', 'Vocabulary', '5'],
    ['P02', 'Task1', 'Rater1', 'Content', '2'],
    ['P02', 'Task2', 'Rater2', 'Grammar', '3'],
    ['P02', 'Task3', 'Rater3', 'Delivery', '1'],
    ['P03', 'Task1', 'Rater1', 'Content', '5'],
    ['P03', 'Task2', 'Rater2', 'Delivery', '4'],
    ['P03', 'Task3', 'Rater3', 'Vocabulary', '2'],
    ['P04', 'Task1', 'Rater1', 'Content', '3'],
    ['P04', 'Task2', 'Rater2', 'Grammar', '4'],
    ['P05', 'Task1', 'Rater3', 'Delivery', '5'],
    ['P05', 'Task2', 'Rater3', 'Vocabulary', ''],
    ['P06', 'Task3', 'Rater2', '', '3']
  ];

  const FALLBACK_TEMPLATE_TEXT = FALLBACK_SAMPLE_ROWS.map(row => row.join('\t')).join('\n');
  const FALLBACK_TEMPLATE_HEADER = FALLBACK_SAMPLE_ROWS[0].join('\t');

  function escapeCsvValue(value) {
    const needsQuote = /[",\n]/.test(value);
    const escaped = value.replace(/"/g, '""');
    return needsQuote ? `"${escaped}"` : escaped;
  }

  const FALLBACK_SAMPLE_CSV = FALLBACK_SAMPLE_ROWS
    .map(row => row.map(cell => escapeCsvValue(String(cell))).join(','))
    .join('\n');

  state.template = {
    columns: [...FALLBACK_SAMPLE_ROWS[0]],
    tab_text: FALLBACK_TEMPLATE_TEXT,
    header_text: FALLBACK_TEMPLATE_HEADER
  };

  function cacheElements() {
    elements.apiBaseInput = document.getElementById('apiBase');
    elements.saveApiBase = document.getElementById('saveApiBase');
    elements.dataModeRadios = document.querySelectorAll('input[name="dataMode"]');
    elements.pasteContainer = document.getElementById('pasteContainer');
    elements.uploadContainer = document.getElementById('uploadContainer');
    elements.pastedData = document.getElementById('pastedData');
    elements.singleRow = document.getElementById('singleRow');
    elements.appendRow = document.getElementById('appendRow');
    elements.clearRows = document.getElementById('clearRows');
    elements.fileInput = document.getElementById('fileInput');
    elements.downloadTSV = document.getElementById('downloadTSV');
    elements.downloadCSV = document.getElementById('downloadCSV');
    elements.hasHeader = document.getElementById('hasHeader');
    elements.separator = document.getElementById('separator');
    elements.loadData = document.getElementById('loadData');
    elements.personCol = document.getElementById('personCol');
    elements.responseCol = document.getElementById('responseCol');
    elements.facetCols = document.getElementById('facetCols');
    elements.linkFunction = document.getElementById('linkFunction');
    elements.threshold = document.getElementById('threshold');
    elements.runAnalysis = document.getElementById('runAnalysis');
    elements.statusMessage = document.getElementById('statusMessage');
    elements.tabBar = document.getElementById('tabBar');
    elements.tabPanels = document.querySelectorAll('.tab-panel');
    elements.pcaFacetSelect = document.getElementById('pcaFacetSelect');
    elements.downloadButtons = document.getElementById('downloadButtons');
  }

  function withTimeout(promise, ms, timeoutMessage) {
    return Promise.race([
      promise,
      new Promise((_, reject) => setTimeout(() => reject(new Error(timeoutMessage || 'timeout')), ms))
    ]);
  }

  function formatNetworkError(err, prefix) {
    const message = err && err.message ? err.message : String(err);
    const indicators = ['Failed to fetch', 'NetworkError', 'Load failed'];
    const isNetwork = indicators.some(ind => message.includes(ind));
    if (isNetwork) {
      return `${prefix}: Unable to reach the API. Check the endpoint URL and ensure the service is running.`;
    }
    if (message === 'timeout') {
      return `${prefix}: The API did not respond before timing out. Verify the deployment status.`;
    }
    return `${prefix}: ${message}`;
  }

  function verifyApiEndpoint(baseUrl) {
    const candidate = baseUrl.replace(/\/$/, '');
    return withTimeout(fetch(`${candidate}/health`, { method: 'GET' }), 6000, 'timeout')
      .then(res => {
        if (!res.ok) {
          throw new Error(`HTTP ${res.status}`);
        }
        return res.json();
      })
      .then(() => candidate);
  }

  function triggerFileDownload(content, format, filenamePrefix = 'MFRM_sample') {
    const timestamp = new Date().toISOString().slice(0, 10);
    const extension = format.toLowerCase();
    const mime = extension === 'csv' ? 'text/csv' : 'text/tab-separated-values';
    const blob = new Blob([content], { type: `${mime};charset=utf-8;` });
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `${filenamePrefix}_${timestamp}.${extension}`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(url);
  }

  function init() {
    cacheElements();
    wireEvents();
    elements.apiBaseInput.value = state.apiBase;
    elements.pastedData.value = FALLBACK_TEMPLATE_TEXT;
    fetchTemplate();
    activateTab('preview');
  }

  function wireEvents() {
    const attemptApiSave = (value, { silent = false } = {}) => {
      const trimmed = (value || '').trim();
      if (!trimmed) {
        if (!silent) setStatus('Enter an API endpoint first.', 'error');
        return Promise.reject(new Error('empty-endpoint'));
      }
      if (!silent) setStatus('Checking API endpoint...', 'info');
      return verifyApiEndpoint(trimmed)
        .then(candidate => {
          if (candidate === state.apiBase) {
            if (!silent) {
              setStatus(`API endpoint is already set to ${candidate}.`, 'info');
            }
            return candidate;
          }
          state.apiBase = candidate;
          elements.apiBaseInput.value = candidate;
          localStorage.setItem('mfrm_api_base', state.apiBase);
          if (!silent) setStatus(`API endpoint set to ${state.apiBase}.`, 'success');
          fetchTemplate(true);
          return candidate;
        })
        .catch(err => {
          if (!silent) setStatus(formatNetworkError(err, 'Failed to verify API endpoint'), 'error');
          throw err;
        });
    };

    elements.saveApiBase?.addEventListener('click', () => {
      attemptApiSave(elements.apiBaseInput.value);
    });

    elements.apiBaseInput.addEventListener('keydown', event => {
      if (event.key === 'Enter') {
        event.preventDefault();
        attemptApiSave(elements.apiBaseInput.value);
      }
    });

    elements.apiBaseInput.addEventListener('blur', () => {
      const value = elements.apiBaseInput.value.trim();
      if (value && value !== state.apiBase) {
        attemptApiSave(value, { silent: true }).catch(() => {
          // Ignore silent failures; user will see the existing status message.
        });
      }
    });

    elements.dataModeRadios.forEach(radio => {
      radio.addEventListener('change', () => {
        state.dataMode = radio.value;
        if (state.dataMode === 'paste') {
          elements.pasteContainer.classList.remove('hidden');
          elements.uploadContainer.classList.add('hidden');
          elements.separator.value = '\\t';
        } else {
          elements.pasteContainer.classList.add('hidden');
          elements.uploadContainer.classList.remove('hidden');
          elements.separator.value = ',';
        }
      });
    });

    elements.appendRow.addEventListener('click', appendRowToTemplate);
    elements.clearRows.addEventListener('click', clearTemplate);
    elements.downloadTSV.addEventListener('click', () => downloadSample('tsv'));
    elements.downloadCSV.addEventListener('click', () => downloadSample('csv'));
    elements.loadData.addEventListener('click', loadData);
    elements.runAnalysis.addEventListener('click', runAnalysis);
    elements.tabBar.addEventListener('click', handleTabClick);
    elements.pcaFacetSelect.addEventListener('change', () => renderPCA());
    elements.downloadButtons.addEventListener('click', handleDownloadClick);
    elements.fileInput.addEventListener('change', handleFileUpload);
    elements.personCol.addEventListener('change', () => {
      state.mapping.person = elements.personCol.value;
      state.mapping.facets = state.mapping.facets.filter(facet => facet !== state.mapping.person);
      syncFacetControls();
      renderResponseDistribution();
    });
    elements.responseCol.addEventListener('change', () => {
      state.mapping.response = elements.responseCol.value;
      renderResponseDistribution();
    });
    elements.facetCols.addEventListener('change', handleFacetChange);
  }

  function fetchTemplate(isManual = false) {
    fetch(`${state.apiBase}/template`)
      .then(res => res.json())
      .then(data => {
        state.template = data;
        elements.pastedData.value = data.tab_text || '';
        if (isManual) {
          setStatus('Template updated.', 'success');
        }
      })
      .catch(err => {
        setStatus(formatNetworkError(err, 'Failed to fetch template'), 'error');
        if (!isManual) {
          state.template = {
            columns: [...FALLBACK_SAMPLE_ROWS[0]],
            tab_text: FALLBACK_TEMPLATE_TEXT,
            header_text: FALLBACK_TEMPLATE_HEADER
          };
          elements.pastedData.value = FALLBACK_TEMPLATE_TEXT;
        }
      });
  }

  function downloadSample(format) {
    const fallback = format === 'csv' ? FALLBACK_SAMPLE_CSV : FALLBACK_TEMPLATE_TEXT;
    withTimeout(fetch(`${state.apiBase}/template/sample?format=${format}`), 6000, 'timeout')
      .then(res => {
        if (!res.ok) {
          throw new Error(`HTTP ${res.status}`);
        }
        return res.text();
      })
      .then(text => {
        triggerFileDownload(text, format);
        setStatus('Sample dataset downloaded from the API.', 'success');
      })
      .catch(err => {
        if (fallback) {
          triggerFileDownload(fallback, format);
          setStatus(formatNetworkError(err, 'API sample download failed') + ' Using bundled sample instead.', 'info');
        } else {
          setStatus(formatNetworkError(err, 'Failed to download sample'), 'error');
        }
      });
  }

  function detectSeparatorFromText(text) {
    const line = text.split(/\n/).find(l => l.trim().length > 0) || '';
    if (line.includes('\t')) return '\t';
    if (line.includes(';')) return ';';
    if (line.includes(',')) return ',';
    return '\t';
  }

  function appendRowToTemplate() {
    const line = (elements.singleRow.value || '').replace(/\r/g, '').trim();
    if (!line) {
      setStatus('Enter a row to append.', 'error');
      return;
    }
    const separator = detectSeparatorFromText(line);
    const fields = line.split(separator).map(f => f.trim());
    if (!state.template) {
      setStatus('Template is not loaded yet.', 'error');
      return;
    }
    const expected = state.template.columns.length;
    while (fields.length < expected) {
      fields.push('');
    }
    if (fields.length > expected) {
      setStatus(`Too many columns provided (expected ${expected}).`, 'error');
      return;
    }
    const currentText = (elements.pastedData.value || '').trim();
    const updated = currentText ? `${currentText}\n${fields.join('\t')}` : fields.join('\t');
    elements.pastedData.value = updated;
    elements.singleRow.value = '';
    setStatus('Row appended.', 'success');
  }

  function clearTemplate() {
    if (!state.template) return;
    const header = state.template.header_text || '';
    elements.pastedData.value = header;
    elements.singleRow.value = '';
    setStatus('Template reset.', 'info');
  }

  function handleFileUpload(event) {
    const file = event.target.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = e => {
      state.fileData = e.target.result;
      setStatus(`Loaded file "${file.name}".`, 'info');
    };
    reader.readAsText(file);
  }

  function loadData() {
    let raw = '';
    if (state.dataMode === 'paste') {
      raw = (elements.pastedData.value || '').trim();
      if (!raw) {
        setStatus('Paste some data first.', 'error');
        return;
      }
    } else {
      raw = state.fileData;
      if (!raw) {
        setStatus('Choose a file to upload.', 'error');
        return;
      }
    }

    const separator = elements.separator.value || detectSeparatorFromText(raw);
    const hasHeader = elements.hasHeader.checked;

    const result = Papa.parse(raw, {
      delimiter: separator,
      header: hasHeader,
      skipEmptyLines: 'greedy',
      transformHeader: h => h.trim(),
      transform: value => value === undefined ? '' : String(value).trim()
    });

    if (result.errors && result.errors.length > 0) {
      setStatus(`Parsing error: ${result.errors[0].message}`, 'error');
      return;
    }

    let data;
    if (hasHeader) {
      data = result.data;
    } else {
      const colCount = result.data[0] ? result.data[0].length : 0;
      const headers = Array.from({ length: colCount }, (_, idx) => `Column_${idx + 1}`);
      data = result.data.map(row => {
        const obj = {};
        headers.forEach((h, i) => {
          obj[h] = row[i];
        });
        return obj;
      });
    }

    if (!Array.isArray(data) || data.length === 0) {
      setStatus('No data could be read.', 'error');
      return;
    }

    state.parsedData = data;
    state.columns = Object.keys(data[0]);
    state.mapping.person = '';
    state.mapping.response = '';
    state.mapping.facets = [];
    updateColumnSelectors();
    renderPreview();
    setStatus(`Loaded ${data.length} rows and ${state.columns.length} columns.`, 'success');
  }

  function updateColumnSelectors() {
    const { columns } = state;
    const createOptions = (select, defaultValue) => {
      select.innerHTML = '<option value="">-- Select --</option>' + columns.map(col => `<option value="${col}">${col}</option>`).join('');
      if (defaultValue && columns.includes(defaultValue)) {
        select.value = defaultValue;
      }
    };

    const guessedPerson = guessColumn(columns, ['Person', 'person', 'ID']);
    state.mapping.person = state.mapping.person && columns.includes(state.mapping.person) ? state.mapping.person : guessedPerson;
    createOptions(elements.personCol, state.mapping.person);

    const guessedResponse = guessColumn(columns, ['Rating', 'Score', 'Response']);
    state.mapping.response = state.mapping.response && columns.includes(state.mapping.response) ? state.mapping.response : guessedResponse;
    createOptions(elements.responseCol, state.mapping.response);

    state.mapping.facets = state.mapping.facets
      .filter(col => columns.includes(col) && col !== state.mapping.person);
    if (!state.mapping.facets.length) {
      state.mapping.facets = guessFacetDefaults(columns, state.mapping.person);
    }

    elements.facetCols.innerHTML = columns.map(col => {
      const isPerson = col === state.mapping.person;
      const labelClass = isPerson ? ' class="disabled"' : '';
      const checked = state.mapping.facets.includes(col) && !isPerson ? 'checked' : '';
      const disabled = isPerson ? 'disabled' : '';
      const badge = isPerson ? '<span class="badge">Auto</span>' : '';
      return `<label${labelClass}><input type="checkbox" value="${col}" ${checked} ${disabled}/> ${col}${badge}</label>`;
    }).join('');

    syncFacetControls();
  }

  function guessColumn(columns, candidates) {
    return candidates.find(c => columns.includes(c)) || '';
  }

  function guessFacetDefaults(columns, personCol) {
    const preferred = ['Rater', 'Task', 'Criterion', 'Item', 'Domain', 'Facet', 'Judge'];
    return columns.filter(col => preferred.includes(col) && col !== personCol);
  }

  function syncFacetControls() {
    if (!elements.facetCols) return;
    const personCol = elements.personCol.value;
    const selected = new Set(state.mapping.facets);
    const checkboxes = elements.facetCols.querySelectorAll('input[type="checkbox"]');
    checkboxes.forEach(cb => {
      const label = cb.closest('label');
      if (cb.value === personCol) {
        cb.checked = false;
        cb.disabled = true;
        if (label) {
          label.classList.add('disabled');
        }
        selected.delete(cb.value);
      } else {
        cb.disabled = false;
        if (label) {
          label.classList.remove('disabled');
        }
        cb.checked = selected.has(cb.value);
      }
    });
    state.mapping.facets = Array.from(selected);
  }

  function handleFacetChange(event) {
    const target = event.target;
    if (!target || target.type !== 'checkbox') return;
    if (target.disabled) return;
    if (target.checked) {
      if (!state.mapping.facets.includes(target.value)) {
        state.mapping.facets.push(target.value);
      }
    } else {
      state.mapping.facets = state.mapping.facets.filter(facet => facet !== target.value);
    }
  }

  function renderPreview() {
    if (!state.parsedData.length) return;
    renderTable('dataPreviewTable', state.parsedData.slice(0, 50));
    const structure = state.columns.map(col => {
      const values = state.parsedData.map(row => row[col]);
      const unique = new Set(values.filter(v => v !== '' && v !== null));
      const naCount = values.filter(v => v === '' || v === null || v === undefined).length;
      return { Column: col, Missing: naCount, Unique: unique.size };
    });
    renderTable('dataStructureTable', structure);
    renderResponseDistribution();
  }

  function renderResponseDistribution() {
    if (!state.parsedData.length) return;
    const responseCol = elements.responseCol.value;
    if (!responseCol) return;
    const counts = {};
    state.parsedData.forEach(row => {
      const value = row[responseCol];
      counts[value] = (counts[value] || 0) + 1;
    });
    const categories = Object.keys(counts);
    const values = categories.map(cat => counts[cat]);
    const trace = {
      x: categories,
      y: values,
      type: 'bar',
      marker: { color: '#1f77b4' }
    };
    Plotly.newPlot('responsePlot', [trace], {
      margin: { t: 40, r: 20, l: 40, b: 40 },
      title: 'Response Distribution',
      xaxis: { title: responseCol },
      yaxis: { title: 'Frequency' }
    }, { displayModeBar: false });
  }

  function buildPayload() {
    if (!state.parsedData.length) {
      throw new Error('Load data before running the analysis.');
    }
    const person = elements.personCol.value;
    const response = elements.responseCol.value;
    if (!person || !response) {
      throw new Error('Select a person ID and rating column.');
    }
    const facets = Array.from(elements.facetCols.querySelectorAll('input[type="checkbox"]:checked')).map(cb => cb.value);
    if (!facets.length) {
      throw new Error('Select at least one facet column.');
    }
    state.mapping.facets = facets;
    return {
      data: state.parsedData,
      config: {
        person_col: person,
        response_col: response,
        facet_cols: facets
      },
      model_config: {
        link_function: elements.linkFunction.value,
        threshold: elements.threshold.value
      }
    };
  }

  function runAnalysis() {
    try {
      const payload = buildPayload();
      setStatus('Submitting analysis job...', 'info');
      elements.runAnalysis.disabled = true;
      fetch(`${state.apiBase}/jobs`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      })
        .then(res => {
          if (!res.ok) throw new Error(`HTTP ${res.status}`);
          return res.json();
        })
        .then(({ job_id }) => {
          state.jobId = job_id;
          setStatus(`Job ${job_id} accepted. Running analysis...`, 'info');
          pollJob(job_id);
        })
        .catch(err => {
          elements.runAnalysis.disabled = false;
          setStatus(formatNetworkError(err, 'Failed to submit job'), 'error');
        });
    } catch (err) {
      setStatus(err.message, 'error');
    }
  }

  function pollJob(jobId) {
    if (state.pollTimer) {
      clearInterval(state.pollTimer);
    }
    state.pollTimer = setInterval(() => {
      fetch(`${state.apiBase}/jobs/${jobId}`)
        .then(res => res.json())
        .then(info => {
          if (info.status === 'finished') {
            clearInterval(state.pollTimer);
            fetchJobResult(jobId);
          } else if (info.status === 'failed') {
            clearInterval(state.pollTimer);
            elements.runAnalysis.disabled = false;
            setStatus(`Job failed: ${info?.error?.message || 'Unknown error'}`, 'error');
          } else {
            setStatus(`Running... (${info.status})`, 'info');
          }
        })
        .catch(err => {
          clearInterval(state.pollTimer);
          elements.runAnalysis.disabled = false;
          setStatus(`Failed to fetch job status: ${err.message}`, 'error');
        });
    }, 5000);
  }

  function fetchJobResult(jobId) {
    fetch(`${state.apiBase}/jobs/${jobId}/result`)
      .then(res => res.json())
      .then(result => {
        state.result = result;
        elements.runAnalysis.disabled = false;
        setStatus('Analysis finished. Review the tabs for results.', 'success');
        renderAll();
        enableDownloadButtons();
      })
      .catch(err => {
        elements.runAnalysis.disabled = false;
        setStatus(`Failed to fetch results: ${err.message}`, 'error');
      });
  }

  function enableDownloadButtons() {
    Array.from(elements.downloadButtons.querySelectorAll('button')).forEach(btn => {
      btn.disabled = !state.result;
    });
  }

  function handleDownloadClick(event) {
    const button = event.target.closest('button[data-resource]');
    if (!button || button.disabled || !state.jobId) return;
    const resource = button.dataset.resource;
    fetch(`${state.apiBase}/jobs/${state.jobId}/download/${resource}`)
      .then(res => res.text())
      .then(csv => {
        const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
        const url = window.URL.createObjectURL(blob);
        const link = document.createElement('a');
        link.href = url;
        link.download = `MFRM_${resource}_${state.jobId}.csv`;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
        window.URL.revokeObjectURL(url);
      })
      .catch(() => setStatus('Failed to download CSV.', 'error'));
  }

  function renderAll() {
    if (!state.result) return;
    renderTable('dataPreviewTable', state.result.data_preview);
    renderTable('dataStructureTable', state.result.data_structure);
    renderResponsePlotFromResult();
    renderTable('modelSummaryTable', state.result.model_summary);
    renderTable('convergenceTable', state.result.convergence);
    renderTable('randomEffectsTable', state.result.random_effects_variance);
    renderFacetTables();
    renderTable('reliabilityTable', state.result.reliability_table);
    renderTable('interpretationGuide', state.result.interpretation_guide);
    renderReliabilityPlot();
    renderTable('fitStatisticsTable', state.result.fit_statistics);
    renderFitPlots();
    populatePCASelector();
    renderPCA();
    renderTable('thresholdTable', state.result.thresholds);
    renderProbabilityCurves();
    renderResidualPlots();
    renderWrightMap();
  }

  function renderTable(containerId, data) {
    const container = document.getElementById(containerId);
    if (!container) return;
    if (!data || !data.length) {
      container.innerHTML = '<p class="hint">No data available.</p>';
      return;
    }
    const columns = Object.keys(data[0]);
    const header = `<thead><tr>${columns.map(col => `<th>${escapeHtml(col)}</th>`).join('')}</tr></thead>`;
    const body = `<tbody>${data.map(row => `<tr>${columns.map(col => `<td>${escapeHtml(formatCell(row[col]))}</td>`).join('')}</tr>`).join('')}</tbody>`;
    container.innerHTML = `<table>${header}${body}</table>`;
  }

  function escapeHtml(value) {
    if (value === null || value === undefined) return '';
    return String(value).replace(/[&<>]/g, ch => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' }[ch]));
  }

  function formatCell(value) {
    if (typeof value === 'number') {
      if (Number.isInteger(value)) return value;
      return value.toFixed(3);
    }
    return value;
  }

  function renderResponsePlotFromResult() {
    const data = state.result.response_distribution;
    if (!data || !data.length) return;
    const trace = {
      x: data.map(row => row.Response),
      y: data.map(row => row.Count),
      type: 'bar',
      marker: { color: '#1f77b4' }
    };
    Plotly.newPlot('responsePlot', [trace], {
      margin: { t: 40, r: 20, l: 40, b: 40 },
      title: 'Response Distribution',
      xaxis: { title: 'Category' },
      yaxis: { title: 'Frequency' }
    }, { displayModeBar: false });
  }

  function renderFacetTables() {
    const container = document.getElementById('facetTables');
    const data = state.result.facet_parameters;
    if (!data || !data.length) {
      container.innerHTML = '<p class="hint">Facet estimates are not available.</p>';
      return;
    }
    const groups = groupBy(data, row => row.Facet);
    container.innerHTML = Object.keys(groups).map(facet => {
      const tableId = `facet-${facet}`;
      setTimeout(() => renderTable(tableId, groups[facet]), 0);
      return `<div class="facet-block"><h3>${escapeHtml(facet)}</h3><div id="${tableId}" class="table-container"></div></div>`;
    }).join('');
  }

  function renderReliabilityPlot() {
    const data = state.result.reliability_table;
    if (!data || !data.length) return;
    const metrics = ['Reliability_Mean', 'Separation_Mean', 'Strata_Mean'];
    const traces = metrics.map(metric => ({
      x: data.map(row => row.Facet),
      y: data.map(row => row[metric]),
      name: metric.replace('_Mean', ''),
      type: 'bar'
    }));
    Plotly.newPlot('reliabilityPlot', traces, {
      barmode: 'group',
      legend: { orientation: 'h' },
      margin: { t: 40, r: 20, l: 40, b: 80 }
    }, { displayModeBar: false });
  }

  function renderFitPlots() {
    const msq = state.result.fit_scatter?.msq || [];
    const zstd = state.result.fit_scatter?.zstd || [];
    if (msq.length) {
      Plotly.newPlot('msqPlot', [{
        x: msq.map(row => row.Value),
        y: msq.map(row => row.DisplayLabel),
        mode: 'markers',
        type: 'scatter',
        marker: { color: msq.map(row => row.Statistic === 'Infit MSQ' ? '#1f77b4' : '#ff7f0e'), size: 10 },
        text: msq.map(row => `${row.DisplayLabel} (${row.Statistic}): ${row.Value.toFixed(2)}`),
        hoverinfo: 'text'
      }], {
        margin: { t: 30, r: 20, l: 150, b: 50 },
        xaxis: { title: 'Mean-square' },
        yaxis: { title: '' }
      }, { displayModeBar: false });
    }
    if (zstd.length) {
      Plotly.newPlot('zstdPlot', [{
        x: zstd.map(row => row.InfitZSTD),
        y: zstd.map(row => row.OutfitZSTD),
        mode: 'markers',
        type: 'scatter',
        marker: { color: '#2a9d8f', size: zstd.map(row => row.PointSize || 9) },
        text: zstd.map(row => `${row.DisplayLabel}\nInfit:${formatNumber(row.InfitZSTD)} Outfit:${formatNumber(row.OutfitZSTD)}`),
        hoverinfo: 'text'
      }], {
        xaxis: { title: 'Infit ZSTD' },
        yaxis: { title: 'Outfit ZSTD' },
        margin: { t: 30, r: 20, l: 50, b: 50 }
      }, { displayModeBar: false });
    }
  }

  function populatePCASelector() {
    const facets = state.result.metadata?.facets || [];
    const select = elements.pcaFacetSelect;
    const options = ['overall', ...facets];
    select.innerHTML = options.map(opt => `<option value="${opt}">${opt === 'overall' ? 'Overall' : opt}</option>`).join('');
  }

  function renderPCA() {
    if (!state.result) return;
    const selection = elements.pcaFacetSelect.value || 'overall';
    const bundle = selection === 'overall' ? state.result.pca_overall : (state.result.pca_by_facet?.[selection] || {});
    renderTable('pcaEigenTable', bundle.eigen || []);
    const loadings = bundle.loadings || [];
    if (!loadings.length) {
      document.getElementById('pcaLoadingsPlot').innerHTML = '';
      return;
    }
    const sorted = loadings
      .map(row => ({ Item: row.Item, Loading: row.PC1 }))
      .sort((a, b) => Math.abs(b.Loading) - Math.abs(a.Loading))
      .slice(0, 20);
    Plotly.newPlot('pcaLoadingsPlot', [{
      x: sorted.map(row => row.Loading),
      y: sorted.map(row => row.Item),
      type: 'bar',
      orientation: 'h',
      marker: { color: sorted.map(row => row.Loading > 0 ? '#1f77b4' : '#e76f51') }
    }], {
      margin: { t: 30, r: 20, l: 200, b: 40 },
      xaxis: { title: 'Loading (PC1)' }
    }, { displayModeBar: false });
  }

  function renderProbabilityCurves() {
    const data = state.result.probability_curves;
    if (!data || !data.length) return;
    const groups = groupBy(data, row => row.Category);
    const traces = Object.keys(groups).map(cat => ({
      x: groups[cat].map(row => row.Ability),
      y: groups[cat].map(row => row.Probability),
      type: 'scatter',
      mode: 'lines',
      name: `Category ${cat}`
    }));
    Plotly.newPlot('probabilityPlot', traces, {
      margin: { t: 40, r: 20, l: 50, b: 50 },
      xaxis: { title: 'Ability' },
      yaxis: { title: 'Probability', range: [0, 1] }
    }, { displayModeBar: false });
  }

  function renderResidualPlots() {
    const residuals = state.result.residuals?.residuals || [];
    if (!residuals.length) return;
    Plotly.newPlot('residualScatter', [{
      x: residuals.map(r => r.Fitted),
      y: residuals.map(r => r.Residuals),
      mode: 'markers',
      type: 'scatter',
      marker: { color: '#1f77b4', opacity: 0.6 }
    }, {
      x: [Math.min(...residuals.map(r => r.Fitted)), Math.max(...residuals.map(r => r.Fitted))],
      y: [0, 0],
      mode: 'lines',
      line: { color: '#e63946', dash: 'dash' }
    }], {
      margin: { t: 30, r: 20, l: 60, b: 50 },
      xaxis: { title: 'Fitted' },
      yaxis: { title: 'Residuals' }
    }, { displayModeBar: false });

    const qq = state.result.residuals.qq || [];
    if (qq.length) {
      Plotly.newPlot('qqPlot', [{
        x: qq.map(r => r.Theoretical),
        y: qq.map(r => r.Sample),
        mode: 'markers',
        type: 'scatter',
        marker: { color: '#2a9d8f' }
      }, {
        x: qq.map(r => r.Theoretical),
        y: qq.map(r => r.Theoretical),
        mode: 'lines',
        line: { color: '#e63946' }
      }], {
        margin: { t: 30, r: 20, l: 60, b: 50 },
        xaxis: { title: 'Theoretical' },
        yaxis: { title: 'Sample' }
      }, { displayModeBar: false });
    }

    const density = state.result.residuals.density || [];
    if (density.length) {
      Plotly.newPlot('residualDensity', [{
        x: density.map(r => r.x),
        y: density.map(r => r.y),
        type: 'scatter',
        mode: 'lines',
        line: { color: '#1f77b4' }
      }], {
        margin: { t: 30, r: 20, l: 60, b: 50 },
        xaxis: { title: 'Std Residuals' },
        yaxis: { title: 'Count (approx)' }
      }, { displayModeBar: false });
    }

    Plotly.newPlot('observedFitted', [{
      x: residuals.map(r => r.Observed),
      y: residuals.map(r => r.Fitted),
      mode: 'markers',
      type: 'scatter',
      marker: { color: '#264653', opacity: 0.6 }
    }, {
      x: residuals.map(r => r.Observed),
      y: residuals.map(r => r.Observed),
      mode: 'lines',
      line: { color: '#e76f51', dash: 'dash' }
    }], {
      margin: { t: 30, r: 20, l: 60, b: 50 },
      xaxis: { title: 'Observed' },
      yaxis: { title: 'Fitted' }
    }, { displayModeBar: false });
  }

  function renderWrightMap() {
    const map = state.result.wright_map;
    if (!map) return;
    const ability = map.ability_draws || [];
    const points = map.point_data || [];
    const traces = [];
    if (ability.length) {
      traces.push({
        x: ability.map(row => row.Display),
        type: 'histogram',
        opacity: 0.4,
        name: 'Persons',
        marker: { color: '#90caf9' }
      });
    }
    if (points.length) {
      traces.push({
        x: points.map(row => row.Mean),
        y: points.map(row => row.label),
        error_x: {
          type: 'data',
          symmetric: false,
          array: points.map(row => row.Upper66 - row.Mean),
          arrayminus: points.map(row => row.Mean - row.Lower66)
        },
        mode: 'markers',
        type: 'scatter',
        name: 'Facets',
        marker: { color: '#1f3a5f', size: 8 }
      });
    }
    if (!traces.length) return;
    Plotly.newPlot('wrightMap', traces, {
      margin: { t: 30, r: 20, l: 160, b: 50 },
      xaxis: { title: 'Latent Scale' },
      yaxis: { title: '' }
    }, { displayModeBar: false });
  }

  function handleTabClick(event) {
    const button = event.target.closest('button[data-tab]');
    if (!button) return;
    activateTab(button.dataset.tab);
  }

  function activateTab(tabId) {
    elements.tabBar.querySelectorAll('button').forEach(btn => {
      btn.classList.toggle('active', btn.dataset.tab === tabId);
    });
    elements.tabPanels.forEach(panel => {
      panel.classList.toggle('active', panel.id === `tab-${tabId}`);
    });
  }

  function setStatus(message, level = 'info') {
    elements.statusMessage.textContent = message || '';
    elements.statusMessage.dataset.level = level;
  }

  function groupBy(array, keySelector) {
    return array.reduce((acc, item) => {
      const key = keySelector(item);
      acc[key] = acc[key] || [];
      acc[key].push(item);
      return acc;
    }, {});
  }

  function formatNumber(value) {
    if (value === null || value === undefined) return '';
    return Number(value).toFixed(2);
  }

  document.addEventListener('DOMContentLoaded', init);
})();
