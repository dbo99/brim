function(el, x, data) {
  'use strict';

  var bundle = data && data.bundle ? data.bundle : null;
  var assets = data && data.assets ? data.assets : {};
  if (!bundle || !Array.isArray(bundle.products)) {
    throw new Error('BRIM Guide payload is missing or invalid.');
  }

  var oldRoot = document.getElementById('brim-guide-root');
  if (oldRoot) oldRoot.remove();

  var state = {
    section: 'explore',
    view: 'landing',
    query: '',
    browseDimension: '',
    browseValue: '',
    selectedId: '',
    previousFocus: null,
    history: []
  };

  var resourcesById = {};
  bundle.resources.forEach(function(resource) { resourcesById[resource.id] = resource; });
  var records = bundle.products.concat(bundle.articles, bundle.resources, bundle.updates);
  var recordsById = {};
  records.forEach(function(record) { recordsById[record.id] = record; });

  function node(tag, className, text) {
    var result = document.createElement(tag);
    if (className) result.className = className;
    if (text !== undefined && text !== null) result.textContent = String(text);
    return result;
  }

  function asArray(value) {
    if (Array.isArray(value)) return value;
    if (value === undefined || value === null || value === '') return [];
    return [value];
  }

  function button(className, text, action, label) {
    var result = node('button', className, text);
    result.type = 'button';
    if (action) result.setAttribute('data-guide-action', action);
    if (label) result.setAttribute('aria-label', label);
    return result;
  }

  function normalize(value) {
    return String(value || '')
      .normalize('NFKD')
      .replace(/[\u0300-\u036f]/g, '')
      .toLowerCase()
      .replace(/&/g, ' and ')
      .replace(/[^a-z0-9]+/g, ' ')
      .trim()
      .replace(/\s+/g, ' ');
  }

  function words(value) {
    var text = normalize(value);
    return text ? text.split(' ') : [];
  }

  function editDistanceAtMostOne(a, b) {
    if (a === b) return true;
    if (Math.abs(a.length - b.length) > 1) return false;
    var i = 0;
    var j = 0;
    var edits = 0;
    while (i < a.length && j < b.length) {
      if (a[i] === b[j]) {
        i += 1;
        j += 1;
      } else {
        edits += 1;
        if (edits > 1) return false;
        if (a.length > b.length) i += 1;
        else if (b.length > a.length) j += 1;
        else {
          i += 1;
          j += 1;
        }
      }
    }
    if (i < a.length || j < b.length) edits += 1;
    return edits <= 1;
  }

  function maxFieldScore(query, values, exact, prefix, contains) {
    var score = 0;
    values.forEach(function(value) {
      var candidate = normalize(value);
      if (!candidate) return;
      if (candidate === query) score = Math.max(score, exact);
      else if (candidate.indexOf(query) === 0) score = Math.max(score, prefix);
      else if (candidate.indexOf(query) >= 0) score = Math.max(score, contains);
    });
    return score;
  }

  function scoreRecord(record, rawQuery) {
    var query = normalize(rawQuery);
    if (!query) return 0;
    var title = normalize(record.title);
    var aliases = asArray(record.aliases);
    var path = asArray(record.path);
    var searchTerms = asArray(record.searchTerms);
    var pathGroups = path.length > 2 ? path.slice(1, -1) : path.slice(0, -1);
    var score = 0;

    score = Math.max(score, maxFieldScore(query, [record.title], 1200, 1000, 520));
    score = Math.max(score, maxFieldScore(query, aliases, 1100, 900, 500));
    score = Math.max(score, maxFieldScore(query, path, 800, 720, 460));
    score = Math.max(score, maxFieldScore(query, pathGroups, 760, 700, 440));
    score = Math.max(score, maxFieldScore(query, [record.subject], 650, 620, 410));
    score = Math.max(score, maxFieldScore(query, [record.mode], 600, 570, 390));
    score = Math.max(score, maxFieldScore(query, [record.family], 550, 520, 360));
    score = Math.max(score, maxFieldScore(query, [record.provider].concat(searchTerms), 420, 390, 260));
    score = Math.max(score, maxFieldScore(query, [record.summary], 250, 220, 150));

    asArray(record.relatedResourceIds).forEach(function(resourceId) {
      var resource = resourcesById[resourceId];
      if (resource) score = Math.max(score, maxFieldScore(query, [resource.title], 120, 110, 90));
    });

    var queryWords = words(query);
    if (queryWords.length > 1) {
      var searchable = normalize([
        record.title,
        aliases.join(' '),
        path.join(' '),
        record.subject,
        record.mode,
        record.family,
        record.provider,
        searchTerms.join(' '),
        record.summary
      ].join(' '));
      if (queryWords.every(function(token) { return searchable.indexOf(token) >= 0; })) {
        score = Math.max(score, 340 + queryWords.length);
      }
    }

    if (!score && queryWords.length === 1 && query.length >= 5) {
      var typoCandidates = words(title + ' ' + aliases.join(' '));
      if (typoCandidates.some(function(candidate) {
        return candidate.length >= 5 && editDistanceAtMostOne(query, candidate);
      })) score = 80;
    }
    return score;
  }

  function search(rawQuery) {
    var query = normalize(rawQuery);
    if (!query) return [];
    return records
      .map(function(record, index) {
        return { record: record, score: scoreRecord(record, query), sourceOrder: index };
      })
      .filter(function(item) { return item.score > 0; })
      .sort(function(a, b) {
        return b.score - a.score || a.sourceOrder - b.sourceOrder || a.record.title.localeCompare(b.record.title);
      })
      .slice(0, 60)
      .map(function(item) { return item.record; });
  }

  var root = node('div', 'brim-guide');
  root.id = 'brim-guide-root';
  root.hidden = true;
  root.setAttribute('aria-hidden', 'true');
  root.style.setProperty('--brim-guide-topo', 'url("' + String(assets.topo || '') + '")');

  var shell = node('section', 'brim-guide__shell');
  shell.setAttribute('role', 'dialog');
  shell.setAttribute('aria-modal', 'true');
  shell.setAttribute('aria-labelledby', 'brim-guide-title');

  var header = node('header', 'brim-guide__header');
  var leftClose = button('brim-guide__close brim-guide__close--left', '×', 'close', 'Close BRIM Guide');
  var home = button('brim-guide__home', '', 'home', 'BRIM Guide Home');
  var homeEyebrow = node('span', 'brim-guide__home-eyebrow', bundle.identity.guide_name);
  var homeName = node('span', 'brim-guide__home-name', bundle.identity.expanded_name);
  home.appendChild(homeEyebrow);
  home.appendChild(homeName);
  var marks = node('div', 'brim-guide__marks');
  var doiLink = node('a', 'brim-guide__mark');
  doiLink.href = 'https://www.doi.gov/';
  doiLink.target = '_blank';
  doiLink.rel = 'noopener noreferrer';
  doiLink.setAttribute('aria-label', 'U.S. Department of the Interior');
  var doiImg = node('img');
  doiImg.src = assets.doi || '';
  doiImg.alt = 'U.S. Department of the Interior';
  doiLink.appendChild(doiImg);
  var blmLink = node('a', 'brim-guide__mark');
  blmLink.href = 'https://www.blm.gov/california';
  blmLink.target = '_blank';
  blmLink.rel = 'noopener noreferrer';
  blmLink.setAttribute('aria-label', 'Bureau of Land Management California');
  var blmImg = node('img');
  blmImg.src = assets.blm || '';
  blmImg.alt = 'Bureau of Land Management';
  blmLink.appendChild(blmImg);
  marks.appendChild(doiLink);
  marks.appendChild(blmLink);
  var rightClose = button('brim-guide__close brim-guide__close--right', '×', 'close', 'Close BRIM Guide');
  header.appendChild(leftClose);
  header.appendChild(home);
  header.appendChild(marks);
  header.appendChild(rightClose);

  var nav = node('nav', 'brim-guide__nav');
  nav.setAttribute('aria-label', 'BRIM Guide sections');
  [
    ['explore', 'A', 'Explore'],
    ['methods', 'B', 'Methods & Guides'],
    ['resources', 'C', 'Resources'],
    ['updates', 'D', 'Updates']
  ].forEach(function(definition) {
    var item = button('brim-guide__nav-item', '', 'section');
    item.setAttribute('data-guide-section', definition[0]);
    item.appendChild(node('span', 'brim-guide__nav-letter', definition[1]));
    item.appendChild(node('span', 'brim-guide__nav-label', definition[2]));
    nav.appendChild(item);
  });

  var main = node('main', 'brim-guide__main');
  main.id = 'brim-guide-main';
  main.tabIndex = -1;

  var footer = node('footer', 'brim-guide__footer');
  var footerStatus = node('p', 'brim-guide__footer-status', 'All included visible Products receive a basic Guide entry. Rich documentation remains selective.');
  var aboutButton = button('brim-guide__text-button', 'About BRIM', 'about');
  footer.appendChild(footerStatus);
  footer.appendChild(aboutButton);

  shell.appendChild(header);
  shell.appendChild(nav);
  shell.appendChild(main);
  shell.appendChild(footer);
  root.appendChild(shell);
  document.body.appendChild(root);

  function snapshot() {
    return {
      section: state.section,
      view: state.view,
      query: state.query,
      browseDimension: state.browseDimension,
      browseValue: state.browseValue,
      selectedId: state.selectedId,
      scrollTop: main.scrollTop
    };
  }

  function restore(saved) {
    Object.keys(saved).forEach(function(key) {
      if (key !== 'scrollTop') state[key] = saved[key];
    });
    render();
    main.scrollTop = saved.scrollTop || 0;
  }

  function pushHistory() {
    state.history.push(snapshot());
    if (state.history.length > 30) state.history.shift();
  }

  function activeNav() {
    nav.querySelectorAll('[data-guide-section]').forEach(function(item) {
      var active = item.getAttribute('data-guide-section') === state.section;
      item.classList.toggle('is-active', active);
      if (active) item.setAttribute('aria-current', 'page');
      else item.removeAttribute('aria-current');
    });
  }

  function heading(eyebrow, title, description) {
    var wrap = node('div', 'brim-guide__page-heading');
    wrap.appendChild(node('p', 'brim-guide__eyebrow', eyebrow));
    var h1 = node('h1', '', title);
    h1.id = 'brim-guide-title';
    wrap.appendChild(h1);
    if (description) wrap.appendChild(node('p', 'brim-guide__lede', description));
    return wrap;
  }

  function recordType(record) {
    return record.kind || 'Product';
  }

  function resultCard(record) {
    var card = button('brim-guide__result', '', 'record');
    card.setAttribute('data-guide-record', record.id);
    card.appendChild(node('span', 'brim-guide__result-kind', recordType(record)));
    card.appendChild(node('strong', 'brim-guide__result-title', record.title));
    var context = record.kind === 'Product' ? record.pathLabel : (record.section || record.provider || record.date || 'BRIM Guide');
    card.appendChild(node('span', 'brim-guide__result-path', context));
    if (record.summary) card.appendChild(node('span', 'brim-guide__result-summary', record.summary));
    return card;
  }

  function renderResults(recordsToRender, emptyMessage) {
    var list = node('div', 'brim-guide__results');
    if (!recordsToRender.length) {
      list.appendChild(node('p', 'brim-guide__empty', emptyMessage || 'No Guide records match this view.'));
      return list;
    }
    recordsToRender.forEach(function(record) { list.appendChild(resultCard(record)); });
    return list;
  }

  function renderQuickAccess() {
    var section = node('section', 'brim-guide__quick');
    section.appendChild(node('p', 'brim-guide__eyebrow', 'Quick Access'));
    var grid = node('div', 'brim-guide__quick-grid');
    bundle.quickAccess.forEach(function(item) {
      var quick = button('brim-guide__quick-item', item.label, 'quick');
      quick.setAttribute('data-guide-quick', item.id);
      quick.setAttribute('data-guide-products', asArray(item.productIds).join(','));
      grid.appendChild(quick);
    });
    section.appendChild(grid);
    return section;
  }

  function renderExplore() {
    var fragment = document.createDocumentFragment();
    fragment.appendChild(heading(
      'A · Explore',
      bundle.identity.guide_name,
      bundle.identity.description
    ));

    var searchWrap = node('div', 'brim-guide__search-wrap');
    var label = node('label', 'brim-guide__search-label', 'Search Products, methods, resources, and updates');
    label.htmlFor = 'brim-guide-search';
    var searchInput = node('input', 'brim-guide__search');
    searchInput.type = 'search';
    searchInput.id = 'brim-guide-search';
    searchInput.autocomplete = 'off';
    searchInput.placeholder = 'Try HUC8, MRMS, groundwater, or an exact BRIM path';
    searchInput.value = state.query;
    searchInput.setAttribute('data-guide-search', 'true');
    searchWrap.appendChild(label);
    searchWrap.appendChild(searchInput);
    fragment.appendChild(searchWrap);

    if (state.query) {
      var searchResults = search(state.query);
      var searchHeader = node('div', 'brim-guide__section-heading');
      searchHeader.appendChild(node('h2', '', 'Search results'));
      searchHeader.appendChild(node('span', 'brim-guide__count', searchResults.length + ' shown'));
      fragment.appendChild(searchHeader);
      fragment.appendChild(renderResults(searchResults, 'No Guide records match that search. Try a shorter source-supported term.'));
      return fragment;
    }

    if (state.browseDimension && state.browseValue) {
      var key = state.browseDimension === 'subject' ? 'subject' : 'mode';
      var filtered = bundle.products.filter(function(product) { return product[key] === state.browseValue; });
      var browseHeading = node('div', 'brim-guide__section-heading');
      var backBrowse = button('brim-guide__text-button', '← All browse options', 'browse-clear');
      browseHeading.appendChild(node('h2', '', state.browseValue));
      browseHeading.appendChild(backBrowse);
      fragment.appendChild(browseHeading);
      fragment.appendChild(renderResults(filtered));
      return fragment;
    }

    fragment.appendChild(renderQuickAccess());
    var browse = node('section', 'brim-guide__browse');
    browse.appendChild(node('p', 'brim-guide__eyebrow', 'Browse'));
    var browseColumns = node('div', 'brim-guide__browse-columns');
    [
      ['subject', 'Primary subject'],
      ['mode', 'Data / guidance mode']
    ].forEach(function(definition) {
      var column = node('div', 'brim-guide__browse-column');
      column.appendChild(node('h2', '', definition[1]));
      var values = Array.from(new Set(bundle.products.map(function(product) { return product[definition[0]]; }))).sort();
      values.forEach(function(value) {
        var count = bundle.products.filter(function(product) { return product[definition[0]] === value; }).length;
        var browseButton = button('brim-guide__browse-item', '', 'browse');
        browseButton.setAttribute('data-guide-dimension', definition[0]);
        browseButton.setAttribute('data-guide-value', value);
        browseButton.appendChild(node('span', '', value));
        browseButton.appendChild(node('span', 'brim-guide__browse-count', count));
        column.appendChild(browseButton);
      });
      browseColumns.appendChild(column);
    });
    browse.appendChild(browseColumns);
    fragment.appendChild(browse);
    return fragment;
  }

  function detailRow(label, value) {
    if (!value) return null;
    var row = node('div', 'brim-guide__detail-row');
    row.appendChild(node('dt', '', label));
    row.appendChild(node('dd', '', value));
    return row;
  }

  function renderProductDetail(product) {
    var fragment = document.createDocumentFragment();
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(heading(product.subsystem, product.title, product.summary));
    var layout = node('div', 'brim-guide__detail-layout');
    var primary = node('article', 'brim-guide__detail-primary');
    primary.appendChild(node('p', 'brim-guide__eyebrow', 'Exact BRIM path'));
    primary.appendChild(node('p', 'brim-guide__path', product.pathLabel));
    var actionNotice = node('div', 'brim-guide__action-notice');
    actionNotice.appendChild(node('strong', '', 'Map actions arrive in Guide I2.'));
    actionNotice.appendChild(node('p', '', 'For now, use the exact path above in the existing BRIM map controls. This Guide does not turn layers on or change map state.'));
    primary.appendChild(actionNotice);
    var relatedResourceIds = asArray(product.relatedResourceIds);
    if (relatedResourceIds.length) {
      primary.appendChild(node('h2', '', 'Related resources'));
      var links = node('div', 'brim-guide__related');
      relatedResourceIds.forEach(function(resourceId) {
        var resource = resourcesById[resourceId];
        if (!resource) return;
        var related = button('brim-guide__related-item', resource.title, 'record');
        related.setAttribute('data-guide-record', resource.id);
        links.appendChild(related);
      });
      primary.appendChild(links);
    }
    var aside = node('aside', 'brim-guide__detail-aside');
    var dl = node('dl', 'brim-guide__detail-list');
    [
      ['Stable Product ID', product.id],
      ['Subsystem', product.subsystem],
      ['Provider / program', product.provider],
      ['Primary subject', product.subject],
      ['Data / guidance mode', product.mode],
      ['Product family', product.family],
      ['Implementation note', product.customOrNonGeneric ? 'Custom or non-generic presentation' : 'Basic Guide coverage']
    ].forEach(function(definition) {
      var row = detailRow(definition[0], definition[1]);
      if (row) dl.appendChild(row);
    });
    aside.appendChild(dl);
    layout.appendChild(primary);
    layout.appendChild(aside);
    fragment.appendChild(layout);
    return fragment;
  }

  function renderRecordDetail(record) {
    if (record.kind === 'Product') return renderProductDetail(record);
    var fragment = document.createDocumentFragment();
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(heading(record.kind, record.title, record.summary));
    var card = node('article', 'brim-guide__prose-card');
    if (record.action === 'open_legacy_notes') {
      card.appendChild(node('p', '', 'Legacy Notes remains an independent BRIM surface during the Guide transition.'));
      card.appendChild(button('brim-guide__primary-button', 'Open legacy Notes', 'legacy-notes'));
    } else if (record.kind === 'Resource' && record.url) {
      var link = node('a', 'brim-guide__primary-button', 'Open official resource ↗');
      link.href = record.url;
      link.target = '_blank';
      link.rel = 'noopener noreferrer';
      card.appendChild(link);
    } else if (record.kind === 'Update') {
      card.appendChild(node('p', 'brim-guide__date', record.date));
    }
    fragment.appendChild(card);
    return fragment;
  }

  function sectionRecordCard(record) {
    var card = node('article', 'brim-guide__content-card');
    card.appendChild(node('p', 'brim-guide__eyebrow', record.kind));
    card.appendChild(node('h2', '', record.title));
    if (record.date) card.appendChild(node('p', 'brim-guide__date', record.date));
    if (record.summary) card.appendChild(node('p', '', record.summary));
    var open = button('brim-guide__text-button', record.kind === 'Resource' ? 'View resource →' : 'Read →', 'record');
    open.setAttribute('data-guide-record', record.id);
    card.appendChild(open);
    return card;
  }

  function renderSection(sectionName) {
    var definitions = {
      methods: ['B · Methods & Guides', 'Methods & Guides', 'Initial guidance for interpreting and finding BRIM Products. Rich content migration is intentionally deferred.'],
      resources: ['C · Resources', 'Resources', 'A small set of verified reusable agency resources.'],
      updates: ['D · Updates', 'Updates', 'Verified user-facing changes to BRIM Guide.']
    };
    var definition = definitions[sectionName];
    var collection = sectionName === 'methods' ? bundle.articles : sectionName === 'resources' ? bundle.resources : bundle.updates;
    var fragment = document.createDocumentFragment();
    fragment.appendChild(heading(definition[0], definition[1], definition[2]));
    var grid = node('div', 'brim-guide__content-grid');
    collection.forEach(function(record) { grid.appendChild(sectionRecordCard(record)); });
    fragment.appendChild(grid);
    return fragment;
  }

  function renderAbout() {
    var fragment = document.createDocumentFragment();
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(heading('About / Contact', bundle.identity.expanded_name, bundle.identity.description));
    var grid = node('div', 'brim-guide__about-grid');
    var purpose = node('article', 'brim-guide__prose-card');
    purpose.appendChild(node('h2', '', 'Purpose and scope'));
    purpose.appendChild(node('p', '', bundle.identity.scope_note));
    var contact = node('article', 'brim-guide__prose-card');
    contact.appendChild(node('h2', '', 'Organization'));
    contact.appendChild(node('p', '', 'BRIM is maintained for Bureau of Land Management California water-resource screening and resource-review support. No contact form or server endpoint is embedded in this standalone map.'));
    grid.appendChild(purpose);
    grid.appendChild(contact);
    fragment.appendChild(grid);
    return fragment;
  }

  function render() {
    activeNav();
    main.replaceChildren();
    if (state.view === 'detail') {
      var selected = recordsById[state.selectedId];
      if (selected) main.appendChild(renderRecordDetail(selected));
      else {
        state.view = 'landing';
        state.selectedId = '';
        main.appendChild(renderExplore());
      }
    } else if (state.view === 'about') {
      main.appendChild(renderAbout());
    } else if (state.section === 'explore') {
      main.appendChild(renderExplore());
    } else {
      main.appendChild(renderSection(state.section));
    }
    main.scrollTop = 0;
  }

  function focusMain(preserveScroll) {
    var desiredScroll = preserveScroll ? main.scrollTop : 0;
    try {
      main.focus({ preventScroll: true });
    } catch (error) {
      main.focus();
    }
    main.scrollTop = desiredScroll;
  }

  function resetHome(focusSearch) {
    state.section = 'explore';
    state.view = 'landing';
    state.query = '';
    state.browseDimension = '';
    state.browseValue = '';
    state.selectedId = '';
    state.history = [];
    render();
    if (focusSearch) {
      var input = document.getElementById('brim-guide-search');
      if (input) input.focus();
    }
  }

  function openGuide(trigger) {
    if (!root.hidden) return;
    state.previousFocus = trigger || document.activeElement;
    root.hidden = false;
    root.setAttribute('aria-hidden', 'false');
    resetHome(false);
    window.setTimeout(function() {
      var input = document.getElementById('brim-guide-search');
      if (input) input.focus();
      else rightClose.focus();
    }, 0);
  }

  function closeGuide(options) {
    if (root.hidden) return;
    root.hidden = true;
    root.setAttribute('aria-hidden', 'true');
    var restoreFocus = !(options && options.restoreFocus === false);
    if (restoreFocus && state.previousFocus && typeof state.previousFocus.focus === 'function') {
      state.previousFocus.focus();
    }
  }

  function openRecord(id) {
    if (!recordsById[id]) return;
    pushHistory();
    state.view = 'detail';
    state.selectedId = id;
    render();
    focusMain(false);
  }

  root.addEventListener('click', function(event) {
    var target = event.target.closest('[data-guide-action]');
    if (!target || !root.contains(target)) return;
    var action = target.getAttribute('data-guide-action');
    if (action === 'close') closeGuide();
    else if (action === 'home') resetHome(true);
    else if (action === 'section') {
      state.section = target.getAttribute('data-guide-section');
      state.view = 'landing';
      state.query = '';
      state.browseDimension = '';
      state.browseValue = '';
      state.selectedId = '';
      state.history = [];
      render();
      focusMain(false);
    } else if (action === 'record') {
      openRecord(target.getAttribute('data-guide-record'));
    } else if (action === 'browse') {
      state.browseDimension = target.getAttribute('data-guide-dimension');
      state.browseValue = target.getAttribute('data-guide-value');
      render();
      focusMain(false);
    } else if (action === 'browse-clear') {
      state.browseDimension = '';
      state.browseValue = '';
      render();
    } else if (action === 'quick') {
      var ids = String(target.getAttribute('data-guide-products') || '').split(',').filter(Boolean);
      if (ids.length === 1) openRecord(ids[0]);
      else {
        state.browseDimension = '';
        state.browseValue = '';
        state.query = '';
        var quickRecords = ids.map(function(id) { return recordsById[id]; }).filter(Boolean);
        main.replaceChildren();
        var fragment = document.createDocumentFragment();
        fragment.appendChild(heading('Quick Access', target.textContent, 'Verified Products in this collection.'));
        fragment.appendChild(renderResults(quickRecords));
        main.appendChild(fragment);
        focusMain(false);
      }
    } else if (action === 'about') {
      pushHistory();
      state.view = 'about';
      render();
      focusMain(false);
    } else if (action === 'back') {
      var saved = state.history.pop();
      if (saved) restore(saved);
      else resetHome(false);
      focusMain(true);
    } else if (action === 'legacy-notes') {
      closeGuide({ restoreFocus: false });
      window.setTimeout(function() {
        var notesButton = document.getElementById('pt-map-notes-btn');
        if (notesButton) notesButton.click();
      }, 0);
    }
  });

  root.addEventListener('input', function(event) {
    if (!event.target.matches('[data-guide-search]')) return;
    state.query = event.target.value;
    state.browseDimension = '';
    state.browseValue = '';
    var position = event.target.selectionStart;
    render();
    var nextInput = document.getElementById('brim-guide-search');
    if (nextInput) {
      nextInput.focus();
      nextInput.setSelectionRange(position, position);
    }
  });

  root.addEventListener('keydown', function(event) {
    if (event.key === 'Escape') {
      event.preventDefault();
      closeGuide();
      return;
    }
    if (event.key !== 'Tab') return;
    var focusable = Array.prototype.slice.call(root.querySelectorAll(
      'button:not([disabled]), input:not([disabled]), a[href], [tabindex]:not([tabindex="-1"])'
    )).filter(function(item) { return item.offsetParent !== null; });
    if (!focusable.length) return;
    var first = focusable[0];
    var last = focusable[focusable.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  });

  window.BRIM_GUIDE = Object.freeze({
    open: openGuide,
    close: closeGuide,
    home: function() { resetHome(true); },
    search: function(query) {
      return search(query).map(function(record) {
        return { id: record.id, title: record.title, kind: record.kind };
      });
    },
    getState: function() { return Object.assign({}, snapshot(), { open: !root.hidden }); }
  });
}
