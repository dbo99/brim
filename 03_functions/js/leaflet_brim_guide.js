function(el, x, data) {
  'use strict';

  var bundle = data && data.bundle ? data.bundle : null;
  var assets = data && data.assets ? data.assets : {};
  if (!bundle || !Array.isArray(bundle.products)) {
    throw new Error('BRIM Guide payload is missing or invalid.');
  }

  var oldRoot = document.getElementById('brim-guide-root');
  if (oldRoot) oldRoot.remove();

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

  var products = asArray(bundle.products);
  var articles = asArray(bundle.articles);
  var resources = asArray(bundle.resources);
  var updates = asArray(bundle.updates);
  var quickAccess = asArray(bundle.quickAccess);
  var resourcesById = {};
  resources.forEach(function(resource) { resourcesById[resource.id] = resource; });
  var articlesById = {};
  articles.forEach(function(article) { articlesById[article.id] = article; });
  var productsById = {};
  products.forEach(function(product) { productsById[product.id] = product; });
  var quickAccessById = {};
  quickAccess.forEach(function(item) { quickAccessById[item.id] = item; });
  products.sort(function(a, b) {
    var titleA = normalize(a.title);
    var titleB = normalize(b.title);
    if (titleA < titleB) return -1;
    if (titleA > titleB) return 1;
    var idA = String(a.id || '');
    var idB = String(b.id || '');
    return idA < idB ? -1 : (idA > idB ? 1 : 0);
  });
  var records = products.concat(articles, resources, updates);
  var recordsById = {};
  records.forEach(function(record) { recordsById[record.id] = record; });

  var state = {
    section: 'explore',
    view: 'landing',
    query: '',
    filters: {
      brimSection: '',
      entityType: '',
      subject: '',
      informationType: ''
    },
    selectedId: '',
    quickIds: [],
    quickId: '',
    quickLabel: '',
    previousFocus: null,
    history: []
  };

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

  function structuredText(record) {
    var values = [];
    asArray(record.sections).forEach(function(section) {
      values.push(section.title);
      values = values.concat(asArray(section.paragraphs), asArray(section.items));
      if (section.table) {
        values.push(section.table.caption);
        asArray(section.table.columns).forEach(function(column) { values.push(column.label); });
        asArray(section.table.rows).forEach(function(row) {
          Object.keys(row || {}).forEach(function(key) { values.push(row[key]); });
        });
      }
    });
    return values.join(' ');
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
    score = Math.max(score, maxFieldScore(query, asArray(record.subjectTags), 650, 620, 410));
    score = Math.max(score, maxFieldScore(query, asArray(record.informationTypes), 600, 570, 390));
    score = Math.max(score, maxFieldScore(query, [record.family], 550, 520, 360));
    score = Math.max(score, maxFieldScore(query, [record.provider].concat(searchTerms), 420, 390, 260));
    score = Math.max(score, maxFieldScore(query, [record.summary], 250, 220, 150));
    score = Math.max(score, maxFieldScore(query, [structuredText(record)], 210, 190, 140));

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
        asArray(record.subjectTags).join(' '),
        asArray(record.informationTypes).join(' '),
        record.family,
        record.provider,
        searchTerms.join(' '),
        record.summary,
        structuredText(record)
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
        return b.score - a.score || a.sourceOrder - b.sourceOrder ||
          a.record.title.localeCompare(b.record.title);
      })
      .slice(0, 60)
      .map(function(item) { return item.record; });
  }

  function hasActiveFilters() {
    return Object.keys(state.filters).some(function(key) { return Boolean(state.filters[key]); });
  }

  function productMatchesFilters(product) {
    if (state.filters.brimSection && product.brimSection !== state.filters.brimSection) return false;
    if (state.filters.entityType && product.entityType !== state.filters.entityType) return false;
    if (state.filters.subject && asArray(product.subjectTags).indexOf(state.filters.subject) < 0) return false;
    if (state.filters.informationType && asArray(product.informationTypes).indexOf(state.filters.informationType) < 0) return false;
    return true;
  }

  function filteredProducts() {
    return products.filter(productMatchesFilters);
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

  var rail = node('aside', 'brim-guide__rail');
  var leftClose = button(
    'brim-guide__close brim-guide__close--left',
    '×',
    'close',
    'Close BRIM Guide'
  );
  var home = button('brim-guide__home', '', 'home', 'BRIM Guide Home');
  home.appendChild(node('span', 'brim-guide__home-name', bundle.identity.guide_name));
  home.appendChild(node('span', 'brim-guide__home-subtitle', 'Home'));
  rail.appendChild(leftClose);
  rail.appendChild(home);

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
  rail.appendChild(nav);

  var quick = node('section', 'brim-guide__quick');
  quick.appendChild(node('h2', 'brim-guide__rail-heading', 'Quick Access'));
  var quickList = node('div', 'brim-guide__quick-list');
  quickAccess.forEach(function(item) {
    var quickButton = button('brim-guide__quick-item', item.label, 'quick');
    quickButton.setAttribute('data-guide-quick', item.id);
    quickButton.setAttribute('data-guide-products', asArray(item.productIds).join(','));
    quickList.appendChild(quickButton);
  });
  quick.appendChild(quickList);
  rail.appendChild(quick);

  var railLower = node('div', 'brim-guide__rail-lower');
  railLower.appendChild(button('brim-guide__about-link', 'About / Contact', 'about'));
  var marks = node('div', 'brim-guide__marks');
  [
    ['https://www.doi.gov/', assets.doi, 'U.S. Department of the Interior'],
    ['https://www.blm.gov/california', assets.blm, 'Bureau of Land Management California']
  ].forEach(function(definition) {
    var link = node('a', 'brim-guide__mark');
    link.href = definition[0];
    link.target = '_blank';
    link.rel = 'noopener noreferrer';
    link.title = definition[2];
    link.setAttribute('aria-label', definition[2]);
    var image = node('img');
    image.src = definition[1] || '';
    image.alt = definition[2];
    link.appendChild(image);
    marks.appendChild(link);
  });
  railLower.appendChild(marks);
  rail.appendChild(railLower);

  var workspace = node('div', 'brim-guide__workspace');
  var utility = node('div', 'brim-guide__utility');
  var searchLabel = node(
    'label',
    'brim-guide__search-label',
    'Search layers, tools, methods, resources, and updates'
  );
  searchLabel.htmlFor = 'brim-guide-search';
  var searchInput = node('input', 'brim-guide__search');
  searchInput.type = 'search';
  searchInput.id = 'brim-guide-search';
  searchInput.autocomplete = 'off';
  searchInput.placeholder = 'Search layers, tools, methods, resources, and updates';
  searchInput.setAttribute('data-guide-search', 'true');
  var rightClose = button(
    'brim-guide__close brim-guide__close--right',
    '×',
    'close',
    'Close BRIM Guide'
  );
  utility.appendChild(searchLabel);
  utility.appendChild(searchInput);
  utility.appendChild(rightClose);
  workspace.appendChild(utility);

  var main = node('main', 'brim-guide__main');
  main.id = 'brim-guide-main';
  main.tabIndex = -1;
  workspace.appendChild(main);

  shell.appendChild(rail);
  shell.appendChild(workspace);
  root.appendChild(shell);
  document.body.appendChild(root);

  function snapshot() {
    return {
      section: state.section,
      view: state.view,
      query: state.query,
      filters: Object.assign({}, state.filters),
      selectedId: state.selectedId,
      quickIds: state.quickIds.slice(),
      quickId: state.quickId,
      quickLabel: state.quickLabel,
      scrollTop: main.scrollTop
    };
  }

  function restore(saved) {
    Object.keys(saved).forEach(function(key) {
      if (key !== 'scrollTop') state[key] = saved[key];
    });
    searchInput.value = state.query;
    render();
    main.scrollTop = saved.scrollTop || 0;
  }

  function pushHistory() {
    state.history.push(snapshot());
    if (state.history.length > 30) state.history.shift();
  }

  function activeNav() {
    nav.querySelectorAll('[data-guide-section]').forEach(function(item) {
      var active = item.getAttribute('data-guide-section') === state.section &&
        state.view !== 'about';
      item.classList.toggle('is-active', active);
      if (active) item.setAttribute('aria-current', 'page');
      else item.removeAttribute('aria-current');
    });
  }

  function pageHeading(eyebrow, title, description, modifier) {
    var wrap = node('div', 'brim-guide__page-heading');
    if (modifier) wrap.classList.add('brim-guide__page-heading--' + modifier);
    wrap.appendChild(node('p', 'brim-guide__eyebrow', eyebrow));
    var heading = node('h1', '', title);
    heading.id = 'brim-guide-title';
    wrap.appendChild(heading);
    if (description) wrap.appendChild(node('p', 'brim-guide__lede', description));
    return wrap;
  }

  function renderStructuredSections(sections, modifier) {
    var wrap = node('div', 'brim-guide__structured-sections');
    if (modifier) wrap.classList.add('brim-guide__structured-sections--' + modifier);
    asArray(sections).forEach(function(section) {
      var block = node('section', 'brim-guide__structured-section');
      if (section.id) block.setAttribute('data-guide-section-id', section.id);
      block.appendChild(node('h2', '', section.title));
      asArray(section.paragraphs).forEach(function(paragraph) {
        block.appendChild(node('p', '', paragraph));
      });
      var items = asArray(section.items);
      if (items.length) {
        var list = node('ul', '');
        items.forEach(function(item) { list.appendChild(node('li', '', item)); });
        block.appendChild(list);
      }
      if (section.table) {
        var tableWrap = node('div', 'brim-guide__table-wrap');
        var table = node('table', 'brim-guide__table');
        if (section.table.caption) table.appendChild(node('caption', '', section.table.caption));
        var columns = asArray(section.table.columns);
        if (columns.length) {
          var head = node('thead');
          var headRow = node('tr');
          columns.forEach(function(column) { headRow.appendChild(node('th', '', column.label)); });
          head.appendChild(headRow);
          table.appendChild(head);
        }
        var body = node('tbody');
        asArray(section.table.rows).forEach(function(row) {
          var tableRow = node('tr');
          columns.forEach(function(column) {
            tableRow.appendChild(node('td', '', row && row[column.key]));
          });
          body.appendChild(tableRow);
        });
        table.appendChild(body);
        tableWrap.appendChild(table);
        block.appendChild(tableWrap);
      }
      wrap.appendChild(block);
    });
    return wrap;
  }

  function renderRelatedProducts(record) {
    var relatedProducts = asArray(record.relatedProductIds).map(function(productId) {
      return productsById[productId];
    }).filter(Boolean);
    if (!relatedProducts.length) return null;
    var related = node('section', 'brim-guide__related brim-guide__related--products');
    related.appendChild(node('h2', '', 'Related Layers & Tools'));
    relatedProducts.forEach(function(product) {
      var relatedButton = button('brim-guide__related-item', product.title, 'record');
      relatedButton.setAttribute('data-guide-record', product.id);
      related.appendChild(relatedButton);
    });
    return related;
  }

  function sectionHeading(number, title, detail) {
    var wrap = node('div', 'brim-guide__section-heading');
    wrap.appendChild(node('span', 'brim-guide__section-number', number));
    wrap.appendChild(node('h2', '', title));
    if (detail) wrap.appendChild(node('span', 'brim-guide__section-detail', detail));
    return wrap;
  }

  function recordType(record) {
    if (record.kind === 'Product') return record.entityType || 'Layer';
    if (record.kind === 'Article') return 'Method';
    return record.kind || 'Resource';
  }

  function resultRow(record) {
    var row = button('brim-guide__result', '', 'record');
    row.setAttribute('data-guide-record', record.id);
    row.appendChild(node('span', 'brim-guide__result-kind', recordType(record)));
    row.appendChild(node('strong', 'brim-guide__result-title', record.title));
    var context = record.kind === 'Product'
      ? record.pathLabel
      : (record.section || record.provider || record.date || 'BRIM Guide');
    row.appendChild(node('span', 'brim-guide__result-path', context));
    if (record.summary) row.appendChild(node('span', 'brim-guide__result-summary', record.summary));
    return row;
  }

  function renderResults(recordsToRender, emptyMessage, modifier) {
    var list = node('div', 'brim-guide__results');
    if (modifier) list.classList.add('brim-guide__results--' + modifier);
    if (modifier === 'index') {
      list.setAttribute('role', 'region');
      list.setAttribute('aria-label', 'All layers and tools A to Z');
      list.tabIndex = 0;
    }
    if (!recordsToRender.length) {
      list.appendChild(node(
        'p',
        'brim-guide__empty',
        emptyMessage || 'No Guide records match this view.'
      ));
      return list;
    }
    recordsToRender.forEach(function(record) { list.appendChild(resultRow(record)); });
    return list;
  }

  function renderIntro() {
    var intro = node('section', 'brim-guide__intro');
    var identity = node('div', 'brim-guide__identity');
    identity.appendChild(node('p', 'brim-guide__eyebrow', bundle.identity.expanded_name));
    var title = node('h1', '', bundle.identity.guide_name);
    title.id = 'brim-guide-title';
    identity.appendChild(title);
    identity.appendChild(node('p', 'brim-guide__identity-line', 'Layers, Tools, Methods & Sources'));
    intro.appendChild(identity);
    intro.appendChild(node('p', 'brim-guide__intro-description', bundle.identity.description));
    return intro;
  }

  function filterValues(field) {
    var values = [];
    products.forEach(function(product) {
      var productValues = field === 'subject'
        ? asArray(product.subjectTags)
        : field === 'informationType'
          ? asArray(product.informationTypes)
          : [product[field]];
      productValues.forEach(function(value) {
        if (value && values.indexOf(value) < 0) values.push(value);
      });
    });
    return values.sort();
  }

  function filterSelect(field, label, values) {
    var wrap = node('label', 'brim-guide__filter');
    wrap.appendChild(node('span', '', label));
    var select = node('select', 'brim-guide__filter-select');
    select.setAttribute('data-guide-filter', field);
    var allOption = node('option', '', 'All');
    allOption.value = '';
    select.appendChild(allOption);
    values.forEach(function(value) {
      var option = node('option', '', value);
      option.value = value;
      if (state.filters[field] === value) option.selected = true;
      select.appendChild(option);
    });
    wrap.appendChild(select);
    return wrap;
  }

  function renderFilters() {
    var browse = node('section', 'brim-guide__browse');
    browse.appendChild(sectionHeading('01', 'Browse BRIM layers & tools', filteredProducts().length + ' of ' + products.length));
    var controls = node('div', 'brim-guide__filters');
    controls.appendChild(filterSelect('brimSection', 'BRIM section', filterValues('brimSection')));
    controls.appendChild(filterSelect('entityType', 'Entity type', filterValues('entityType')));
    controls.appendChild(filterSelect('subject', 'Subject', filterValues('subject')));
    controls.appendChild(filterSelect('informationType', 'Information Type', filterValues('informationType')));
    var clear = button('brim-guide__clear-filters', 'Clear filters', 'filters-clear');
    clear.disabled = !hasActiveFilters();
    controls.appendChild(clear);
    browse.appendChild(controls);
    return browse;
  }

  function renderExplore() {
    var fragment = document.createDocumentFragment();

    if (state.query) {
      var searchResults = search(state.query);
      if (hasActiveFilters()) {
        searchResults = searchResults.filter(function(record) {
          return record.kind === 'Product' && productMatchesFilters(record);
        });
      }
      fragment.appendChild(pageHeading('A · Explore', 'Search results', state.query));
      fragment.appendChild(renderFilters());
      fragment.appendChild(renderResults(
        searchResults,
        'No Guide records match that search. Try a shorter source-supported term.'
      ));
      return fragment;
    }

    fragment.appendChild(renderIntro());
    fragment.appendChild(node('p', 'brim-guide__scope-note', bundle.identity.scope_note));
    var directory = node('div', 'brim-guide__explore-directory');
    directory.appendChild(renderFilters());
    var filtered = filteredProducts();
    var productSection = node('section', 'brim-guide__product-index');
    productSection.appendChild(sectionHeading('02', 'All Layers & Tools A–Z', filtered.length + ' of ' + products.length));
    productSection.appendChild(renderResults(filtered, 'No layers or tools match the active filters.', 'index'));
    directory.appendChild(productSection);
    fragment.appendChild(directory);
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
    fragment.appendChild(pageHeading(product.subsystem, product.title, product.summary, 'product'));

    var locator = node('section', 'brim-guide__locator');
    locator.appendChild(node('h2', '', 'Find in layer list'));
    locator.appendChild(node('p', 'brim-guide__path', product.pathLabel));
    locator.appendChild(node(
      'p',
      'brim-guide__boundary',
      'Use this exact path in the existing map controls. The Guide explains this item but does not change map state.'
    ));
    fragment.appendChild(locator);

    if (asArray(product.sections).length) {
      fragment.appendChild(renderStructuredSections(product.sections, 'product'));
    }

    var detailLayout = node('div', 'brim-guide__detail-layout');
    var dl = node('dl', 'brim-guide__detail-list');
    [
      ['BRIM section', product.brimSection],
      ['Provider / program', product.provider],
      ['Subjects', asArray(product.subjectTags).join(', ')],
      ['Information Type', asArray(product.informationTypes).join(', ')],
      ['Layer / tool family', product.family],
      ['Guide coverage', product.contentTier === 'SOURCE_BACKED_RICH'
        ? 'Source-backed rich detail'
        : (product.contentTier === 'EDITORIAL_REVIEW_REQUIRED'
          ? 'Editorial review required'
          : 'Structured basic detail')]
    ].forEach(function(definition) {
      var row = detailRow(definition[0], definition[1]);
      if (row) dl.appendChild(row);
    });
    detailLayout.appendChild(dl);

    var relatedArticleIds = asArray(product.relatedArticleIds);
    var relationships = asArray(product.relatedResources);
    if (relatedArticleIds.length || relationships.length) {
      var relatedColumn = node('div', 'brim-guide__related-column');
      if (relatedArticleIds.length) {
        var methods = node('section', 'brim-guide__related');
        methods.appendChild(node('h2', '', 'Related Methods'));
        relatedArticleIds.forEach(function(articleId) {
          var article = articlesById[articleId];
          if (!article) return;
          var methodButton = button('brim-guide__related-item', article.title, 'record');
          methodButton.setAttribute('data-guide-record', article.id);
          methods.appendChild(methodButton);
        });
        relatedColumn.appendChild(methods);
      }
      if (relationships.length) {
        var related = node('section', 'brim-guide__related');
        related.appendChild(node('h2', '', 'Related Resources'));
        relationships.forEach(function(relationship) {
          var resource = resourcesById[relationship.id];
          if (!resource) return;
          var relatedButton = button('brim-guide__related-item', '', 'record');
          relatedButton.setAttribute('data-guide-record', resource.id);
          relatedButton.appendChild(node('span', 'brim-guide__related-title', resource.title));
          relatedButton.appendChild(node('span', 'brim-guide__related-role', relationship.role));
          related.appendChild(relatedButton);
        });
        relatedColumn.appendChild(related);
      }
      detailLayout.appendChild(relatedColumn);
    }
    fragment.appendChild(detailLayout);
    return fragment;
  }

  function renderRecordDetail(record) {
    if (record.kind === 'Product') return renderProductDetail(record);
    var fragment = document.createDocumentFragment();
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(pageHeading(recordType(record), record.title, record.summary));
    var detail = node('article', 'brim-guide__prose');
    if (record.kind === 'Article') {
      detail.classList.add('brim-guide__method-body');
      detail.appendChild(renderStructuredSections(record.sections, 'method'));
      var externalLinks = asArray(record.externalLinks);
      if (externalLinks.length) {
        var sources = node('section', 'brim-guide__related');
        sources.appendChild(node('h2', '', 'Sources & Resources'));
        externalLinks.forEach(function(externalLink) {
          var link = node('a', 'brim-guide__related-item', '');
          link.href = externalLink.url;
          link.target = '_blank';
          link.rel = 'noopener noreferrer';
          link.appendChild(node('span', 'brim-guide__related-title', externalLink.label));
          link.appendChild(node('span', 'brim-guide__related-role', externalLink.role));
          sources.appendChild(link);
        });
        detail.appendChild(sources);
      }
    }
    if (record.kind === 'Resource' && record.url) {
      var link = node('a', 'brim-guide__resource-link', 'Open official resource ↗');
      link.href = record.url;
      link.target = '_blank';
      link.rel = 'noopener noreferrer';
      detail.appendChild(link);
    } else if (record.kind === 'Update') {
      var updateMeta = node('p', 'brim-guide__date');
      updateMeta.appendChild(node('time', '', record.date));
      if (record.updateType) updateMeta.appendChild(node('span', '', ' · ' + record.updateType));
      detail.appendChild(updateMeta);
    }
    var relatedProducts = renderRelatedProducts(record);
    if (relatedProducts) detail.appendChild(relatedProducts);
    fragment.appendChild(detail);
    return fragment;
  }

  function contentRow(record) {
    var row = button('brim-guide__content-row', '', 'record');
    row.setAttribute('data-guide-record', record.id);
    row.appendChild(node('span', 'brim-guide__result-kind', recordType(record)));
    row.appendChild(node('strong', '', record.title));
    row.appendChild(node('span', '', record.date || record.summary || 'Open'));
    row.appendChild(node('span', 'brim-guide__row-arrow', '→'));
    return row;
  }

  function renderSection(sectionName) {
    var definitions = {
      methods: ['B · Methods & Guides', 'Methods & Guides', 'Compact guidance for interpreting and finding BRIM layers and tools.'],
      resources: ['C · Resources', 'Resources', 'Verified reusable agency resources.'],
      updates: ['D · Updates', 'Updates', 'Verified user-facing Guide changes.']
    };
    var definition = definitions[sectionName];
    var collection = sectionName === 'methods'
      ? articles
      : sectionName === 'resources' ? resources : updates;
    var fragment = document.createDocumentFragment();
    fragment.appendChild(pageHeading(definition[0], definition[1], definition[2]));
    var list = node('div', 'brim-guide__content-list');
    collection.forEach(function(record) { list.appendChild(contentRow(record)); });
    fragment.appendChild(list);
    return fragment;
  }

  function renderQuickResults() {
    var quickRecords = state.quickIds.map(function(id) {
      return recordsById[id];
    }).filter(Boolean);
    var fragment = document.createDocumentFragment();
    var quickDefinition = quickAccessById[state.quickId];
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(pageHeading(
      'Collection · Quick Access',
      state.quickLabel,
      quickDefinition && quickDefinition.summary
        ? quickDefinition.summary
        : 'Verified layers and tools in this collection.'
    ));
    fragment.appendChild(renderResults(quickRecords));
    return fragment;
  }

  function renderAbout() {
    var fragment = document.createDocumentFragment();
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(pageHeading(
      'About / Contact',
      bundle.identity.expanded_name,
      bundle.identity.description
    ));
    var about = node('div', 'brim-guide__about');
    var purpose = node('section', 'brim-guide__prose');
    purpose.appendChild(node('h2', '', 'Purpose and scope'));
    purpose.appendChild(node('p', '', bundle.identity.scope_note));
    var contact = node('section', 'brim-guide__prose');
    contact.appendChild(node('h2', '', 'Contact'));
    contact.appendChild(node(
      'p',
      '',
      'BRIM is maintained for Bureau of Land Management California water-resource screening and resource-review support. The link below opens a draft in your email application; the Guide does not send or store the message.'
    ));
    var contactLink = node('a', 'brim-guide__resource-link', 'Open email draft');
    contactLink.href = 'mailto:doconnor@blm.gov?subject=' +
      encodeURIComponent('BRIM Guide feedback') + '&body=' +
      encodeURIComponent('BRIM Guide page or layer:\n\nFeedback:\n');
    contact.appendChild(contactLink);
    about.appendChild(purpose);
    about.appendChild(contact);
    fragment.appendChild(about);
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
    } else if (state.view === 'quick') {
      main.appendChild(renderQuickResults());
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
    state.filters = { brimSection: '', entityType: '', subject: '', informationType: '' };
    state.selectedId = '';
    state.quickIds = [];
    state.quickId = '';
    state.quickLabel = '';
    state.history = [];
    searchInput.value = '';
    render();
    if (focusSearch) searchInput.focus();
  }

  function openGuide(trigger) {
    if (trigger) state.previousFocus = trigger;
    else if (root.hidden) state.previousFocus = document.activeElement;
    root.hidden = false;
    root.setAttribute('aria-hidden', 'false');
    resetHome(false);
    window.setTimeout(function() { searchInput.focus(); }, 0);
  }

  function closeGuide() {
    if (root.hidden) return;
    root.hidden = true;
    root.setAttribute('aria-hidden', 'true');
    if (state.previousFocus && typeof state.previousFocus.focus === 'function') {
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
    if (action === 'close') {
      closeGuide();
    } else if (action === 'home') {
      resetHome(true);
    } else if (action === 'section') {
      state.section = target.getAttribute('data-guide-section');
      state.view = 'landing';
      state.query = '';
      state.selectedId = '';
      state.quickIds = [];
      state.quickId = '';
      state.quickLabel = '';
      state.history = [];
      searchInput.value = '';
      render();
      focusMain(false);
    } else if (action === 'record') {
      openRecord(target.getAttribute('data-guide-record'));
    } else if (action === 'filters-clear') {
      state.filters = { brimSection: '', entityType: '', subject: '', informationType: '' };
      render();
    } else if (action === 'quick') {
      var quickIds = String(target.getAttribute('data-guide-products') || '')
        .split(',')
        .filter(Boolean);
      if (quickIds.length === 1) {
        openRecord(quickIds[0]);
      } else {
        pushHistory();
        state.section = 'explore';
        state.view = 'quick';
        state.query = '';
        state.quickIds = quickIds;
        state.quickId = target.getAttribute('data-guide-quick') || '';
        state.quickLabel = target.textContent;
        searchInput.value = '';
        render();
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
    }
  });

  root.addEventListener('change', function(event) {
    var select = event.target.closest('[data-guide-filter]');
    if (!select || !root.contains(select)) return;
    var field = select.getAttribute('data-guide-filter');
    if (!Object.prototype.hasOwnProperty.call(state.filters, field)) return;
    state.filters[field] = select.value;
    state.section = 'explore';
    state.view = 'landing';
    state.selectedId = '';
    state.quickIds = [];
    state.quickId = '';
    state.quickLabel = '';
    state.history = [];
    render();
    var replacement = root.querySelector('[data-guide-filter="' + field + '"]');
    if (replacement) replacement.focus();
  });

  searchInput.addEventListener('input', function(event) {
    state.section = 'explore';
    state.view = 'landing';
    state.query = event.target.value;
    state.selectedId = '';
    state.quickIds = [];
    state.quickId = '';
    state.quickLabel = '';
    state.history = [];
    render();
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
    getState: function() {
      return Object.assign({}, snapshot(), { open: !root.hidden });
    }
  });
}
