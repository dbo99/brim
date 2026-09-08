function(el, x, data) {
  'use strict';

  var bundle = data && data.bundle ? data.bundle : null;
  var assets = data && data.assets ? data.assets : {};
  if (!bundle || !Array.isArray(bundle.products)) {
    throw new Error('BRIM Guide payload is missing or invalid.');
  }

  var oldRoot = document.getElementById('brim-guide-root');
  if (oldRoot && typeof oldRoot.__brimGuideTeardown === 'function') {
    oldRoot.__brimGuideTeardown();
  } else if (oldRoot) {
    oldRoot.remove();
  }

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

  function ptCreateResourceExplorerModel(inputResources) {
    'use strict';

    var sourceResources = Array.isArray(inputResources) ? inputResources.slice() : [];
    var pageSize = 25;
    var presetDefinitions = [
      { id: 'in_brim_map', label: 'In BRIM map' },
      { id: 'beyond_the_map', label: 'Beyond the map' },
      { id: 'all_resources', label: 'All Resources' }
    ];

    function array(value) {
      if (Array.isArray(value)) return value;
      if (value === undefined || value === null || value === '') return [];
      return [value];
    }

    function normalizeText(value) {
      return String(value || '')
        .normalize('NFKD')
        .replace(/[\u0300-\u036f]/g, '')
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, ' ')
        .trim()
        .replace(/\s+/g, ' ');
    }


    // Curated browsing associations only; original attribution and search stay intact.
    // Checkbox membership and order are an explicit maintainer decision. Counts are display only.
    var providerPolicy = {
      "provider_roles": [
        "display_provider",
        "publisher",
        "maintainer",
        "partner"
      ],
      "groups": [
        {
          "id": "federal",
          "label": "Federal"
        },
        {
          "id": "state",
          "label": "State"
        }
      ],
      "families": [
        {
          "id": "blm",
          "label": "BLM",
          "group": "federal",
          "exact_provider_names": [
            "Bureau of Land Management"
          ]
        },
        {
          "id": "epa",
          "label": "EPA",
          "group": "federal",
          "exact_provider_names": [
            "U.S. Environmental Protection Agency"
          ]
        },
        {
          "id": "fema",
          "label": "FEMA",
          "group": "federal",
          "exact_provider_names": [
            "Federal Emergency Management Agency"
          ]
        },
        {
          "id": "nasa",
          "label": "NASA",
          "group": "federal",
          "exact_provider_names": [
            "NASA",
            "National Aeronautics and Space Administration",
            "NASA / JPL",
            "NASA Jet Propulsion Laboratory",
            "NASA / NSIDC DAAC",
            "NASA / University of Nebraska–Lincoln"
          ]
        },
        {
          "id": "noaa",
          "label": "NOAA",
          "group": "federal",
          "exact_provider_names": [
            "National Oceanic and Atmospheric Administration",
            "National Weather Service",
            "NOAA / National Weather Service",
            "NOAA / NESDIS STAR",
            "NOAA National Environmental Satellite, Data, and Information Service / Center for Satellite Applications and Research",
            "NOAA Climate Prediction Center",
            "NOAA Weather Prediction Center",
            "CIRA / NOAA RAMMB",
            "NOAA Regional and Mesoscale Meteorology Branch",
            "National Integrated Drought Information System",
            "NIDIS / NOAA",
            "NCSMMN / NIDIS"
          ]
        },
        {
          "id": "usace",
          "label": "USACE",
          "group": "federal",
          "exact_provider_names": [
            "U.S. Army Corps of Engineers"
          ]
        },
        {
          "id": "usbr",
          "label": "USBR",
          "group": "federal",
          "exact_provider_names": [
            "Bureau of Reclamation"
          ]
        },
        {
          "id": "usda",
          "label": "USDA",
          "group": "federal",
          "exact_provider_names": [
            "U.S. Department of Agriculture",
            "U.S. Department of Agriculture National Agricultural Statistics Service",
            "USDA NASS / George Mason University",
            "USDA Natural Resources Conservation Service",
            "USDA NRCS"
          ]
        },
        {
          "id": "usgs",
          "label": "USGS",
          "group": "federal",
          "exact_provider_names": [
            "U.S. Geological Survey",
            "USGS / National Drought Mitigation Center"
          ]
        },
        {
          "id": "dwr",
          "label": "DWR",
          "group": "state",
          "exact_provider_names": [
            "California Department of Water Resources"
          ]
        },
        {
          "id": "waterboards",
          "label": "Water Boards",
          "group": "state",
          "exact_provider_names": [
            "California State Water Resources Control Board"
          ]
        }
      ],
      "exact_resource_rules": [
        {
          "resource_id": "resource_geospatial_and_remote_sensing_data_provi_fema_national_flood_hazard_layer_viewer_viewer",
          "families": [
            "fema"
          ],
          "expected_provider": "Geospatial and remote-sensing data providers",
          "expected_title": "FEMA National Flood Hazard Layer Viewer",
          "expected_canonical_url": "https://hazards-fema.maps.arcgis.com/apps/webappviewer/index.html?id=8b0adb51996444d4879338b5529aa9cd"
        },
        {
          "resource_id": "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
          "families": [
            "noaa"
          ],
          "expected_provider": "Climate and drought data providers",
          "expected_title": "Drought.gov California",
          "expected_canonical_url": "https://drought.gov/states/california"
        },
        {
          "resource_id": "resource_water_quality_and_ecosystem_data_provide_safe_to_swim_map_viewer",
          "families": [
            "waterboards"
          ],
          "expected_provider": "Water quality and ecosystem data providers",
          "expected_title": "Safe to Swim Map",
          "expected_canonical_url": "https://www.mywaterquality.ca.gov/safe-to-swim/content/interactive_map/index.html"
        }
      ],
      "legacy_families": [
        {
          "id": "nifc",
          "label": "NIFC / WFIGS — Interagency fire information",
          "exact_provider_names": [
            "NIFC / WFIGS"
          ]
        },
        {
          "id": "calfire",
          "label": "CAL FIRE / FRAP",
          "exact_provider_names": [
            "CAL FIRE / FRAP"
          ]
        },
        {
          "id": "caloes",
          "label": "Cal OES — Emergency Services",
          "exact_provider_names": [
            "California Governor's Office of Emergency Services"
          ]
        },
        {
          "id": "cdfa",
          "label": "CDFA — Food & Agriculture",
          "exact_provider_names": [
            "California Department of Food and Agriculture"
          ]
        },
        {
          "id": "cvfpb",
          "label": "Central Valley Flood Protection Board",
          "exact_provider_names": [
            "Central Valley Flood Protection Board"
          ]
        },
        {
          "id": "aso",
          "label": "Airborne Snow Observatories",
          "exact_provider_names": [
            "Airborne Snow Observatories, Inc."
          ]
        },
        {
          "id": "cw3e",
          "label": "CW3E — Western Weather & Water Extremes",
          "exact_provider_names": [
            "Center for Western Weather and Water Extremes"
          ]
        },
        {
          "id": "climateengine",
          "label": "Climate Engine",
          "exact_provider_names": [
            "Climate Engine"
          ]
        },
        {
          "id": "nsidc",
          "label": "NSIDC — Snow & Ice Data Center",
          "exact_provider_names": [
            "National Snow and Ice Data Center",
            "National Snow and Ice Data Center Distributed Active Archive Center",
            "NASA / NSIDC DAAC"
          ]
        },
        {
          "id": "pivotal",
          "label": "Pivotal Weather",
          "exact_provider_names": [
            "Pivotal Weather"
          ]
        },
        {
          "id": "prism",
          "label": "PRISM — Oregon State University",
          "exact_provider_names": [
            "PRISM Climate Group, Oregon State University"
          ]
        },
        {
          "id": "synoptic",
          "label": "Synoptic Data / MesoWest",
          "exact_provider_names": [
            "Synoptic Data"
          ]
        },
        {
          "id": "windy",
          "label": "Windy",
          "exact_provider_names": [
            "Windy"
          ]
        }
      ]
    };
    var allProviderFamilies = providerPolicy.families.concat(providerPolicy.legacy_families);

    function normalizeProvider(value) {
      return String(value || '').normalize('NFC').trim().replace(/\s+/g, ' ').toLowerCase();
    }

    function providerFamily(value) {
      return allProviderFamilies.find(function(family) {
        return value === 'family:' + family.id;
      });
    }

    function providerMembership(resource) {
      var names = [resource.provider].concat(array(resource.providers).filter(function(provider) {
        return providerPolicy.provider_roles.indexOf(provider.role) >= 0;
      }).map(function(provider) { return provider.name; })).map(normalizeProvider);
      var members = allProviderFamilies.filter(function(family) {
        return family.exact_provider_names.some(function(name) {
          return names.indexOf(normalizeProvider(name)) >= 0;
        });
      }).map(function(family) { return 'family:' + family.id; });
      providerPolicy.exact_resource_rules.forEach(function(rule) {
        if (resource.id !== rule.resource_id) return;
        if (resource.provider !== rule.expected_provider || resource.title !== rule.expected_title ||
            resource.canonicalUrl !== rule.expected_canonical_url) {
          throw new Error('Selected provider Resource guard mismatch: ' + resource.id);
        }
        rule.families.forEach(function(id) {
          if (members.indexOf('family:' + id) < 0) members.push('family:' + id);
        });
      });
      return members;
    }

    var providerMemberships = new Map();
    sourceResources.forEach(function(resource) {
      providerMemberships.set(resource.id, providerMembership(resource));
    });

    function providerMatches(resource, selection) {
      // Existing raw-name selections remain exact, including search-only local sources.
      return providerFamily(selection)
        ? providerMemberships.get(resource.id).indexOf(selection) >= 0
        : resource.provider === selection;
    }

    function providerFocusKey(selection) {
      return 'resource-provider-' + encodeURIComponent(selection);
    }

    function providerGroups() {
      return providerPolicy.groups.map(function(group) {
        return { id: group.id, label: group.label };
      });
    }

    function providerCounts(stateInput) {
      var state = createState(stateInput);
      var matches = new Map(allProviderFamilies.map(function(family) {
        return ['family:' + family.id, new Set()];
      }));
      sourceResources.forEach(function(resource) {
        if (!dimensionMatch(resource, state, 'providers')) return;
        providerMemberships.get(resource.id).forEach(function(value) {
          matches.get(value).add(resource.id);
        });
      });
      var counts = {};
      matches.forEach(function(ids, value) { counts[value] = ids.size; });
      return counts;
    }

    function tokenWords(value) {
      var normalized = normalizeText(value);
      return normalized ? normalized.split(' ') : [];
    }

    function distanceAtMostOne(a, b) {
      if (a === b) return true;
      if (Math.abs(a.length - b.length) > 1) return false;
      var left = 0;
      var right = 0;
      var edits = 0;
      while (left < a.length && right < b.length) {
        if (a[left] === b[right]) {
          left += 1;
          right += 1;
        } else {
          edits += 1;
          if (edits > 1) return false;
          if (a.length > b.length) left += 1;
          else if (b.length > a.length) right += 1;
          else {
            left += 1;
            right += 1;
          }
        }
      }
      if (left < a.length || right < b.length) edits += 1;
      return edits <= 1;
    }

    function fieldScore(query, values, exact, prefix, contains) {
      var result = 0;
      array(values).forEach(function(value) {
        var candidate = normalizeText(value);
        if (!candidate) return;
        if (candidate === query) result = Math.max(result, exact);
        else if (candidate.indexOf(query) === 0) result = Math.max(result, prefix);
        else if (candidate.indexOf(query) >= 0) result = Math.max(result, contains);
      });
      return result;
    }

    function providerNames(resource) {
      return array(resource.providers).map(function(provider) { return provider.name; });
    }

    function resourceTypeLabel(value) {
      var match = sourceResources.find(function(resource) {
        return resource.resourceType === value && resource.resourceTypeLabel;
      });
      return match ? match.resourceTypeLabel : value;
    }

    function geographyValues(resource) {
      var scope = resource.geographicScope || {};
      return [scope.scopeLabel].concat(array(scope.names)).filter(Boolean);
    }

    function hasProductContext(resource, productId) {
      if (!productId) return true;
      return array(resource.representedProducts).some(function(product) {
        return product.productId === productId;
      });
    }

    function scoreResource(resource, state) {
      var query = normalizeText(state.query);
      var tokens = tokenWords(query);
      var searchable = normalizeText(resource.searchText);
      var allTokensMatch = !tokens.length || tokens.every(function(token) {
        return searchable.indexOf(token) >= 0;
      });
      var typoMatch = false;
      if (!allTokensMatch && tokens.length === 1 && tokens[0].length >= 5) {
        var candidates = tokenWords(
          [resource.title].concat(array(resource.aliases)).join(' ')
        );
        typoMatch = candidates.some(function(candidate) {
          return candidate.length >= 5 && distanceAtMostOne(tokens[0], candidate);
        });
      }
      if (!allTokensMatch && !typoMatch) return null;

      var score = typoMatch ? 70 : 0;
      if (query && allTokensMatch) {
        score = Math.max(score, fieldScore(query, [resource.title], 1200, 1000, 520));
        score = Math.max(score, fieldScore(query, array(resource.aliases), 1100, 900, 500));
        score = Math.max(score, fieldScore(query, [resource.provider], 850, 700, 420));
        score = Math.max(score, fieldScore(
          query,
          providerNames(resource).filter(function(name) { return name !== resource.provider; }),
          700, 560, 360
        ));
        score = Math.max(score, fieldScore(query, array(resource.subjectTags), 650, 620, 410));
        score = Math.max(score, fieldScore(
          query,
          array(resource.variables).concat(array(resource.useScopes)),
          600, 570, 390
        ));
        score = Math.max(score, fieldScore(query, array(resource.informationTypeTags), 560, 530, 370));
        score = Math.max(score, fieldScore(query, geographyValues(resource), 520, 490, 340));
        score = Math.max(score, fieldScore(
          query,
          [resource.resourceTypeLabel, resource.resourceGranularity].concat(
            array(resource.accessPoints).map(function(point) { return point.label; })
          ),
          460, 430, 300
        ));
        score = Math.max(score, fieldScore(query, [resource.summary], 250, 220, 150));
        score += 340 + tokens.length;
      }
      if (state.productContextId && hasProductContext(resource, state.productContextId)) {
        score += 2000;
      }
      return score;
    }

    function stateCopy(value) {
      return {
        query: String(value.query || ''),
        preset: String(value.preset || 'all_resources'),
        providers: array(value.providers).slice(),
        // Ignore obsolete provider-only search state; it cannot hide the shortlist.
        providerQuery: '',
        subject: String(value.subject || ''),
        informationType: String(value.informationType || ''),
        resourceType: String(value.resourceType || ''),
        moreFiltersOpen: Boolean(value.moreFiltersOpen),
        sortMode: String(value.sortMode || ''),
        selectedId: String(value.selectedId || ''),
        renderLimit: Number(value.renderLimit) || pageSize,
        productContextId: String(value.productContextId || ''),
        pane: String(value.pane || 'results'),
        facetsOpen: Boolean(value.facetsOpen),
        facetScrollTop: Math.max(0, Number(value.facetScrollTop) || 0),
        resultsScrollTop: Math.max(0, Number(value.resultsScrollTop) || 0),
        detailScrollTop: Math.max(0, Number(value.detailScrollTop) || 0),
        returnResultsScrollTop: Math.max(0, Number(value.returnResultsScrollTop) || 0),
        selectedFocusKey: String(value.selectedFocusKey || ''),
        focusKey: String(value.focusKey || 'resource-search')
      };
    }

    function createState(options) {
      var initial = stateCopy(options || {});
      if (!presetDefinitions.some(function(preset) { return preset.id === initial.preset; })) {
        initial.preset = 'all_resources';
      }
      initial.providers = initial.providers.filter(function(value, index, values) {
        return typeof value === 'string' && value && values.indexOf(value) === index;
      });
      initial.renderLimit = Math.max(pageSize, initial.renderLimit);
      if (!initial.sortMode) {
        initial.sortMode = initial.query || initial.productContextId ? 'relevance' : 'title';
      }
      return initial;
    }

    function presetMatch(resource, presetId) {
      if (presetId === 'in_brim_map') {
        return resource.mapRepresentation === 'direct_match_in_brim' ||
          resource.mapRepresentation === 'selected_products_in_brim';
      }
      if (presetId === 'beyond_the_map') {
        return resource.mapRepresentation === 'not_currently_mapped_in_brim';
      }
      return true;
    }

    function dimensionMatch(resource, state, ignored) {
      if (ignored !== 'preset' && !presetMatch(resource, state.preset)) return false;
      if (ignored !== 'productContext' &&
          !hasProductContext(resource, state.productContextId)) return false;
      if (ignored !== 'providers' && state.providers.length &&
          !state.providers.some(function(value) { return providerMatches(resource, value); })) return false;
      if (ignored !== 'subject' && state.subject &&
          array(resource.subjectTags).indexOf(state.subject) < 0) return false;
      if (ignored !== 'informationType' && state.informationType &&
          array(resource.informationTypeTags).indexOf(state.informationType) < 0) return false;
      if (ignored !== 'resourceType' && state.resourceType &&
          resource.resourceType !== state.resourceType) return false;
      if (ignored !== 'query' && scoreResource(resource, state) === null) return false;
      return true;
    }

    function compareTitle(a, b) {
      var titleA = normalizeText(a.resource.title);
      var titleB = normalizeText(b.resource.title);
      if (titleA < titleB) return -1;
      if (titleA > titleB) return 1;
      return String(a.resource.id).localeCompare(String(b.resource.id));
    }

    function resultItems(stateInput, ignored) {
      var state = createState(stateInput);
      var items = sourceResources.map(function(resource) {
        return { resource: resource, score: scoreResource(resource, state) };
      }).filter(function(item) {
        return item.score !== null && dimensionMatch(item.resource, state, ignored || '');
      });
      if (state.sortMode === 'provider') {
        items.sort(function(a, b) {
          return normalizeText(a.resource.provider).localeCompare(
            normalizeText(b.resource.provider)
          ) || compareTitle(a, b);
        });
      } else if (state.sortMode === 'relevance' &&
                 (normalizeText(state.query) || state.productContextId)) {
        items.sort(function(a, b) { return b.score - a.score || compareTitle(a, b); });
      } else {
        items.sort(compareTitle);
      }
      return items;
    }

    function results(state) {
      return resultItems(state).map(function(item) { return item.resource; });
    }

    function uniqueSorted(values) {
      return values.filter(function(value, index, all) {
        return value && all.indexOf(value) === index;
      }).sort(function(a, b) { return normalizeText(a).localeCompare(normalizeText(b)); });
    }

    function countValues(state, dimension, valuesForResource) {
      var counts = {};
      sourceResources.forEach(function(resource) {
        if (!dimensionMatch(resource, createState(state), dimension)) return;
        uniqueSorted(valuesForResource(resource)).forEach(function(value) {
          counts[value] = (counts[value] || 0) + 1;
        });
      });
      return counts;
    }

    function facetCounts(state) {
      var presets = {};
      presetDefinitions.forEach(function(preset) {
        var candidate = createState(state);
        candidate.preset = preset.id;
        presets[preset.id] = sourceResources.filter(function(resource) {
          return dimensionMatch(resource, candidate, '');
        }).length;
      });
      return {
        presets: presets,
        providers: providerCounts(state),
        subjects: countValues(state, 'subject', function(resource) {
          return array(resource.subjectTags);
        }),
        informationTypes: countValues(state, 'informationType', function(resource) {
          return array(resource.informationTypeTags);
        }),
        resourceTypes: countValues(state, 'resourceType', function(resource) {
          return [resource.resourceType];
        })
      };
    }

    function providerOptions(state, counts) {
      counts = counts || providerCounts(state);
      return providerPolicy.families.map(function(family) {
        var value = 'family:' + family.id;
        return { value: value, label: family.label, group: family.group, count: counts[value] || 0 };
      });
    }

    function chips(stateInput) {
      var state = createState(stateInput);
      var output = [];
      state.providers.slice().sort(function(a, b) {
        return normalizeText(a).localeCompare(normalizeText(b));
      }).forEach(function(value) {
        var family = providerFamily(value);
        var exact = sourceResources.some(function(resource) { return resource.provider === value; });
        output.push({ key: 'provider', value: value,
          label: family ? family.label : (exact ? 'Exact provider: ' : 'Unavailable provider: ') + value
        });
      });
      [
        ['subject', state.subject, 'Subject'],
        ['informationType', state.informationType, 'Information Type'],
        ['resourceType', state.resourceType, 'Resource type', resourceTypeLabel]
      ].forEach(function(definition) {
        if (definition[1]) output.push({
          key: definition[0], value: definition[1],
          label: definition[2] + ': ' + (
            definition[3] ? definition[3](definition[1]) : definition[1]
          )
        });
      });
      return output;
    }

    function removeChip(stateInput, key, value) {
      var state = createState(stateInput);
      if (key === 'provider') state.providers = state.providers.filter(function(item) {
        return item !== value;
      });
      else if (Object.prototype.hasOwnProperty.call(state, key)) state[key] = '';
      state.selectedId = '';
      state.renderLimit = pageSize;
      state.pane = 'results';
      state.focusKey = 'resource-search';
      return state;
    }

    function clearProviders(stateInput) {
      var state = createState(stateInput);
      state.providers = [];
      state.providerQuery = '';
      state.selectedId = '';
      state.renderLimit = pageSize;
      state.pane = 'results';
      state.focusKey = 'resource-provider-family%3Ablm';
      return state;
    }

    function reset() {
      var state = createState({ productContextId: '' });
      state.focusKey = 'resource-search';
      return state;
    }

    function setQuery(stateInput, query) {
      var state = createState(stateInput);
      var hadMeaning = Boolean(normalizeText(state.query) || state.productContextId);
      state.query = String(query || '');
      var hasMeaning = Boolean(normalizeText(state.query) || state.productContextId);
      if ((!hadMeaning && hasMeaning && state.sortMode === 'title') ||
          (hadMeaning && !hasMeaning && state.sortMode === 'relevance')) {
        state.sortMode = hasMeaning ? 'relevance' : 'title';
      }
      state.selectedId = '';
      state.renderLimit = pageSize;
      state.pane = 'results';
      return state;
    }

    function toggleProvider(stateInput, provider) {
      var state = createState(stateInput);
      state.providers = state.providers.indexOf(provider) >= 0
        ? state.providers.filter(function(value) { return value !== provider; })
        : state.providers.concat(provider);
      state.selectedId = '';
      state.renderLimit = pageSize;
      state.pane = 'results';
      state.focusKey = providerFocusKey(provider);
      return state;
    }

    function setPreset(stateInput, presetId) {
      var state = createState(stateInput);
      state.preset = presetDefinitions.some(function(preset) {
        return preset.id === presetId;
      }) ? presetId : 'all_resources';
      state.selectedId = '';
      state.renderLimit = pageSize;
      state.pane = 'results';
      state.focusKey = 'resource-preset-' + state.preset;
      return state;
    }

    function patchState(stateInput, patch) {
      var state = createState(stateInput);
      Object.keys(patch || {}).forEach(function(key) {
        if (Object.prototype.hasOwnProperty.call(state, key)) state[key] = patch[key];
      });
      state = createState(state);
      state.selectedId = patch && Object.prototype.hasOwnProperty.call(patch, 'selectedId')
        ? String(patch.selectedId || '') : '';
      state.renderLimit = patch && Object.prototype.hasOwnProperty.call(patch, 'renderLimit')
        ? Math.max(pageSize, Number(patch.renderLimit) || pageSize) : pageSize;
      return state;
    }

    function selectResource(stateInput, resourceId) {
      var state = createState(stateInput);
      if (!sourceResources.some(function(resource) { return resource.id === resourceId; })) {
        return state;
      }
      state.selectedId = resourceId;
      state.pane = 'detail';
      state.focusKey = 'resource-result-' + resourceId;
      state.selectedFocusKey = state.focusKey;
      state.detailScrollTop = 0;
      return state;
    }

    function detail(stateInput) {
      var state = createState(stateInput);
      if (!state.selectedId) return null;
      return sourceResources.find(function(resource) {
        return resource.id === state.selectedId;
      }) || null;
    }

    function accessPointGroups(resource) {
      var points = array(resource && resource.accessPoints).slice();
      var canonicalUrl = String(resource && resource.canonicalUrl || '');
      var official = points.find(function(point) {
        return point.role === 'canonical' && point.url === canonicalUrl;
      }) || null;
      return {
        official: official,
        additional: points.filter(function(point) { return point !== official; })
      };
    }

    function showMore(stateInput) {
      var state = createState(stateInput);
      state.renderLimit += pageSize;
      state.focusKey = 'resource-show-more';
      return state;
    }

    function sortOptions(stateInput) {
      var state = createState(stateInput);
      return normalizeText(state.query) || state.productContextId
        ? ['relevance', 'title', 'provider']
        : ['title', 'provider'];
    }

    function escape(stateInput) {
      var state = createState(stateInput);
      if (state.pane === 'detail' || state.selectedId) {
        var selectedFocusKey = state.selectedFocusKey ||
          (state.selectedId ? 'resource-result-' + state.selectedId : 'resource-search');
        state.selectedId = '';
        state.pane = 'results';
        state.resultsScrollTop = state.returnResultsScrollTop;
        state.detailScrollTop = 0;
        state.focusKey = selectedFocusKey;
        return { action: 'nested', state: state };
      }
      if (state.pane === 'facets' || state.facetsOpen) {
        state.pane = 'results';
        state.facetsOpen = false;
        state.focusKey = 'resource-facets-toggle';
        return { action: 'nested', state: state };
      }
      return { action: 'close-guide', state: state };
    }

    function snapshot(stateInput) {
      return stateCopy(createState(stateInput));
    }

    function restore(saved) {
      return createState(stateCopy(saved || {}));
    }

    function createLifecycle() {
      var attached = false;
      var destroyed = false;
      return Object.freeze({
        attach: function() {
          if (attached || destroyed) return false;
          attached = true;
          return true;
        },
        detach: function() {
          if (destroyed) return false;
          attached = false;
          destroyed = true;
          return true;
        },
        state: function() { return { attached: attached, destroyed: destroyed }; }
      });
    }

    return Object.freeze({
      pageSize: pageSize,
      normalize: normalizeText,
      createState: createState,
      results: results,
      resultItems: resultItems,
      facetCounts: facetCounts,
      providerOptions: providerOptions,
      providerGroups: providerGroups,
      providerMembership: providerMembership,
      providerFocusKey: providerFocusKey,
      providerPolicy: function() { return JSON.parse(JSON.stringify(providerPolicy)); },
      chips: chips,
      removeChip: removeChip,
      clearProviders: clearProviders,
      reset: reset,
      setQuery: setQuery,
      toggleProvider: toggleProvider,
      setPreset: setPreset,
      patchState: patchState,
      selectResource: selectResource,
      detail: detail,
      accessPointGroups: accessPointGroups,
      resourceTypeLabel: resourceTypeLabel,
      showMore: showMore,
      sortOptions: sortOptions,
      escape: escape,
      snapshot: snapshot,
      restore: restore,
      createLifecycle: createLifecycle,
      presets: presetDefinitions.map(function(preset) { return Object.assign({}, preset); })
    });
  }

  var products = asArray(bundle.products);
  var articles = asArray(bundle.articles);
  var resources = asArray(bundle.resources);
  var resourceExplorerModel = ptCreateResourceExplorerModel(resources);
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
  function quickProductIds(item) {
    return item.entryKind === 'collection' ? asArray(item.memberIds) : asArray(item.productId);
  }
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
      brimSection: [],
      subject: [],
      informationType: []
    },
    selectedId: '',
    quickIds: [],
    quickId: '',
    quickLabel: '',
    previousFocus: null,
    history: [],
    resourceExplorer: resourceExplorerModel.createState(),
    compactSnapshot: null,
    resourceEntryFocusKey: ''
  };
  var pendingResourceResultAlignment = '';

  function guideFilterOwner(section) {
    if (section === 'explore') return 'products';
    if (section === 'resources') return 'resources';
    return '';
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
    var searchTerms = asArray(record.searchTerms);
    var structured = record.kind === 'Product' ? '' : structuredText(record);
    var score = 0;

    score = Math.max(score, maxFieldScore(query, [record.title], 1200, 1000, 520));
    score = Math.max(score, maxFieldScore(query, aliases, 1100, 900, 500));
    if (record.kind === 'Product' && normalize(record.pathLabel) === query) score = Math.max(score, 1050);
    score = Math.max(score, maxFieldScore(query, asArray(record.subjectTags), 650, 620, 410));
    score = Math.max(score, maxFieldScore(query, asArray(record.informationTypes), 600, 570, 390));
    score = Math.max(score, maxFieldScore(query, [record.provider].concat(searchTerms), 420, 390, 260));
    score = Math.max(score, maxFieldScore(query, [record.summary], 250, 220, 150));
    score = Math.max(score, maxFieldScore(query, [structured], 210, 190, 140));

    asArray(record.relatedResourceIds).forEach(function(resourceId) {
      var resource = resourcesById[resourceId];
      if (resource) score = Math.max(score, maxFieldScore(query, [resource.title], 120, 110, 90));
    });

    var queryWords = words(query);
    if (queryWords.length > 1) {
      var searchable = normalize([
        record.title,
        aliases.join(' '),
        asArray(record.subjectTags).join(' '),
        asArray(record.informationTypes).join(' '),
        record.provider,
        searchTerms.join(' '),
        record.summary,
        structured
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
    return Object.keys(state.filters).some(function(key) { return state.filters[key].length > 0; });
  }

  function productMatchesFilters(product) {
    if (state.filters.brimSection.length && state.filters.brimSection.indexOf(product.brimSection) < 0) return false;
    if (state.filters.subject.length && asArray(product.subjectTags).indexOf(state.filters.subject[0]) < 0) return false;
    if (state.filters.informationType.length && asArray(product.informationTypes).indexOf(state.filters.informationType[0]) < 0) return false;
    return true;
  }

  function filteredProducts() {
    return products.filter(productMatchesFilters);
  }

  function visibleProducts() {
    var filtered = filteredProducts();
    if (!state.query) return filtered;
    var matched = {};
    search(state.query).forEach(function(record) {
      if (record.kind === 'Product') matched[record.id] = true;
    });
    return filtered.filter(function(product) { return matched[product.id]; });
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
  var resourceSpine = node('div', 'brim-guide__resource-spine');
  resourceSpine.hidden = true;
  resourceSpine.appendChild(node('span', 'brim-guide__resource-spine-letter', 'C'));
  resourceSpine.appendChild(button(
    'brim-guide__resource-spine-back',
    'Back to Guide',
    'resource-back-guide',
    'Back to compact BRIM Guide'
  ));
  rail.appendChild(leftClose);
  rail.appendChild(resourceSpine);
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
    var quickButton = button('brim-guide__quick-item', '', 'quick');
    quickButton.setAttribute('data-guide-quick', item.id);
    quickButton.setAttribute('data-guide-products', quickProductIds(item).join(','));
    quickButton.setAttribute('data-guide-entry-kind', item.entryKind);
    quickButton.setAttribute('data-guide-entry-label', item.label);
    quickButton.appendChild(node('span', 'brim-guide__quick-label', item.label));
    quickButton.appendChild(node('span', 'brim-guide__quick-type', item.typeLabel));
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
  var searchStack = node('div', 'brim-guide__search-stack');
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
  var activeSummary = node('div', 'brim-guide__active-filters');
  activeSummary.setAttribute('aria-live', 'polite');
  activeSummary.hidden = true;
  var rightClose = button(
    'brim-guide__close brim-guide__close--right',
    '×',
    'close',
    'Close BRIM Guide'
  );
  searchStack.appendChild(searchLabel);
  searchStack.appendChild(searchInput);
  searchStack.appendChild(activeSummary);
  utility.appendChild(searchStack);
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

  function copyFilters() {
    return {
      brimSection: state.filters.brimSection.slice(),
      subject: state.filters.subject.slice(),
      informationType: state.filters.informationType.slice()
    };
  }

  function snapshot() {
    return {
      section: state.section,
      view: state.view,
      query: state.query,
      filters: copyFilters(),
      selectedId: state.selectedId,
      quickIds: state.quickIds.slice(),
      quickId: state.quickId,
      quickLabel: state.quickLabel,
      resourceExplorerState: resourceExplorerModel.snapshot(state.resourceExplorer),
      scrollTop: main.scrollTop
    };
  }

  function snapshotCompactGuide(triggerFocusKey) {
    var saved = snapshot();
    saved.history = state.history.map(function(item) {
      return Object.assign({}, item, {
        filters: {
          brimSection: item.filters.brimSection.slice(),
          subject: item.filters.subject.slice(),
          informationType: item.filters.informationType.slice()
        },
        quickIds: item.quickIds.slice(),
        resourceExplorerState: resourceExplorerModel.snapshot(item.resourceExplorerState)
      });
    });
    saved.triggerFocusKey = triggerFocusKey || '';
    return saved;
  }

  function restore(saved) {
    Object.keys(saved).forEach(function(key) {
      if (key !== 'scrollTop' && key !== 'triggerFocusKey' &&
          key !== 'resourceExplorerState') state[key] = saved[key];
    });
    state.resourceExplorer = resourceExplorerModel.restore(saved.resourceExplorerState);
    searchInput.value = state.query;
    render();
    main.scrollTop = saved.scrollTop || 0;
  }

  function setSearchPresentation(resourceMode, resourceGatewayMode) {
    searchLabel.textContent = resourceGatewayMode
      ? 'Search Resource titles, providers, subjects, variables, and places'
      : (resourceMode
        ? 'Search Resources'
        : 'Search layers, tools, methods, resources, and updates');
    searchInput.placeholder = resourceGatewayMode
      ? 'Search Resource titles, providers, subjects, variables, and places'
      : (resourceMode
        ? 'Search titles, providers, subjects, variables, and places'
        : 'Search layers, tools, methods, resources, and updates');
    searchInput.setAttribute('aria-label', searchLabel.textContent);
    if (resourceGatewayMode) {
      searchInput.setAttribute('data-guide-focus-key', 'resource-gateway-search');
    } else if (resourceMode) {
      searchInput.setAttribute('data-guide-focus-key', 'resource-search');
    }
    else searchInput.removeAttribute('data-guide-focus-key');
    searchInput.value = resourceMode ? state.resourceExplorer.query : state.query;
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

  function sectionHeading(number, title, detail, liveDetail) {
    var wrap = node('div', 'brim-guide__section-heading');
    wrap.appendChild(node('span', 'brim-guide__section-number', number));
    wrap.appendChild(node('h2', '', title));
    if (detail) {
      var detailNode = node('span', 'brim-guide__section-detail', detail);
      if (liveDetail) {
        detailNode.setAttribute('role', 'status');
        detailNode.setAttribute('aria-live', 'polite');
        detailNode.setAttribute('aria-atomic', 'true');
      }
      wrap.appendChild(detailNode);
    }
    return wrap;
  }

  function recordType(record) {
    if (record.kind === 'Product') return record.entityType || 'Layer';
    if (record.kind === 'Article') return 'Method';
    return record.kind || 'Resource';
  }

  function productResultContext(record) {
    return recordType(record) === 'Tool'
      ? (record.summary || record.accessHint || '')
      : (record.pathLabel || record.summary || '');
  }

  function resultRow(record) {
    var row = button('brim-guide__result', '', 'record');
    row.setAttribute('data-guide-record', record.id);
    resourceFocusKey(row, 'guide-record-' + record.id);
    row.appendChild(node('span', 'brim-guide__result-kind', recordType(record)));
    row.appendChild(node('strong', 'brim-guide__result-title', record.title));
    var context = record.kind === 'Product'
      ? productResultContext(record)
      : (record.section || record.provider || record.date || 'BRIM Guide');
    row.appendChild(node('span', 'brim-guide__result-path', context));
    var summary = record.kind === 'Product'
      ? [record.provider, record.summary === context ? '' : record.summary]
          .filter(Boolean).join(' · ')
      : record.summary;
    if (summary) row.appendChild(node('span', 'brim-guide__result-summary', summary));
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
    var masthead = node('p', 'brim-guide__eyebrow brim-guide__masthead');
    String(bundle.identity.expanded_name || '').split(' ').forEach(function(word, index) {
      if (index) masthead.appendChild(document.createTextNode(' '));
      if (index < 4 && word) {
        masthead.appendChild(node('span', 'brim-guide__masthead-acronym', word.charAt(0)));
        masthead.appendChild(document.createTextNode(word.slice(1)));
      } else {
        masthead.appendChild(document.createTextNode(word));
      }
    });
    identity.appendChild(masthead);
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
        if (value && value !== 'Tool / Workflow' && values.indexOf(value) < 0) values.push(value);
      });
    });
    if (field === 'brimSection') {
      return ['Local', 'Ops Live', 'External', 'Tools'].filter(function(value) {
        return values.indexOf(value) >= 0;
      });
    }
    if (field === 'informationType') {
      return [
        'Static Reference', 'Live Observation', 'Forecast / Outlook',
        'Model / Simulation', 'Historical Context', 'Screening / Derived',
        'External On-Demand Service'
      ].filter(function(value) { return values.indexOf(value) >= 0; });
    }
    return values.sort(function(a, b) { return a.localeCompare(b); });
  }

  function facetGroup(field, label, values) {
    var wrap = node('section', 'brim-guide__facet');
    wrap.appendChild(node('h3', '', label));
    var options = node('div', 'brim-guide__facet-options');
    var radioGroup = field === 'brimSection';
    var selectedValue = state.filters[field][0] || '';
    options.setAttribute('role', radioGroup ? 'radiogroup' : 'group');
    options.setAttribute('aria-label', label);
    values.forEach(function(value, index) {
      var selected = state.filters[field].indexOf(value) >= 0;
      var option = button('brim-guide__facet-button', value, 'facet-toggle');
      option.setAttribute('data-guide-filter', field);
      option.setAttribute('data-guide-value', value);
      if (radioGroup) {
        option.setAttribute('role', 'radio');
        option.setAttribute('aria-checked', selected ? 'true' : 'false');
        option.tabIndex = selected || (!selectedValue && index === 0) ? 0 : -1;
      } else {
        option.setAttribute('aria-pressed', selected ? 'true' : 'false');
      }
      option.classList.toggle('is-selected', selected);
      options.appendChild(option);
    });
    wrap.appendChild(options);
    return wrap;
  }

  function renderActiveSummary() {
    activeSummary.replaceChildren();
    var labels = { brimSection: 'Where in BRIM', subject: 'Primary Subject', informationType: 'Information Type' };
    Object.keys(labels).forEach(function(field) {
      state.filters[field].forEach(function(value) {
        var chip = button(
          'brim-guide__filter-chip', '', 'filter-remove',
          'Remove ' + labels[field] + ': ' + value + ' filter'
        );
        chip.setAttribute('data-guide-filter', field);
        chip.setAttribute('data-guide-value', value);
        chip.appendChild(node('span', '', value));
        var remove = node('span', 'brim-guide__filter-chip-x', '×');
        remove.setAttribute('aria-hidden', 'true');
        chip.appendChild(remove);
        activeSummary.appendChild(chip);
      });
    });
    if (hasActiveFilters()) {
      activeSummary.appendChild(button(
        'brim-guide__clear-all', 'Clear all', 'clear-all',
        'Clear all A Explore filters'
      ));
    }
    activeSummary.hidden = !hasActiveFilters();
  }

  function renderFilters() {
    var browse = node('section', 'brim-guide__browse');
    browse.appendChild(sectionHeading('01', 'Browse BRIM layers & tools', visibleProducts().length + ' of ' + products.length));
    var controls = node('div', 'brim-guide__filters');
    controls.appendChild(facetGroup('brimSection', 'Where in BRIM', filterValues('brimSection')));
    controls.appendChild(facetGroup('subject', 'Primary Subject', filterValues('subject')));
    controls.appendChild(facetGroup('informationType', 'Information Type', filterValues('informationType')));
    browse.appendChild(controls);
    return browse;
  }

  function renderExplore() {
    var fragment = document.createDocumentFragment();

    if (!state.query) {
      fragment.appendChild(renderIntro());
      fragment.appendChild(node('p', 'brim-guide__scope-note', bundle.identity.scope_note));
    }
    var directory = node('div', 'brim-guide__explore-directory');
    var browse = renderFilters();
    if (state.query) {
      var searchResults = search(state.query);
      if (hasActiveFilters()) {
        searchResults = searchResults.filter(function(record) {
          return record.kind === 'Product' && productMatchesFilters(record);
        });
      }
      browse.appendChild(sectionHeading('', 'Search matches', searchResults.length + ' records'));
      browse.appendChild(renderResults(
        searchResults,
        'No Guide records match that search. Try a shorter source-supported term.'
      ));
    }
    directory.appendChild(browse);
    var filtered = visibleProducts();
    var productSection = node('section', 'brim-guide__product-index');
    var subsetStatus = filtered.length + ' of ' + products.length;
    if (filtered.length !== products.length) subsetStatus += ' shown · filtered';
    productSection.appendChild(sectionHeading(
      '02', 'All Layers & Tools A–Z', subsetStatus, true
    ));
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
    var entityType = recordType(product);
    fragment.appendChild(button('brim-guide__back', '← Back', 'back'));
    fragment.appendChild(pageHeading(
      entityType,
      product.title,
      entityType === 'Tool' ? '' : product.summary,
      'product'
    ));

    var locator = node('section', 'brim-guide__locator');
    if (entityType === 'Tool') {
      locator.appendChild(node('h2', '', 'What this tool does'));
      locator.appendChild(node('p', 'brim-guide__path', product.summary));
    } else if (product.pathLabel) {
      locator.appendChild(node('h2', '', 'Find in layer list'));
      locator.appendChild(node('p', 'brim-guide__path', product.pathLabel));
      locator.appendChild(node(
        'p',
        'brim-guide__boundary',
        'Use this exact path in the existing map controls. BRIM Guide explains this item but does not change map state.'
      ));
    } else {
      locator.appendChild(node('h2', '', 'About this layer'));
      locator.appendChild(node('p', 'brim-guide__path', product.summary));
    }
    fragment.appendChild(locator);

    if (entityType === 'Tool' && product.accessHint) {
      var access = node('section', 'brim-guide__locator');
      access.appendChild(node('h2', '', 'How to open it'));
      access.appendChild(node('p', 'brim-guide__path', product.accessHint));
      fragment.appendChild(access);
    }

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
          var relatedButton = button(
            'brim-guide__related-item', '', 'resource-open-context'
          );
          relatedButton.setAttribute('data-resource-product-context', product.id);
          resourceFocusKey(
            relatedButton,
            'product-resource-' + product.id + '-' + resource.id
          );
          relatedButton.appendChild(node('span', 'brim-guide__related-title', resource.title));
          relatedButton.appendChild(node(
            'span', 'brim-guide__related-role',
            relationshipRoleLabel(relationship.relationshipRole)
          ));
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
    if (record.kind === 'Resource' && record.canonicalUrl) {
      var link = node('a', 'brim-guide__resource-link', 'Open official resource ↗');
      link.href = record.canonicalUrl;
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

  function resourceFocusKey(element, key) {
    if (element && key) element.setAttribute('data-guide-focus-key', key);
    return element;
  }

  function resourceCountText(count) {
    var numericCount = Math.max(0, Number(count) || 0);
    return numericCount + (numericCount === 1 ? ' Resource' : ' Resources');
  }

  function appendResourceFacetLabel(control, label, count) {
    control.appendChild(node(
      'span', 'brim-guide__resource-facet-label', label
    ));
    var badge = node(
      'span', 'brim-guide__resource-facet-count', String(Math.max(0, Number(count) || 0))
    );
    badge.setAttribute('aria-hidden', 'true');
    control.appendChild(badge);
  }

  function renderResourceGateway() {
    var fragment = document.createDocumentFragment();
    var gatewayState = resourceExplorerModel.createState();
    var counts = resourceExplorerModel.facetCounts(gatewayState).presets;
    fragment.appendChild(pageHeading(
      'C · Resources',
      'Resource Explorer',
      'Datasets, viewers, portals, and official sources linked to BRIM or useful beyond it. ' +
        'For layers, live products, and tools available in BRIM, use A · Explore.'
    ));
    var gateway = node('section', 'brim-guide__resource-gateway');
    gateway.appendChild(node(
      'p',
      'brim-guide__resource-gateway-count',
      resources.length + ' published Resources in the current BRIM Guide.'
    ));
    var actions = node('div', 'brim-guide__resource-gateway-actions');
    [
      ['in_brim_map', 'In BRIM map', counts.in_brim_map, false,
        'View ' + counts.in_brim_map + ' Resources represented in the BRIM map'],
      ['beyond_the_map', 'Beyond the map', counts.beyond_the_map, false,
        'View ' + counts.beyond_the_map + ' Resources reviewed as beyond the map'],
      ['all_resources', 'All Resources', counts.all_resources, true,
        'View all ' + counts.all_resources + ' Resources']
    ].forEach(function(definition) {
      var action = button(
        'brim-guide__resource-gateway-action', '', 'resource-open', definition[4]
      );
      action.setAttribute('data-resource-preset', definition[0]);
      action.setAttribute('data-resource-focus-search', definition[3] ? 'true' : 'false');
      resourceFocusKey(action, 'resource-gateway-' + definition[0]);
      action.appendChild(node('strong', '', definition[1]));
      action.appendChild(node('span', '', definition[2] + ' Resources'));
      actions.appendChild(action);
    });
    gateway.appendChild(actions);
    fragment.appendChild(gateway);
    return fragment;
  }

  function sortedCountKeys(counts) {
    return Object.keys(counts || {}).sort(function(a, b) {
      return resourceExplorerModel.normalize(a).localeCompare(
        resourceExplorerModel.normalize(b)
      );
    });
  }

  function resourceFacetChoices(label, field, value, counts) {
    var section = node('section', 'brim-guide__resource-choice-group');
    section.appendChild(node('h3', '', label));
    var choices = node('div', 'brim-guide__resource-choices');
    choices.setAttribute('role', 'group');
    choices.setAttribute('aria-label', label);
    sortedCountKeys(counts).forEach(function(optionValue) {
      var selected = optionValue === value;
      var count = counts[optionValue];
      var choice = button(
        'brim-guide__resource-choice',
        '',
        'resource-facet-choice',
        label + ': ' + optionValue + ', ' + resourceCountText(count)
      );
      choice.setAttribute('data-resource-field', field);
      choice.setAttribute('data-resource-value', optionValue);
      choice.setAttribute('aria-pressed', selected ? 'true' : 'false');
      choice.classList.toggle('is-selected', selected);
      appendResourceFacetLabel(choice, optionValue, count);
      resourceFocusKey(
        choice,
        'resource-facet-' + field + '-' +
          resourceExplorerModel.normalize(optionValue).replace(/ /g, '-')
      );
      choices.appendChild(choice);
    });
    section.appendChild(choices);
    return section;
  }

  function resourceFacetSelect(label, field, value, counts, emptyLabel, valueLabel) {
    var section = node('section', 'brim-guide__resource-select-group');
    var selectLabel = node('label', 'brim-guide__resource-filter-label');
    selectLabel.appendChild(node('span', '', label));
    var select = node('select', 'brim-guide__resource-select');
    select.setAttribute('data-resource-field', field);
    select.setAttribute('aria-label', label);
    resourceFocusKey(select, 'resource-more-' + field);
    var empty = node('option', '', emptyLabel);
    empty.value = '';
    empty.selected = !value;
    select.appendChild(empty);
    sortedCountKeys(counts).forEach(function(optionValue) {
      var visibleValue = valueLabel ? valueLabel(optionValue) : optionValue;
      var option = node('option', '', visibleValue + ' (' + counts[optionValue] + ')');
      option.value = optionValue;
      option.selected = optionValue === value;
      select.appendChild(option);
    });
    selectLabel.appendChild(select);
    section.appendChild(selectLabel);
    return section;
  }

  function renderResourceProviders(resourceState, counts) {
    var providerBlock = node('section', 'brim-guide__resource-provider');
    var providerHeader = node('div', 'brim-guide__resource-provider-heading');
    providerHeader.appendChild(node('h3', '', 'Selected providers'));
    var providerClear = button(
      'brim-guide__text-button', 'Clear', 'resource-provider-clear', 'Clear providers only'
    );
    providerClear.disabled = !resourceState.providers.length;
    providerHeader.appendChild(providerClear);
    providerBlock.appendChild(providerHeader);
    providerBlock.appendChild(node(
      'p', 'brim-guide__resource-provider-help', 'Other providers remain in results and searchable above.'
    ));
    var options = resourceExplorerModel.providerOptions(resourceState, counts);
    resourceExplorerModel.providerGroups().forEach(function(group) {
      var section = node('fieldset', 'brim-guide__resource-provider-group');
      section.appendChild(node('legend', '', group.label));
      var panel = node('div', 'brim-guide__resource-provider-options');
      options.filter(function(option) { return option.group === group.id; }).forEach(function(option) {
        var label = node('label', 'brim-guide__resource-provider-option');
        var checkbox = node('input', '');
        checkbox.type = 'checkbox';
        checkbox.id = 'brim-guide-provider-' + option.value.slice('family:'.length);
        checkbox.checked = resourceState.providers.indexOf(option.value) >= 0;
        // All declared choices stay focusable at zero; counts never determine the roster.
        checkbox.setAttribute('data-resource-provider', option.value);
        checkbox.setAttribute('aria-label', option.label + ', ' + resourceCountText(option.count));
        resourceFocusKey(checkbox, resourceExplorerModel.providerFocusKey(option.value));
        label.setAttribute('for', checkbox.id);
        label.appendChild(checkbox);
        label.appendChild(node('span', '', option.label));
        var count = node('small', '', String(option.count));
        count.setAttribute('aria-hidden', 'true');
        label.appendChild(count);
        panel.appendChild(label);
      });
      section.appendChild(panel);
      providerBlock.appendChild(section);
    });
    return providerBlock;
  }

  function renderResourceFacets(resourceState, counts) {
    var facets = node('aside', 'brim-guide__resource-facets');
    facets.id = 'brim-guide-resource-facets';
    facets.setAttribute('aria-label', 'Resource filters');
    facets.classList.toggle('is-open', resourceState.facetsOpen);
    facets.appendChild(button(
      'brim-guide__resource-pane-back', '← Results', 'resource-pane-results'
    ));
    var heading = node('div', 'brim-guide__resource-facet-heading');
    heading.appendChild(node('h2', '', 'Refine Resources'));
    facets.appendChild(heading);

    // The filter body owns desktop scrolling; the action row reserves its own space.
    var body = node('div', 'brim-guide__resource-filter-body');
    body.appendChild(renderResourceProviders(resourceState, counts.providers));
    var choiceStack = node('div', 'brim-guide__resource-facet-choices');
    choiceStack.appendChild(resourceFacetChoices(
      'Subject', 'subject', resourceState.subject, counts.subjects
    ));
    choiceStack.appendChild(resourceFacetChoices(
      'Information Type', 'informationType', resourceState.informationType,
      counts.informationTypes
    ));
    body.appendChild(choiceStack);
    var morePanel = node('div', 'brim-guide__resource-more-panel');
    morePanel.id = 'brim-guide-resource-more-panel';
    morePanel.hidden = !resourceState.moreFiltersOpen;
    morePanel.appendChild(resourceFacetSelect(
      'Resource type', 'resourceType', resourceState.resourceType,
      counts.resourceTypes, 'All Resource types', resourceExplorerModel.resourceTypeLabel
    ));
    body.appendChild(morePanel);
    facets.appendChild(body);

    var actions = node('div', 'brim-guide__resource-filter-actions');
    var moreToggle = button(
      'brim-guide__resource-more-toggle', 'More filters', 'resource-more-filters'
    );
    moreToggle.setAttribute('aria-expanded', resourceState.moreFiltersOpen ? 'true' : 'false');
    moreToggle.setAttribute('aria-controls', morePanel.id);
    resourceFocusKey(moreToggle, 'resource-more-filters');
    actions.appendChild(moreToggle);
    actions.appendChild(button('brim-guide__text-button', 'Reset all', 'resource-reset'));
    facets.appendChild(actions);
    return facets;
  }

  function resourceRepresentation(resource) {
    var definitions = {
      direct_match_in_brim: {
        label: 'Direct match in BRIM',
        sentence: 'This Resource is represented directly in the BRIM map.',
        className: 'brim-guide__resource-representation--direct'
      },
      selected_products_in_brim: {
        label: 'Selected products in BRIM',
        sentence: 'BRIM includes selected products from this broader Resource.',
        className: 'brim-guide__resource-representation--selected'
      },
      not_currently_mapped_in_brim: {
        label: 'Not currently mapped in BRIM',
        sentence: 'This Resource is not currently represented in the BRIM map.',
        className: 'brim-guide__resource-representation--beyond'
      }
    };
    return definitions[resource.mapRepresentation] || null;
  }

  function deliveryLabel(value) {
    return {
      provider_hosted: 'Provider-hosted',
      brim_enhanced: 'BRIM-enhanced',
      brim_managed: 'BRIM-managed',
      not_applicable: 'Not applicable'
    }[value] || '';
  }

  function relationshipRoleLabel(value) {
    return {
      direct_match_in_brim: 'Direct match',
      selected_product_from_broader_resource: 'Selected product',
      source_reference: 'Source reference'
    }[value] || '';
  }

  function resourceBadges(resource) {
    var values = [];
    if (asArray(resource.subjectTags).length) values.push(asArray(resource.subjectTags)[0]);
    if (resource.resourceTypeLabel) values.push(resource.resourceTypeLabel);
    if (asArray(resource.accessPoints).length > 1) {
      values.push(asArray(resource.accessPoints).length + ' access points');
    }
    return values;
  }

  function renderResourceResult(resource, selected) {
    var row = button('brim-guide__resource-result', '', 'resource-select');
    row.setAttribute('data-resource-id', resource.id);
    row.setAttribute('aria-pressed', selected ? 'true' : 'false');
    if (selected) row.setAttribute('aria-current', 'true');
    row.classList.toggle('is-selected', selected);
    resourceFocusKey(row, 'resource-result-' + resource.id);
    var header = node('span', 'brim-guide__resource-result-header');
    header.appendChild(node('strong', '', resource.title));
    header.appendChild(node('span', '', resource.provider));
    row.appendChild(header);
    row.appendChild(node('span', 'brim-guide__resource-result-summary', resource.summary));
    var badges = node('span', 'brim-guide__resource-badges');
    if (selected) {
      badges.appendChild(node(
        'span', 'brim-guide__resource-badge brim-guide__resource-selected-label', 'Selected'
      ));
    }
    resourceBadges(resource).forEach(function(value) {
      badges.appendChild(node(
        'span', 'brim-guide__resource-badge', value
      ));
    });
    var representation = resourceRepresentation(resource);
    if (representation) {
      var representationBadge = node(
        'span',
        'brim-guide__resource-badge brim-guide__resource-representation-indicator ' +
          representation.className,
        representation.label
      );
      representationBadge.setAttribute(
        'aria-label', 'Map representation: ' + representation.label
      );
      badges.appendChild(representationBadge);
    }
    row.appendChild(badges);
    return row;
  }

  function appendResourceDetailRow(list, label, value) {
    if (!value || (Array.isArray(value) && !value.length)) return;
    var row = node('div', 'brim-guide__resource-detail-row');
    row.appendChild(node('dt', '', label));
    row.appendChild(node('dd', '', Array.isArray(value) ? value.join(', ') : value));
    list.appendChild(row);
  }

  function renderResourceDetail(resource) {
    var detail = node('aside', 'brim-guide__resource-detail');
    detail.setAttribute('role', 'region');
    detail.setAttribute('aria-label', 'Selected Resource detail');
    detail.setAttribute('data-resource-detail', resource.id);
    detail.appendChild(button(
      'brim-guide__resource-pane-back', '← Results', 'resource-detail-back'
    ));
    detail.appendChild(node('p', 'brim-guide__eyebrow', 'Selected Resource'));
    detail.appendChild(node('h2', '', resource.title));
    detail.appendChild(node('p', 'brim-guide__resource-detail-summary', resource.summary));
    var representation = resourceRepresentation(resource);
    if (representation) {
      var representationStatement = node(
        'section',
        'brim-guide__resource-representation ' + representation.className
      );
      representationStatement.appendChild(node('h3', '', representation.label));
      representationStatement.appendChild(node('p', '', representation.sentence));
      detail.appendChild(representationStatement);
    }
    var accessGroups = resourceExplorerModel.accessPointGroups(resource);
    var access = node('section', 'brim-guide__resource-detail-section brim-guide__resource-access-section');
    access.appendChild(node('h3', '', 'Official Resource'));
    if (accessGroups.official) {
      var officialLink = node(
        'a',
        'brim-guide__resource-access brim-guide__resource-access--primary',
        'Open official resource ↗'
      );
      officialLink.href = accessGroups.official.url;
      officialLink.target = '_blank';
      officialLink.rel = 'noopener noreferrer';
      officialLink.setAttribute('data-resource-access-role', accessGroups.official.role);
      officialLink.setAttribute('aria-label', 'Open official resource in a new tab');
      access.appendChild(officialLink);
    }
    if (accessGroups.additional.length) {
      access.appendChild(node(
        'p', 'brim-guide__resource-access-subheading', 'Additional access points'
      ));
      accessGroups.additional.forEach(function(point) {
        var link = node(
          'a', 'brim-guide__resource-access brim-guide__resource-access--secondary',
          point.label + ' ↗'
        );
        link.href = point.url;
        link.target = '_blank';
        link.rel = 'noopener noreferrer';
        link.setAttribute('data-resource-access-role', point.role);
        access.appendChild(link);
      });
    }
    detail.appendChild(access);

    var list = node('dl', 'brim-guide__resource-detail-list');
    appendResourceDetailRow(list, 'Display provider', resource.provider);
    appendResourceDetailRow(
      list,
      'Providers',
      asArray(resource.providers).map(function(provider) {
        return provider.name + ' — ' + provider.role.replace(/_/g, ' ');
      })
    );
    appendResourceDetailRow(list, 'Subjects', asArray(resource.subjectTags));
    appendResourceDetailRow(list, 'Information Types', asArray(resource.informationTypeTags));
    appendResourceDetailRow(list, 'Resource type', resource.resourceTypeLabel);
    if (resource.temporalCharacter && resource.temporalCharacter !== 'unknown') {
      appendResourceDetailRow(
        list, 'Temporal character', resource.temporalCharacterLabel
      );
    }
    appendResourceDetailRow(list, 'Granularity', resource.resourceGranularity);
    appendResourceDetailRow(list, 'Variables', asArray(resource.variables));
    appendResourceDetailRow(list, 'Use scopes', asArray(resource.useScopes));
    appendResourceDetailRow(
      list,
      'Geography',
      [resource.geographicScope && resource.geographicScope.scopeType !== 'unknown'
        ? resource.geographicScope.scopeLabel : '']
        .concat(asArray(resource.geographicScope && resource.geographicScope.names))
        .filter(Boolean)
    );
    detail.appendChild(list);

    var relationships = asArray(resource.representedProducts);
    if (relationships.length) {
      var related = node('section', 'brim-guide__resource-detail-section');
      related.appendChild(node('h3', '', 'Related BRIM Products'));
      relationships.forEach(function(relationship) {
        var item = button(
          'brim-guide__resource-relationship', '', 'record',
          'Open BRIM Product: ' + relationship.title
        );
        item.setAttribute('data-guide-record', relationship.productId);
        item.appendChild(node('strong', '', relationship.title));
        item.appendChild(node(
          'span', '',
          [relationshipRoleLabel(relationship.relationshipRole),
           'Delivery: ' + deliveryLabel(relationship.deliveryClass)]
            .filter(Boolean).join(' · ')
        ));
        if (relationship.coverageDisposition === 'multiple_source_resources') {
          var sourceTitles = asArray(relationship.sourceResourceIds).map(function(id) {
            return resourcesById[id] && resourcesById[id].title;
          }).filter(Boolean);
          item.appendChild(node(
            'span', 'brim-guide__resource-multiple-source',
            'BRIM combines this Product from multiple sources: ' +
              sourceTitles.join('; ') + '.'
          ));
        }
        related.appendChild(item);
      });
      detail.appendChild(related);
    }

    return detail;
  }

  function renderResourceExplorer() {
    var resourceState = state.resourceExplorer;
    var allResults = resourceExplorerModel.results(resourceState);
    var visibleResults = allResults.slice(0, resourceState.renderLimit);
    var selected = resourceExplorerModel.detail(resourceState);
    var counts = resourceExplorerModel.facetCounts(resourceState);
    var fragment = document.createDocumentFragment();
    var explorer = node('section', 'brim-guide__resource-explorer');
    explorer.setAttribute('data-resource-pane', resourceState.pane);
    explorer.setAttribute('data-resource-preset', resourceState.preset);
    explorer.classList.toggle('has-detail', Boolean(selected));

    var header = node('header', 'brim-guide__resource-explorer-header');
    var headerText = node('div');
    headerText.appendChild(node('p', 'brim-guide__eyebrow', 'C · Resources'));
    var heading = node('h1', '', 'Resource Explorer');
    heading.id = 'brim-guide-title';
    headerText.appendChild(heading);
    if (resourceState.productContextId && productsById[resourceState.productContextId]) {
      headerText.appendChild(node(
        'p',
        'brim-guide__resource-context',
        'Showing exact Resources related to ' + productsById[resourceState.productContextId].title + '.'
      ));
    } else {
      headerText.appendChild(node(
        'p',
        'brim-guide__resource-context',
        'Resources are datasets, viewers, portals, and supporting sources. ' +
          'Use A · Explore for BRIM layers, live products, and tools.'
      ));
    }
    header.appendChild(headerText);
    var facetToggle = button(
      'brim-guide__resource-facets-toggle',
      'Filters',
      'resource-facets-toggle'
    );
    facetToggle.setAttribute('aria-controls', 'brim-guide-resource-facets');
    facetToggle.setAttribute('aria-expanded', resourceState.facetsOpen ? 'true' : 'false');
    resourceFocusKey(facetToggle, 'resource-facets-toggle');
    header.appendChild(facetToggle);
    explorer.appendChild(header);

    var presets = node('div', 'brim-guide__resource-presets');
    presets.setAttribute('role', 'radiogroup');
    presets.setAttribute('aria-label', 'Resource map-presence views');
    resourceExplorerModel.presets.forEach(function(preset) {
      var count = counts.presets[preset.id] || 0;
      var presetButton = button(
        'brim-guide__resource-preset',
        '',
        'resource-preset',
        preset.label + ', ' + resourceCountText(count)
      );
      presetButton.setAttribute('data-resource-preset', preset.id);
      presetButton.setAttribute('role', 'radio');
      presetButton.setAttribute('aria-checked', resourceState.preset === preset.id ? 'true' : 'false');
      presetButton.tabIndex = resourceState.preset === preset.id ? 0 : -1;
      presetButton.classList.toggle('is-selected', resourceState.preset === preset.id);
      appendResourceFacetLabel(presetButton, preset.label, count);
      resourceFocusKey(presetButton, 'resource-preset-' + preset.id);
      presets.appendChild(presetButton);
    });
    explorer.appendChild(presets);

    var chipValues = resourceExplorerModel.chips(resourceState);
    if (chipValues.length) {
      var chips = node('div', 'brim-guide__resource-chips');
      chips.setAttribute('aria-label', 'Active Resource filters');
      chipValues.forEach(function(chipValue) {
        var chip = button(
          'brim-guide__filter-chip',
          chipValue.label + ' ×',
          'resource-chip-remove',
          'Remove ' + chipValue.label + ' filter'
        );
        chip.setAttribute('data-resource-chip-key', chipValue.key);
        chip.setAttribute('data-resource-chip-value', chipValue.value);
        chips.appendChild(chip);
      });
      explorer.appendChild(chips);
    }

    var layout = node('div', 'brim-guide__resource-layout');
    layout.appendChild(renderResourceFacets(resourceState, counts));
    var resultsSection = node('section', 'brim-guide__resource-results');
    resultsSection.setAttribute('aria-label', 'Resource results');
    var resultsHeader = node('div', 'brim-guide__resource-results-header');
    var status = node(
      'p', 'brim-guide__resource-status',
      allResults.length + (allResults.length === 1 ? ' Resource' : ' Resources')
    );
    status.setAttribute('role', 'status');
    status.setAttribute('aria-live', 'polite');
    status.setAttribute('aria-atomic', 'true');
    resultsHeader.appendChild(status);
    var sortLabel = node('label', 'brim-guide__resource-sort');
    sortLabel.appendChild(node('span', '', 'Sort'));
    var sort = node('select', 'brim-guide__resource-select');
    sort.setAttribute('data-resource-field', 'sortMode');
    sort.setAttribute('aria-label', 'Sort Resources');
    resourceExplorerModel.sortOptions(resourceState).forEach(function(value) {
      var label = value === 'relevance' ? 'Relevance' :
        (value === 'provider' ? 'Provider' : 'Title A–Z');
      var option = node('option', '', label);
      option.value = value;
      option.selected = value === resourceState.sortMode;
      sort.appendChild(option);
    });
    sortLabel.appendChild(sort);
    resultsHeader.appendChild(sortLabel);
    resultsSection.appendChild(resultsHeader);
    var resultsScroll = node('div', 'brim-guide__resource-results-scroll');
    resultsScroll.tabIndex = 0;
    resultsScroll.setAttribute('role', 'region');
    resultsScroll.setAttribute('aria-label', 'Scrollable Resource results');
    var resultList = node('div', 'brim-guide__resource-result-list');
    if (!visibleResults.length) {
      var empty = node('div', 'brim-guide__resource-empty');
      empty.appendChild(node('p', '', 'No Resources match the active search and filters.'));
      empty.appendChild(button('brim-guide__text-button', 'Reset all', 'resource-reset'));
      resultList.appendChild(empty);
    } else {
      visibleResults.forEach(function(resource) {
        resultList.appendChild(renderResourceResult(
          resource, resourceState.selectedId === resource.id
        ));
      });
    }
    resultsScroll.appendChild(resultList);
    if (visibleResults.length < allResults.length) {
      var showMore = button(
        'brim-guide__resource-show-more',
        'Show more (' + (allResults.length - visibleResults.length) + ' remaining)',
        'resource-show-more'
      );
      resourceFocusKey(showMore, 'resource-show-more');
      resultsScroll.appendChild(showMore);
    }
    if (selected) {
      var alignmentSpacer = node('div', 'brim-guide__resource-result-alignment-spacer');
      alignmentSpacer.setAttribute('aria-hidden', 'true');
      resultsScroll.appendChild(alignmentSpacer);
    }
    resultsSection.appendChild(resultsScroll);
    var content = node('div', 'brim-guide__resource-content');
    content.appendChild(resultsSection);
    if (selected) content.appendChild(renderResourceDetail(selected));
    layout.appendChild(content);
    explorer.appendChild(layout);
    fragment.appendChild(explorer);
    return fragment;
  }

  function renderSection(sectionName) {
    var definitions = {
      methods: ['B · Methods & Guides', 'Methods & Guides', 'Compact guidance for interpreting and finding BRIM layers and tools.'],
      resources: ['C · Resources', 'Resources', 'Verified reusable agency resources.'],
      updates: ['D · Updates', 'Updates', 'Verified user-facing Guide changes.']
    };
    var definition = definitions[sectionName];
    if (sectionName === 'resources') return renderResourceGateway();
    var collection = sectionName === 'methods'
      ? articles
      : updates;
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
      quickDefinition && quickDefinition.typeLabel ? quickDefinition.typeLabel : 'Collection · Quick Access',
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
      'BRIM is maintained for Bureau of Land Management California water-resource screening and resource-review support. The link below opens a draft in your email application; BRIM Guide does not send or store the message.'
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

  function captureResourceScrollPositions() {
    var existing = main.querySelector('.brim-guide__resource-explorer');
    if (!existing) return;
    var facets = existing.querySelector('.brim-guide__resource-filter-body');
    var results = existing.querySelector('.brim-guide__resource-results-scroll');
    var detail = existing.querySelector('.brim-guide__resource-detail');
    var wide = window.matchMedia('(min-width: 1101px)').matches;
    if (wide) {
      if (facets) state.resourceExplorer.facetScrollTop = facets.scrollTop;
      if (results) state.resourceExplorer.resultsScrollTop = results.scrollTop;
      if (detail) state.resourceExplorer.detailScrollTop = detail.scrollTop;
      return;
    }
    var pane = existing.getAttribute('data-resource-pane') || 'results';
    if (pane === 'facets') state.resourceExplorer.facetScrollTop = main.scrollTop;
    else if (pane === 'detail') state.resourceExplorer.detailScrollTop = main.scrollTop;
    else state.resourceExplorer.resultsScrollTop = main.scrollTop;
  }

  function restoreResourceScrollPositions() {
    var existing = main.querySelector('.brim-guide__resource-explorer');
    if (!existing) return;
    var wide = window.matchMedia('(min-width: 1101px)').matches;
    if (wide) {
      main.scrollTop = 0;
      var facets = existing.querySelector('.brim-guide__resource-filter-body');
      var results = existing.querySelector('.brim-guide__resource-results-scroll');
      var detail = existing.querySelector('.brim-guide__resource-detail');
      if (facets) facets.scrollTop = state.resourceExplorer.facetScrollTop;
      if (results) results.scrollTop = state.resourceExplorer.resultsScrollTop;
      if (detail) detail.scrollTop = state.resourceExplorer.detailScrollTop;
      return;
    }
    var pane = existing.getAttribute('data-resource-pane') || 'results';
    main.scrollTop = pane === 'facets' ? state.resourceExplorer.facetScrollTop
      : (pane === 'detail' ? state.resourceExplorer.detailScrollTop
        : state.resourceExplorer.resultsScrollTop);
  }

  function prepareResourceResultAlignment(resourceId) {
    var wide = window.matchMedia('(min-width: 1101px)').matches;
    pendingResourceResultAlignment = wide ? String(resourceId || '') : '';
    var existing = main.querySelector('.brim-guide__resource-explorer');
    var results = existing && existing.querySelector('.brim-guide__resource-results-scroll');
    var currentResultsScrollTop = wide && results ? results.scrollTop : main.scrollTop;
    state.resourceExplorer.resultsScrollTop = currentResultsScrollTop;
    state.resourceExplorer.returnResultsScrollTop = currentResultsScrollTop;
    state.resourceExplorer.detailScrollTop = 0;
    state.resourceExplorer.selectedFocusKey = resourceId
      ? 'resource-result-' + resourceId : 'resource-search';
  }

  function alignPendingResourceResult() {
    var resourceId = pendingResourceResultAlignment;
    pendingResourceResultAlignment = '';
    if (!resourceId || !window.matchMedia('(min-width: 1101px)').matches) return;
    var existing = main.querySelector('.brim-guide__resource-explorer');
    var results = existing && existing.querySelector('.brim-guide__resource-results-scroll');
    var detail = existing && existing.querySelector('.brim-guide__resource-detail');
    var rows = existing ? existing.querySelectorAll('[data-resource-id]') : [];
    var selectedRow = null;
    for (var index = 0; index < rows.length; index += 1) {
      if (rows[index].getAttribute('data-resource-id') === resourceId) {
        selectedRow = rows[index];
        break;
      }
    }
    if (!results || !selectedRow) return;
    var contentTop = results.getBoundingClientRect().top;
    var rowTop = selectedRow.getBoundingClientRect().top;
    var desiredScrollTop = Math.max(0, results.scrollTop + rowTop - contentTop);
    var alignmentSpacer = results.querySelector(
      '.brim-guide__resource-result-alignment-spacer'
    );
    if (alignmentSpacer) {
      alignmentSpacer.style.height = '0px';
      alignmentSpacer.style.height = Math.ceil(Math.max(
        0,
        desiredScrollTop - Math.max(0, results.scrollHeight - results.clientHeight)
      )) + 'px';
    }
    results.scrollTop = desiredScrollTop;
    state.resourceExplorer.resultsScrollTop = results.scrollTop;
    if (detail) detail.scrollTop = 0;
    state.resourceExplorer.detailScrollTop = 0;
  }

  function render(preserveResourceScroll) {
    var resourceMode = state.view === 'resource-explorer';
    var resourceGatewayMode = state.section === 'resources' && state.view === 'landing';
    var filterOwner = guideFilterOwner(state.section);
    if (resourceMode && preserveResourceScroll !== false) {
      captureResourceScrollPositions();
    }
    root.classList.toggle('brim-guide--resource-explorer', resourceMode);
    resourceSpine.hidden = !resourceMode;
    setSearchPresentation(resourceMode, resourceGatewayMode);
    activeNav();
    if (filterOwner !== 'products') {
      activeSummary.replaceChildren();
      activeSummary.hidden = true;
    } else {
      renderActiveSummary();
    }
    main.replaceChildren();
    if (resourceMode) {
      main.appendChild(renderResourceExplorer());
    } else if (state.view === 'detail') {
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
    if (resourceMode) {
      restoreResourceScrollPositions();
      alignPendingResourceResult();
    }
    else main.scrollTop = 0;
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
    state.filters = { brimSection: [], subject: [], informationType: [] };
    state.selectedId = '';
    state.quickIds = [];
    state.quickId = '';
    state.quickLabel = '';
    state.history = [];
    state.resourceExplorer = resourceExplorerModel.reset(state.resourceExplorer);
    state.compactSnapshot = null;
    state.resourceEntryFocusKey = '';
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

  function restoreResourceScrollAfterFocus() {
    if (state.view !== 'resource-explorer') return;
    restoreResourceScrollPositions();
    window.requestAnimationFrame(function() {
      if (state.view !== 'resource-explorer') return;
      restoreResourceScrollPositions();
      window.requestAnimationFrame(function() {
        if (state.view === 'resource-explorer') restoreResourceScrollPositions();
      });
    });
  }

  function focusResourceTarget(key, reveal) {
    window.setTimeout(function() {
      if (key === 'resource-search') {
        try {
          searchInput.focus({ preventScroll: true });
        } catch (error) {
          searchInput.focus();
        }
        restoreResourceScrollAfterFocus();
        return;
      }
      var candidates = root.querySelectorAll('[data-guide-focus-key]');
      for (var index = 0; index < candidates.length; index += 1) {
        if (candidates[index].getAttribute('data-guide-focus-key') === key &&
            candidates[index].offsetParent !== null) {
          try {
            candidates[index].focus({ preventScroll: !reveal });
          } catch (error) {
            candidates[index].focus();
          }
          if (reveal) captureResourceScrollPositions();
          restoreResourceScrollAfterFocus();
          return;
        }
      }
      if (state.view === 'resource-explorer') {
        try {
          searchInput.focus({ preventScroll: true });
        } catch (error) {
          searchInput.focus();
        }
        restoreResourceScrollAfterFocus();
        return;
      }
      focusMain(true);
    }, 0);
  }

  function openResourceExplorer(options, trigger) {
    options = options || {};
    if (state.view !== 'resource-explorer') {
      var triggerKey = trigger && trigger.getAttribute
        ? trigger.getAttribute('data-guide-focus-key') : '';
      state.compactSnapshot = snapshotCompactGuide(triggerKey);
      state.resourceEntryFocusKey = triggerKey;
    }
    state.section = 'resources';
    state.view = 'resource-explorer';
    state.resourceExplorer = resourceExplorerModel.createState({
      preset: options.preset || 'all_resources',
      query: String(options.query || ''),
      productContextId: options.productContextId || '',
      sortMode: options.productContextId ? 'relevance' : '',
      focusKey: options.focusSearch ? 'resource-search' : 'resource-facets-toggle'
    });
    if (options.selectedId) {
      state.resourceExplorer = resourceExplorerModel.selectResource(
        state.resourceExplorer, options.selectedId
      );
      pendingResourceResultAlignment = window.matchMedia('(min-width: 1101px)').matches
        ? String(options.selectedId) : '';
    }
    render();
    if (options.focusSearch) focusResourceTarget('resource-search');
    else if (options.selectedId) focusMain(false);
    else focusResourceTarget('resource-facets-toggle');
  }

  function backToCompactGuide() {
    var saved = state.compactSnapshot;
    if (!saved) {
      resetHome(false);
      focusMain(false);
      return;
    }
    var triggerKey = saved.triggerFocusKey;
    state.compactSnapshot = null;
    restore(saved);
    focusResourceTarget(triggerKey || 'resource-search');
  }

  function openRecord(id, trigger) {
    if (!recordsById[id]) return;
    if (recordsById[id].kind === 'Resource') {
      openResourceExplorer({ preset: 'all_resources', selectedId: id }, trigger);
      return;
    }
    pushHistory();
    state.view = 'detail';
    state.selectedId = id;
    render();
    focusMain(false);
  }

  function handleRootClick(event) {
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
      openRecord(target.getAttribute('data-guide-record'), target);
    } else if (action === 'resource-open') {
      openResourceExplorer({
        preset: target.getAttribute('data-resource-preset') || 'all_resources',
        focusSearch: target.getAttribute('data-resource-focus-search') === 'true'
      }, target);
    } else if (action === 'resource-open-context') {
      openResourceExplorer({
        preset: 'all_resources',
        productContextId: target.getAttribute('data-resource-product-context') || ''
      }, target);
    } else if (action === 'resource-back-guide') {
      backToCompactGuide();
    } else if (action === 'resource-preset') {
      state.resourceExplorer = resourceExplorerModel.setPreset(
        state.resourceExplorer,
        target.getAttribute('data-resource-preset') || 'all_resources'
      );
      render();
      focusResourceTarget(state.resourceExplorer.focusKey);
    } else if (action === 'resource-facet-choice') {
      var facetField = target.getAttribute('data-resource-field');
      var facetValue = target.getAttribute('data-resource-value') || '';
      var facetPatch = {};
      facetPatch[facetField] = state.resourceExplorer[facetField] === facetValue
        ? '' : facetValue;
      facetPatch.pane = 'results';
      facetPatch.facetsOpen = false;
      facetPatch.renderLimit = 25;
      state.resourceExplorer = resourceExplorerModel.patchState(
        state.resourceExplorer, facetPatch
      );
      render();
      focusResourceTarget(
        'resource-facet-' + facetField + '-' +
          resourceExplorerModel.normalize(facetValue).replace(/ /g, '-')
      );
    } else if (action === 'resource-more-filters') {
      state.resourceExplorer = resourceExplorerModel.patchState(
        state.resourceExplorer,
        {
          moreFiltersOpen: !state.resourceExplorer.moreFiltersOpen,
          pane: state.resourceExplorer.pane,
          facetsOpen: state.resourceExplorer.facetsOpen,
          selectedId: state.resourceExplorer.selectedId,
          renderLimit: state.resourceExplorer.renderLimit
        }
      );
      render();
      focusResourceTarget(state.resourceExplorer.moreFiltersOpen
        ? 'resource-more-resourceType' : 'resource-more-filters', true);
    } else if (action === 'resource-facets-toggle') {
      var openingFacets = !state.resourceExplorer.facetsOpen;
      state.resourceExplorer = resourceExplorerModel.patchState(
        state.resourceExplorer,
        {
          facetsOpen: openingFacets,
          pane: openingFacets ? 'facets' : 'results',
          selectedId: state.resourceExplorer.selectedId,
          renderLimit: state.resourceExplorer.renderLimit
        }
      );
      render();
      focusResourceTarget(openingFacets ? 'resource-provider-family%3Ablm' : 'resource-facets-toggle', true);
    } else if (action === 'resource-pane-results') {
      state.resourceExplorer = resourceExplorerModel.patchState(
        state.resourceExplorer,
        {
          facetsOpen: false,
          pane: 'results',
          selectedId: state.resourceExplorer.selectedId,
          renderLimit: state.resourceExplorer.renderLimit
        }
      );
      render();
      focusResourceTarget('resource-facets-toggle');
    } else if (action === 'resource-detail-back') {
      var detailFocusKey = state.resourceExplorer.selectedFocusKey ||
        (state.resourceExplorer.selectedId
          ? 'resource-result-' + state.resourceExplorer.selectedId : 'resource-search');
      state.resourceExplorer = resourceExplorerModel.patchState(
        state.resourceExplorer,
        {
          selectedId: '', pane: 'results',
          renderLimit: state.resourceExplorer.renderLimit,
          resultsScrollTop: state.resourceExplorer.returnResultsScrollTop,
          detailScrollTop: 0,
          selectedFocusKey: detailFocusKey
        }
      );
      render(false);
      focusResourceTarget(detailFocusKey);
    } else if (action === 'resource-provider-clear') {
      state.resourceExplorer = resourceExplorerModel.clearProviders(state.resourceExplorer);
      render();
      focusResourceTarget('resource-provider-family%3Ablm', true);
    } else if (action === 'resource-reset') {
      state.resourceExplorer = resourceExplorerModel.reset(state.resourceExplorer);
      render(false);
      focusResourceTarget('resource-search');
    } else if (action === 'resource-chip-remove') {
      state.resourceExplorer = resourceExplorerModel.removeChip(
        state.resourceExplorer,
        target.getAttribute('data-resource-chip-key'),
        target.getAttribute('data-resource-chip-value')
      );
      render();
      focusResourceTarget('resource-search');
    } else if (action === 'resource-select') {
      var resourceId = target.getAttribute('data-resource-id');
      prepareResourceResultAlignment(resourceId);
      state.resourceExplorer = resourceExplorerModel.selectResource(
        state.resourceExplorer,
        resourceId
      );
      render(false);
      focusMain(false);
    } else if (action === 'resource-show-more') {
      state.resourceExplorer = resourceExplorerModel.showMore(state.resourceExplorer);
      render();
      focusResourceTarget('resource-show-more');
    } else if (action === 'clear-all') {
      resetHome(true);
    } else if (action === 'facet-toggle') {
      var facetField = target.getAttribute('data-guide-filter');
      var facetValue = target.getAttribute('data-guide-value');
      if (!Object.prototype.hasOwnProperty.call(state.filters, facetField)) return;
      var selected = state.filters[facetField].indexOf(facetValue) >= 0;
      state.filters[facetField] = facetField === 'brimSection'
        ? [facetValue] : (selected ? [] : [facetValue]);
      state.section = 'explore';
      state.view = 'landing';
      state.selectedId = '';
      state.quickIds = [];
      state.quickId = '';
      state.quickLabel = '';
      state.history = [];
      render();
      var facetReplacement = root.querySelector(
        '[data-guide-action="facet-toggle"][data-guide-filter="' + facetField + '"][data-guide-value="' + facetValue + '"]'
      );
      if (facetReplacement) facetReplacement.focus();
    } else if (action === 'filter-remove') {
      var chipField = target.getAttribute('data-guide-filter');
      if (!Object.prototype.hasOwnProperty.call(state.filters, chipField)) return;
      state.filters[chipField] = [];
      state.section = 'explore';
      state.view = 'landing';
      render();
      searchInput.focus();
    } else if (action === 'quick') {
      var quickIds = String(target.getAttribute('data-guide-products') || '')
        .split(',')
        .filter(Boolean);
      if (target.getAttribute('data-guide-entry-kind') !== 'collection') {
        openRecord(quickIds[0]);
      } else {
        pushHistory();
        state.section = 'explore';
        state.view = 'quick';
        state.query = '';
        state.quickIds = quickIds;
        state.quickId = target.getAttribute('data-guide-quick') || '';
        state.quickLabel = target.getAttribute('data-guide-entry-label') || '';
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
  }

  function resourceGatewaySearchOptions(section, view, query) {
    var exactQuery = String(query || '');
    if (section !== 'resources' || view !== 'landing' || !/\S/.test(exactQuery)) {
      return null;
    }
    return {
      preset: 'all_resources',
      query: exactQuery,
      focusSearch: true
    };
  }

  function handleSearchInput(event) {
    if (state.view === 'resource-explorer') {
      state.resourceExplorer = resourceExplorerModel.setQuery(
        state.resourceExplorer, event.target.value
      );
      render();
      return;
    }
    var gatewayOptions = resourceGatewaySearchOptions(
      state.section, state.view, event.target.value
    );
    if (gatewayOptions) {
      openResourceExplorer(gatewayOptions, event.target);
      return;
    }
    if (state.section === 'resources' && state.view === 'landing') return;
    state.section = 'explore';
    state.view = 'landing';
    state.query = event.target.value;
    state.selectedId = '';
    state.quickIds = [];
    state.quickId = '';
    state.quickLabel = '';
    state.history = [];
    render();
  }

  function handleResourceChange(event) {
    var target = event.target;
    if (!target) return;
    if (target.hasAttribute('data-resource-provider')) {
      state.resourceExplorer = resourceExplorerModel.toggleProvider(
        state.resourceExplorer,
        target.getAttribute('data-resource-provider')
      );
      state.resourceExplorer.facetsOpen = true;
      state.resourceExplorer.pane = 'facets';
      render();
      focusResourceTarget(state.resourceExplorer.focusKey);
      return;
    }
    if (!target.hasAttribute('data-resource-field')) return;
    var field = target.getAttribute('data-resource-field');
    var patch = {};
    patch[field] = target.value;
    patch.pane = field === 'sortMode' ? state.resourceExplorer.pane : 'results';
    patch.facetsOpen = field === 'sortMode' ? state.resourceExplorer.facetsOpen : false;
    patch.selectedId = field === 'sortMode' ? state.resourceExplorer.selectedId : '';
    patch.renderLimit = field === 'sortMode' ? state.resourceExplorer.renderLimit : 25;
    state.resourceExplorer = resourceExplorerModel.patchState(
      state.resourceExplorer, patch
    );
    render();
    focusResourceTarget(field === 'sortMode' ? 'resource-search' : 'resource-facets-toggle');
  }

  function handleRootKeydown(event) {
    var browseRadioField = event.target && event.target.getAttribute
      ? event.target.getAttribute('data-guide-filter') : '';
    if (browseRadioField === 'brimSection' &&
        ['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', 'Home', 'End'].indexOf(event.key) >= 0) {
      event.preventDefault();
      var browseRadios = Array.prototype.slice.call(root.querySelectorAll(
        '[data-guide-action="facet-toggle"][data-guide-filter="brimSection"]'
      ));
      var browseRadioIndex = browseRadios.indexOf(event.target);
      if (browseRadioIndex < 0 || !browseRadios.length) return;
      var nextBrowseRadioIndex = browseRadioIndex;
      if (event.key === 'Home') nextBrowseRadioIndex = 0;
      else if (event.key === 'End') nextBrowseRadioIndex = browseRadios.length - 1;
      else if (event.key === 'ArrowRight' || event.key === 'ArrowDown') {
        nextBrowseRadioIndex = (browseRadioIndex + 1) % browseRadios.length;
      } else {
        nextBrowseRadioIndex = (browseRadioIndex - 1 + browseRadios.length) % browseRadios.length;
      }
      browseRadios[nextBrowseRadioIndex].click();
      return;
    }
    var presetTarget = event.target && event.target.getAttribute
      ? event.target.getAttribute('data-resource-preset') : '';
    if (presetTarget && ['ArrowLeft', 'ArrowRight', 'Home', 'End'].indexOf(event.key) >= 0) {
      event.preventDefault();
      var enabledPresets = resourceExplorerModel.presets.filter(function(preset) {
        return (resourceExplorerModel.facetCounts(state.resourceExplorer).presets[preset.id] || 0) > 0 ||
          preset.id === 'all_resources';
      });
      var currentIndex = enabledPresets.findIndex(function(preset) {
        return preset.id === presetTarget;
      });
      var nextIndex = currentIndex;
      if (event.key === 'Home') nextIndex = 0;
      else if (event.key === 'End') nextIndex = enabledPresets.length - 1;
      else if (event.key === 'ArrowRight') nextIndex = (currentIndex + 1) % enabledPresets.length;
      else nextIndex = (currentIndex - 1 + enabledPresets.length) % enabledPresets.length;
      state.resourceExplorer = resourceExplorerModel.setPreset(
        state.resourceExplorer, enabledPresets[nextIndex].id
      );
      render();
      focusResourceTarget(state.resourceExplorer.focusKey);
      return;
    }
    if (event.key === 'Escape') {
      event.preventDefault();
      if (state.view === 'resource-explorer') {
        var escapeResult = resourceExplorerModel.escape(state.resourceExplorer);
        if (escapeResult.action === 'nested') {
          state.resourceExplorer = escapeResult.state;
          render(false);
          focusResourceTarget(state.resourceExplorer.focusKey);
          return;
        }
      }
      closeGuide();
      return;
    }
    if (event.key !== 'Tab') return;
    var focusable = Array.prototype.slice.call(root.querySelectorAll(
      'button:not([disabled]), input:not([disabled]), select:not([disabled]), a[href], [tabindex]:not([tabindex="-1"])'
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
  }

  var lifecycle = resourceExplorerModel.createLifecycle();
  var guideApi = null;
  function teardownGuide() {
    if (!lifecycle.detach()) return;
    root.removeEventListener('click', handleRootClick);
    root.removeEventListener('change', handleResourceChange);
    root.removeEventListener('keydown', handleRootKeydown);
    searchInput.removeEventListener('input', handleSearchInput);
    if (root.parentNode) root.parentNode.removeChild(root);
    if (window.BRIM_GUIDE === guideApi) window.BRIM_GUIDE = undefined;
  }
  root.__brimGuideTeardown = teardownGuide;
  if (lifecycle.attach()) {
    root.addEventListener('click', handleRootClick);
    root.addEventListener('change', handleResourceChange);
    root.addEventListener('keydown', handleRootKeydown);
    searchInput.addEventListener('input', handleSearchInput);
  }

  guideApi = Object.freeze({
    open: openGuide,
    close: closeGuide,
    home: function() { resetHome(true); },
    openResources: function(options) { openResourceExplorer(options || {}, null); },
    search: function(query) {
      return search(query).map(function(record) {
        return { id: record.id, title: record.title, kind: record.kind };
      });
    },
    getState: function() {
      return Object.assign({}, snapshot(), {
        open: !root.hidden,
        resourceExplorerOpen: state.view === 'resource-explorer',
        listenerState: lifecycle.state()
      });
    },
    teardown: teardownGuide
  });
  window.BRIM_GUIDE = guideApi;
}
