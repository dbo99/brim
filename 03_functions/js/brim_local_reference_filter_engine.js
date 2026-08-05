(function(root, factory) {
  var api = factory();
  if (typeof module === 'object' && module.exports) module.exports = api;
  if (root) {
    root.BRIM = root.BRIM || {};
    root.BRIM.localReferenceFilterEngine = api;
  }
}(typeof self !== 'undefined' ? self : this, function() {
  'use strict';

  function clean(value) {
    return String(value === undefined || value === null ? '' : value)
      .toLowerCase()
      .replace(/\s+/g, ' ')
      .trim();
  }

  function unique(values) {
    var seen = Object.create(null);
    return values.filter(function(value) {
      var key = String(value);
      if (seen[key]) return false;
      seen[key] = true;
      return true;
    });
  }

  function copySet(source) {
    var out = new Set();
    source.forEach(function(value) { out.add(value); });
    return out;
  }

  function setsDiffer(left, right) {
    if (left.size !== right.size) return true;
    var different = false;
    left.forEach(function(value) {
      if (!right.has(value)) different = true;
    });
    return different;
  }

  function normalizeBounds(value) {
    var bounds = Array.isArray(value) ? value.slice(0, 4) : [];
    if (bounds.length !== 4) return null;
    bounds = bounds.map(Number);
    if (bounds.some(function(item) { return !isFinite(item); })) return null;
    if (bounds[0] > bounds[2] || bounds[1] > bounds[3]) return null;
    return bounds;
  }

  function mergeBounds(values) {
    var valid = values.map(normalizeBounds).filter(Boolean);
    if (!valid.length) return null;
    return valid.reduce(function(out, bounds) {
      return [
        Math.min(out[0], bounds[0]),
        Math.min(out[1], bounds[1]),
        Math.max(out[2], bounds[2]),
        Math.max(out[3], bounds[3])
      ];
    });
  }

  function create(config) {
    config = config || {};
    var categories = Array.isArray(config.categories) ? config.categories.slice() : [];
    var records = Array.isArray(config.records) ? config.records.slice() : [];
    var configuredFeatures = Array.isArray(config.features) ? config.features.slice() : [];
    var categoryKeys = categories.map(function(category) {
      return String(category.category_key || '');
    });
    if (!categoryKeys.length || categoryKeys.some(function(key) { return !key; })) {
      throw new Error('Local Reference filter engine requires named categories.');
    }
    if (unique(categoryKeys).length !== categoryKeys.length) {
      throw new Error('Local Reference category keys must be unique.');
    }
    var validCategory = Object.create(null);
    categoryKeys.forEach(function(key) { validCategory[key] = true; });

    records.forEach(function(record, index) {
      if (!record || !validCategory[String(record.category_key || '')]) {
        throw new Error('Local Reference record ' + index + ' has an unknown category.');
      }
      if (!record.geometry_key || !record.semantic_feature_key) {
        throw new Error('Local Reference records require geometry and semantic feature keys.');
      }
      record.category_key = String(record.category_key);
      record.geometry_key = String(record.geometry_key);
      record.feature_key = String(record.feature_key || record.semantic_feature_key);
      record.semantic_feature_key = String(record.semantic_feature_key);
      record.feature_display_name = String(
        record.feature_display_name || record.semantic_feature_key
      );
      record.search_text = clean(record.search_text);
      record.semantic_feature_bounds = normalizeBounds(record.semantic_feature_bounds);
      record.geometry_component_count = Math.max(
        1,
        Number(record.geometry_component_count) || 1
      );
    });

    var featureSelectionSupported = config.feature_selection_supported === true;
    var featureSelectionMode = String(config.feature_selection_mode || 'none');
    if (featureSelectionSupported && featureSelectionMode !== 'semantic_feature_multi') {
      throw new Error('Enabled Local Reference feature selection requires semantic_feature_multi mode.');
    }

    var featureBySemantic = Object.create(null);
    var featureOrder = [];
    function addFeatureDefinition(feature) {
      var semanticKey = String(feature.semantic_feature_key || '');
      if (!semanticKey) throw new Error('Local Reference features require semantic keys.');
      if (featureBySemantic[semanticKey]) {
        throw new Error('Local Reference semantic feature keys must be unique.');
      }
      var featureCategories = unique((Array.isArray(feature.category_keys) ?
        feature.category_keys : []).map(String));
      if (!featureCategories.length) {
        featureCategories = unique(records.filter(function(record) {
          return record.semantic_feature_key === semanticKey;
        }).map(function(record) { return record.category_key; }));
      }
      if (featureCategories.some(function(key) { return !validCategory[key]; })) {
        throw new Error('Local Reference feature ' + semanticKey + ' has an unknown category.');
      }
      var bounds = normalizeBounds(feature.semantic_feature_bounds);
      if (featureSelectionSupported && !bounds) {
        throw new Error('Local Reference selectable feature ' + semanticKey + ' requires bounds.');
      }
      var displayName = String(feature.display_name || semanticKey);
      featureBySemantic[semanticKey] = {
        semantic_feature_key: semanticKey,
        feature_key: String(feature.feature_key || semanticKey),
        display_name: displayName,
        category_keys: featureCategories,
        search_text: clean([displayName, feature.search_text, semanticKey].join(' ')),
        semantic_feature_bounds: bounds,
        geometry_component_count: Math.max(
          1,
          Number(feature.geometry_component_count) || 1
        )
      };
      featureOrder.push(semanticKey);
    }

    if (configuredFeatures.length) {
      configuredFeatures.forEach(addFeatureDefinition);
    } else {
      unique(records.map(function(record) {
        return record.semantic_feature_key;
      })).forEach(function(semanticKey) {
        var rows = records.filter(function(record) {
          return record.semantic_feature_key === semanticKey;
        });
        addFeatureDefinition({
          semantic_feature_key: semanticKey,
          feature_key: rows[0].feature_key,
          display_name: rows[0].feature_display_name,
          category_keys: unique(rows.map(function(record) { return record.category_key; })),
          search_text: rows.map(function(record) { return record.search_text; }).join(' '),
          semantic_feature_bounds: mergeBounds(rows.map(function(record) {
            return record.semantic_feature_bounds;
          })),
          geometry_component_count: rows.reduce(function(total, record) {
            return total + record.geometry_component_count;
          }, 0)
        });
      });
    }
    records.forEach(function(record) {
      if (!featureBySemantic[record.semantic_feature_key]) {
        throw new Error('Local Reference record has no semantic feature definition.');
      }
    });

    var defaultSelected = new Set(categoryKeys);
    var draftSelected = copySet(defaultSelected);
    var appliedSelected = copySet(defaultSelected);
    var draftFeatureKeys = new Set();
    var appliedFeatureKeys = new Set();
    var autoSupported = config.auto_supported === true;
    var auto = autoSupported && config.auto_default === true;
    var autoZoomSupported = config.auto_zoom_supported === true;
    var autoZoom = autoZoomSupported && config.auto_zoom_default === true;

    function apply() {
      appliedSelected = copySet(draftSelected);
      appliedFeatureKeys = copySet(draftFeatureKeys);
      return snapshot();
    }

    function maybeApply() {
      return auto ? apply() : snapshot();
    }

    function setCategory(categoryKey, selected) {
      categoryKey = String(categoryKey || '');
      if (!validCategory[categoryKey]) {
        throw new Error('Unknown Local Reference category: ' + categoryKey);
      }
      if (selected) draftSelected.add(categoryKey);
      else draftSelected.delete(categoryKey);
      return maybeApply();
    }

    function setAuto(value) {
      if (!autoSupported) {
        auto = false;
        return snapshot();
      }
      auto = value === true;
      if (auto) return apply();
      return snapshot();
    }

    function setAutoZoom(value) {
      autoZoom = autoZoomSupported && value === true;
      return snapshot();
    }

    function all() {
      draftSelected = copySet(defaultSelected);
      return maybeApply();
    }

    function none() {
      draftSelected = new Set();
      return maybeApply();
    }

    function addFeature(semanticKey) {
      semanticKey = String(semanticKey || '');
      if (!featureSelectionSupported) {
        throw new Error('Named-feature selection is unsupported for this Local Reference layer.');
      }
      var feature = featureBySemantic[semanticKey];
      if (!feature) throw new Error('Unknown Local Reference semantic feature: ' + semanticKey);
      if (draftFeatureKeys.has(semanticKey)) return snapshot();
      draftFeatureKeys.add(semanticKey);
      feature.category_keys.forEach(function(categoryKey) {
        draftSelected.add(categoryKey);
      });
      return maybeApply();
    }

    function removeFeature(semanticKey) {
      semanticKey = String(semanticKey || '');
      draftFeatureKeys.delete(semanticKey);
      return maybeApply();
    }

    function reset() {
      draftSelected = copySet(defaultSelected);
      appliedSelected = copySet(defaultSelected);
      draftFeatureKeys = new Set();
      appliedFeatureKeys = new Set();
      auto = autoSupported && config.auto_default === true;
      autoZoom = autoZoomSupported && config.auto_zoom_default === true;
      return snapshot();
    }

    function featureSearch(value, limit) {
      if (!featureSelectionSupported) return [];
      var query = clean(value);
      if (!query) return [];
      limit = Math.max(1, Number(limit) || 8);
      return featureOrder.map(function(semanticKey, order) {
        var feature = featureBySemantic[semanticKey];
        var display = clean(feature.display_name);
        var rank = display === query ? 0 :
          display.indexOf(query) === 0 ? 1 :
          display.indexOf(query) !== -1 ? 2 :
          feature.search_text.indexOf(query) !== -1 ? 3 : 99;
        return {feature: feature, rank: rank, order: order};
      }).filter(function(result) {
        return result.rank < 99 && !draftFeatureKeys.has(
          result.feature.semantic_feature_key
        );
      }).sort(function(left, right) {
        if (left.rank !== right.rank) return left.rank - right.rank;
        var nameOrder = left.feature.display_name.localeCompare(
          right.feature.display_name
        );
        return nameOrder || left.order - right.order;
      }).slice(0, limit).map(function(result) {
        return {
          semantic_feature_key: result.feature.semantic_feature_key,
          feature_key: result.feature.feature_key,
          display_name: result.feature.display_name,
          category_keys: result.feature.category_keys.slice(),
          semantic_feature_bounds:
            result.feature.semantic_feature_bounds.slice()
        };
      });
    }

    function recordMatches(record) {
      if (!appliedSelected.has(record.category_key)) return false;
      return !appliedFeatureKeys.size || appliedFeatureKeys.has(
        record.semantic_feature_key
      );
    }

    function countRows(rows) {
      var semantic = unique(rows.map(function(record) {
        return record.semantic_feature_key;
      })).length;
      var geometry = rows.reduce(function(total, record) {
        return total + record.geometry_component_count;
      }, 0);
      return {
        record_count: rows.length,
        semantic_feature_count: semantic,
        geometry_component_count: geometry
      };
    }

    function selectedFeatureRows(selectedKeys) {
      return featureOrder.filter(function(key) {
        return selectedKeys.has(key);
      }).map(function(key) {
        var feature = featureBySemantic[key];
        return {
          semantic_feature_key: feature.semantic_feature_key,
          feature_key: feature.feature_key,
          display_name: feature.display_name,
          category_keys: feature.category_keys.slice(),
          semantic_feature_bounds: feature.semantic_feature_bounds.slice()
        };
      });
    }

    function snapshot() {
      var showing = records.filter(recordMatches);
      var visibleSemanticKeys = unique(showing.map(function(record) {
        return record.semantic_feature_key;
      }));
      var visibleBounds = mergeBounds(visibleSemanticKeys.map(function(key) {
        return featureBySemantic[key].semantic_feature_bounds;
      }));
      var categoryCounts = {};
      categories.forEach(function(category) {
        var key = String(category.category_key);
        var totalRows = records.filter(function(record) {
          return record.category_key === key;
        });
        var showingRows = showing.filter(function(record) {
          return record.category_key === key;
        });
        categoryCounts[key] = {
          total: countRows(totalRows),
          currently_showing: countRows(showingRows)
        };
      });
      return {
        auto_supported: autoSupported,
        auto: auto,
        auto_zoom_supported: autoZoomSupported,
        auto_zoom: autoZoom,
        feature_selection_supported: featureSelectionSupported,
        feature_selection_mode: featureSelectionMode,
        draft_selected: categoryKeys.filter(function(key) {
          return draftSelected.has(key);
        }),
        applied_selected: categoryKeys.filter(function(key) {
          return appliedSelected.has(key);
        }),
        draft_feature_keys: featureOrder.filter(function(key) {
          return draftFeatureKeys.has(key);
        }),
        applied_feature_keys: featureOrder.filter(function(key) {
          return appliedFeatureKeys.has(key);
        }),
        draft_features: selectedFeatureRows(draftFeatureKeys),
        applied_features: selectedFeatureRows(appliedFeatureKeys),
        has_pending_changes:
          setsDiffer(draftSelected, appliedSelected) ||
          setsDiffer(draftFeatureKeys, appliedFeatureKeys),
        counts: {
          total: countRows(records),
          currently_showing: countRows(showing)
        },
        category_counts: categoryCounts,
        visible_geometry_keys: showing.map(function(record) {
          return record.geometry_key;
        }),
        visible_semantic_feature_keys: visibleSemanticKeys,
        visible_semantic_feature_bounds: visibleBounds
      };
    }

    return {
      setCategory: setCategory,
      setAuto: setAuto,
      setAutoZoom: setAutoZoom,
      addFeature: addFeature,
      removeFeature: removeFeature,
      featureSearch: featureSearch,
      all: all,
      none: none,
      apply: apply,
      reset: reset,
      snapshot: snapshot
    };
  }

  return {create: create};
}));
