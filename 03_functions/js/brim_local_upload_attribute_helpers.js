function ptInitBrimLocalUploadAttributeHelpers(el, x) {
  // Pure value classification helpers for browser-session Local GIS uploads.
  // Source feature properties are read only and are never rewritten here.
  window.BRIM = window.BRIM || {};

  var NO_DATA_COLOR = '#BDBDBD';
  var CATEGORY_WARN_COUNT = 30;
  var sequentialPalettes = {
    Viridis: ['#440154', '#482878', '#3E4989', '#31688E', '#26828E', '#1F9E89', '#35B779', '#6DCD59', '#B4DE2C', '#FDE725'],
    Blues: ['#F7FBFF', '#E3EEF9', '#CFE1F2', '#B5D4E9', '#93C4DE', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B'],
    'Yellow–orange–red': ['#FFFFCC', '#FFF2A9', '#FEE187', '#FEC965', '#FEAB49', '#FD8D3C', '#FC5A2D', '#E93420', '#C81D13', '#800026'],
    Greens: ['#F7FCF5', '#E5F5E0', '#C7E9C0', '#A1D99B', '#74C476', '#41AB5D', '#238B45', '#087F3C', '#006D2C', '#00441B'],
    Purples: ['#FCFBFD', '#EFEDF5', '#DADAEB', '#BCBDDC', '#9E9AC8', '#807DBA', '#6A51A3', '#54278F', '#4A1486', '#3F007D']
  };
  var qualitativeColors = [
    '#4E79A7', '#F28E2B', '#E15759', '#76B7B2', '#59A14F',
    '#EDC948', '#B07AA1', '#FF9DA7', '#9C755F', '#BAB0AC',
    '#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD',
    '#8C564B', '#E377C2', '#7F7F7F', '#BCBD22', '#17BECF'
  ];

  function missing(value) {
    if (value === null || value === undefined) return true;
    if (typeof value === 'number') return !Number.isFinite(value);
    if (typeof value !== 'string') return false;

    var text = value.trim();
    if (!text) return true;
    return /^(?:null|na|nan|[+-]?inf(?:inity)?)$/i.test(text);
  }

  function displayValue(value) {
    if (missing(value)) return '—';
    if (typeof value === 'string') return value.trim();
    if (typeof value === 'object') {
      try {
        return JSON.stringify(value);
      } catch (err) {
        return String(value);
      }
    }
    return String(value);
  }

  function strictNumber(value) {
    if (typeof value === 'number') {
      return Number.isFinite(value) ? {ok: true, value: value, convertedText: false} : {ok: false};
    }
    if (typeof value !== 'string' || missing(value)) return {ok: false};

    var text = value.trim();
    if (!/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$/.test(text)) return {ok: false};

    var unsigned = text.replace(/^[+-]/, '');
    var mantissa = unsigned.split(/[eE]/)[0];
    var integerPart = mantissa.split('.')[0];
    if (integerPart.length > 1 && integerPart.charAt(0) === '0') return {ok: false};

    var number = Number(text);
    if (!Number.isFinite(number)) return {ok: false};
    return {ok: true, value: number, convertedText: true};
  }

  function propertyValue(feature, field) {
    var props = feature && feature.properties ? feature.properties : {};
    return Object.prototype.hasOwnProperty.call(props, field) ? props[field] : undefined;
  }

  function uploadId(sequence) {
    return 'pt_local_' + String(Math.max(1, Math.floor(Number(sequence) || 1)));
  }

  function detectNumericField(features, field) {
    features = Array.isArray(features) ? features : [];
    var meaningfulCount = 0;
    var convertedText = false;

    for (var i = 0; i < features.length; i++) {
      var raw = propertyValue(features[i], field);
      if (missing(raw)) continue;
      var parsed = strictNumber(raw);
      if (!parsed.ok) {
        return {field: field, numeric: false, meaningfulCount: meaningfulCount, convertedText: false};
      }
      meaningfulCount += 1;
      convertedText = convertedText || parsed.convertedText;
    }

    return {
      field: field,
      numeric: meaningfulCount > 0,
      meaningfulCount: meaningfulCount,
      convertedText: meaningfulCount > 0 && convertedText
    };
  }

  function detectNumericFields(features, fields) {
    fields = Array.isArray(fields) ? fields : [];
    return fields.map(function(field) {
      return detectNumericField(features, field);
    }).filter(function(info) {
      return info.numeric;
    });
  }

  function paletteColors(name, count, reverse) {
    var source = sequentialPalettes[name] || sequentialPalettes.Viridis;
    count = Math.max(1, Math.min(10, Number(count) || 1));
    var colors = [];

    if (count === 1) {
      colors.push(source[Math.floor(source.length / 2)]);
    } else {
      for (var i = 0; i < count; i++) {
        colors.push(source[Math.round(i * (source.length - 1) / (count - 1))]);
      }
    }

    return reverse ? colors.reverse() : colors;
  }

  function formatNumber(value) {
    if (!Number.isFinite(value)) return '—';
    if (value === 0) return '0';

    var abs = Math.abs(value);
    if (abs >= 1000000000 || abs < 0.0001) {
      return value.toExponential(3).replace(/\.0+(?=e)/, '').replace(/(\.\d*?)0+(?=e)/, '$1');
    }

    return new Intl.NumberFormat('en-US', {
      maximumSignificantDigits: 8,
      useGrouping: true
    }).format(value);
  }

  function rangeLabel(lower, upper) {
    if (lower === upper) return formatNumber(lower);
    var lowerText = formatNumber(lower);
    var upperText = formatNumber(upper);
    if (lowerText === upperText) {
      lowerText = Number(lower).toPrecision(10).replace(/\.?0+$/, '');
      upperText = Number(upper).toPrecision(10).replace(/\.?0+$/, '');
    }
    return lowerText + ' – ' + upperText;
  }

  function numericClassification(features, field, options) {
    features = Array.isArray(features) ? features : [];
    options = options || {};
    var requestedCount = Math.max(3, Math.min(10, Number(options.classCount) || 7));
    var method = options.method === 'equal' ? 'equal' : 'quantile';
    var finiteValues = [];

    features.forEach(function(feature) {
      var parsed = strictNumber(propertyValue(feature, field));
      if (parsed.ok) finiteValues.push(parsed.value);
    });

    finiteValues.sort(function(a, b) { return a - b; });
    if (!finiteValues.length) {
      return {
        field: field,
        method: method,
        requestedCount: requestedCount,
        effectiveCount: 0,
        bins: [],
        noDataCount: features.length,
        noDataColor: NO_DATA_COLOR,
        classIndex: function() { return -1; }
      };
    }

    var distinct = finiteValues.filter(function(value, index, values) {
      return index === 0 || value !== values[index - 1];
    });
    var targetCount = Math.max(1, Math.min(requestedCount, distinct.length));
    var groups = [];
    var classifier;

    if (targetCount === 1) {
      groups = [{rawIndex: 0, values: finiteValues.slice()}];
      classifier = function(value) {
        return strictNumber(value).ok ? 0 : -1;
      };
    } else if (method === 'equal') {
      var minimum = finiteValues[0];
      var maximum = finiteValues[finiteValues.length - 1];
      var width = (maximum - minimum) / targetCount;
      var rawGroups = [];
      var rawToCompact = {};
      for (var equalIndex = 0; equalIndex < targetCount; equalIndex++) rawGroups.push([]);

      finiteValues.forEach(function(value) {
        var rawIndex = value === maximum ? targetCount - 1 : Math.floor((value - minimum) / width);
        rawIndex = Math.max(0, Math.min(targetCount - 1, rawIndex));
        rawGroups[rawIndex].push(value);
      });

      rawGroups.forEach(function(values, rawIndex) {
        if (!values.length) return;
        rawToCompact[rawIndex] = groups.length;
        groups.push({rawIndex: rawIndex, values: values});
      });

      classifier = function(value) {
        var parsed = strictNumber(value);
        if (!parsed.ok) return -1;
        var rawIndex = parsed.value === maximum ? targetCount - 1 : Math.floor((parsed.value - minimum) / width);
        rawIndex = Math.max(0, Math.min(targetCount - 1, rawIndex));
        return Object.prototype.hasOwnProperty.call(rawToCompact, rawIndex) ? rawToCompact[rawIndex] : -1;
      };
    } else {
      var thresholds = [];
      var maximumValue = finiteValues[finiteValues.length - 1];

      for (var quantileIndex = 1; quantileIndex < targetCount; quantileIndex++) {
        var position = Math.max(0, Math.ceil(quantileIndex * finiteValues.length / targetCount) - 1);
        var candidate = finiteValues[position];
        if (candidate >= maximumValue) continue;
        if (!thresholds.length || candidate > thresholds[thresholds.length - 1]) thresholds.push(candidate);
      }

      groups = thresholds.map(function(_threshold, index) {
        return {rawIndex: index, values: []};
      });
      groups.push({rawIndex: thresholds.length, values: []});

      function quantileClass(value) {
        for (var thresholdIndex = 0; thresholdIndex < thresholds.length; thresholdIndex++) {
          if (value <= thresholds[thresholdIndex]) return thresholdIndex;
        }
        return thresholds.length;
      }

      finiteValues.forEach(function(value) {
        groups[quantileClass(value)].values.push(value);
      });
      groups = groups.filter(function(group) { return group.values.length > 0; });
      classifier = function(value) {
        var parsed = strictNumber(value);
        return parsed.ok ? quantileClass(parsed.value) : -1;
      };
    }

    var colors = paletteColors(options.palette, groups.length, !!options.reverse);
    var bins = groups.map(function(group, index) {
      var lower = group.values[0];
      var upper = group.values[group.values.length - 1];
      return {
        index: index,
        lower: lower,
        upper: upper,
        count: group.values.length,
        color: colors[index],
        label: rangeLabel(lower, upper)
      };
    });

    return {
      field: field,
      method: method,
      requestedCount: requestedCount,
      effectiveCount: bins.length,
      bins: bins,
      noDataCount: Math.max(0, features.length - finiteValues.length),
      noDataColor: NO_DATA_COLOR,
      classIndex: classifier
    };
  }

  function categoryColor(index) {
    if (index < qualitativeColors.length) return qualitativeColors[index];
    var hue = Math.round((index * 137.508) % 360);
    var saturation = 58 + ((index % 3) * 7);
    var lightness = 40 + ((Math.floor(index / 3) % 3) * 8);
    return 'hsl(' + hue + ',' + saturation + '%,' + lightness + '%)';
  }

  function categoryClassification(features, field) {
    features = Array.isArray(features) ? features : [];
    var counts = Object.create(null);
    var labels = Object.create(null);
    var noDataCount = 0;

    features.forEach(function(feature) {
      var raw = propertyValue(feature, field);
      if (missing(raw)) {
        noDataCount += 1;
        return;
      }
      var label = displayValue(raw);
      var key = label;
      counts[key] = (counts[key] || 0) + 1;
      labels[key] = label;
    });

    var keys = Object.keys(counts).sort(function(a, b) {
      var aNumber = strictNumber(a);
      var bNumber = strictNumber(b);
      if (aNumber.ok && bNumber.ok && aNumber.value !== bNumber.value) return aNumber.value - bNumber.value;
      var aLower = a.toLowerCase();
      var bLower = b.toLowerCase();
      if (aLower < bLower) return -1;
      if (aLower > bLower) return 1;
      return a < b ? -1 : (a > b ? 1 : 0);
    });
    var keyToIndex = Object.create(null);
    var categories = keys.map(function(key, index) {
      keyToIndex[key] = index;
      return {
        index: index,
        key: key,
        label: labels[key],
        count: counts[key],
        color: categoryColor(index)
      };
    });

    return {
      field: field,
      categories: categories,
      noDataCount: noDataCount,
      noDataColor: NO_DATA_COLOR,
      highCardinality: categories.length > CATEGORY_WARN_COUNT,
      warningCount: CATEGORY_WARN_COUNT,
      classIndex: function(value) {
        if (missing(value)) return -1;
        var key = displayValue(value);
        return Object.prototype.hasOwnProperty.call(keyToIndex, key) ? keyToIndex[key] : -1;
      }
    };
  }

  window.BRIM.localUploadAttribute = {
    noDataColor: NO_DATA_COLOR,
    categoryWarningCount: CATEGORY_WARN_COUNT,
    paletteNames: Object.keys(sequentialPalettes),
    missing: missing,
    displayValue: displayValue,
    strictNumber: strictNumber,
    uploadId: uploadId,
    detectNumericField: detectNumericField,
    detectNumericFields: detectNumericFields,
    paletteColors: paletteColors,
    formatNumber: formatNumber,
    numericClassification: numericClassification,
    categoryClassification: categoryClassification
  };
}
