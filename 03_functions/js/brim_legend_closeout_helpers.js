function(el, x) {
  // Shared BRIM legend closeout helper.
  //
  // Purpose:
  //   Keep the common "x hides only this legend" behavior in one place while
  //   each layer controller continues to own its own active-layer/filter logic.
  //
  // Contract:
  //   - close button click stops propagation
  //   - caller decides what local state to change in onClose()
  //   - helper hides the legend div by default after onClose()
  //   - wiring is idempotent for a given button node
  window.BRIM = window.BRIM || {};

  if (!window.BRIM.legendCloseout) {
    window.BRIM.legendCloseout = {};
  }

  window.BRIM.legendCloseout.wire = function(div, selector, onClose, options) {
    if (!div || !selector) return null;
    var btn = div.querySelector(selector);
    if (!btn || btn.__brimLegendCloseoutWired) return btn || null;

    options = options || {};
    btn.__brimLegendCloseoutWired = true;

    btn.addEventListener('click', function(e) {
      if (e && e.preventDefault) e.preventDefault();
      if (e && e.stopPropagation) e.stopPropagation();

      if (typeof onClose === 'function') {
        onClose(e, btn, div);
      }

      if (options.hide !== false) {
        div.style.display = 'none';
      }
    }, false);

    return btn;
  };

  window.BRIM.legendCloseout.buttonHtml = function(extraClass, label) {
    extraClass = extraClass || '';
    label = label || 'Hide legend';
    return '<button type="button" class="pt-map-legend-close ' + extraClass +
      '" aria-label="' + label + '" title="' + label + '">&times;</button>';
  };
}
