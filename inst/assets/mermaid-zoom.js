// Zoomable Mermaid SVG JS
function enableZoomPan(svg) {
  let scale = 1, panX = 0, panY = 0, isPanning = false, startX, startY;
  svg.parentElement.classList.add('zoom-container');
  svg.style.transformOrigin = "0 0";

  svg.addEventListener('wheel', function(e) {
    e.preventDefault();
    const delta = e.deltaY > 0 ? 0.9 : 1.1;
    scale *= delta;
    svg.style.transform = `translate(${panX}px,${panY}px) scale(${scale})`;
  });

  svg.addEventListener('mousedown', function(e) {
    isPanning = true;
    startX = e.clientX - panX;
    startY = e.clientY - panY;
    svg.style.cursor = "grabbing";
  });
  window.addEventListener('mousemove', function(e) {
    if (!isPanning) return;
    panX = e.clientX - startX;
    panY = e.clientY - startY;
    svg.style.transform = `translate(${panX}px,${panY}px) scale(${scale})`;
  });
  window.addEventListener('mouseup', function() {
    isPanning = false;
    svg.style.cursor = "grab";
  });
}

function observeMermaidSVG() {
  function tryEnableZoom() {
    const mermaidContainers = document.querySelectorAll('svg.mermaid-js');
    mermaidContainers.forEach(container => {
      const hasGraphics = container.querySelector('g, path, text');
      if (hasGraphics) {
        enableZoomPan(container);
        return true;
      }
    });
    return false;
  }

  let attempts = 0;
  const maxAttempts = 30;
  function poll() {
    if (tryEnableZoom() || attempts >= maxAttempts) return;
    attempts++;
    setTimeout(poll, 200);
  }
  poll();
}

document.addEventListener('DOMContentLoaded', observeMermaidSVG);
