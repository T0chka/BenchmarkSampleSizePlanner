# Theme Configuration ----------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B7285",
  base_font = font_google("Inter"),
  code_font  = font_google("Fira Mono"),
  heading_font = font_google("Inter")
)

theme <- bslib::bs_add_rules(theme, "
  :root {
    --bs-link-color: #0B7285;
    --bs-link-hover-color: #085c6b;
  }
")

theme <- bslib::bs_add_rules(theme, "
  a { color: var(--bs-link-color) !important; }
  a:hover { color: var(--bs-link-hover-color) !important; }
")



# Navigation -------------------------------------------------------------------

# Navbar brand styling (Title and subtitle)
theme <- bslib::bs_add_rules(theme, "
  .navbar .navbar-brand { margin: 0 !important; }
  .navbar .navbar-brand .name-title   { font-size: 2rem; }
  .navbar .navbar-brand .name-subtitle{
    font-size: 1rem; font-weight: 400; color: var(--bs-gray-500);
  }
")

# Nav link colors
theme <- bslib::bs_add_rules(theme, "
  .nav {
    --bs-nav-link-color: var(--bs-primary-text-emphasis) !important;
    --bs-nav-link-hover-color: var(--bs-primary);
  }
")

# Match card-header style to accordion-button
theme <- bslib::bs_add_rules(theme, "
  .card-header {
    background-color: var(--bs-primary-bg-subtle);
    color: var(--bs-primary-text-emphasis);
  }
  "
)

# Layout & Spacing -------------------------------------------------------------

# Height of the page
theme <- bslib::bs_add_rules(theme, "
  .html-fill-container {
    min-height: auto !important;
  }
")

# Bottom spacing for content
theme <- bslib::bs_add_rules(theme, "
  /* Apply bottom spacing only to the main content container (after navbar) */
  .bslib-page-navbar > .navbar + .container-fluid {
    padding-bottom: 2.5rem;
  }
")

# Left panel input blocks spacing
theme <- bslib::bs_add_rules(theme, "
  .input-block { margin-bottom: .75rem; display: flow-root; }
  .input-block > h5 { margin: 0 0 .25rem 0; font-size: 1.25rem; }
  
  /* Ensure no special-casing for last block margins */
  .input-block:last-child { margin-bottom: .75rem !important; }
  
  /* Remove row gap (between settings) */
  .bslib-card .card-body{
    row-gap: 0;
  }
  "
)

# Column layout
theme <- bslib::bs_add_rules(theme, "
  /* Column gap */
  .layout-columns {
    --tight-gap: 1rem;
    gap: var(--tight-gap) !important;
  }
")

# Sidebar card header padding
theme <- bslib::bs_add_rules(theme, "
  .sidebar-card .card-header {
    padding-top: 1rem !important;
    padding-bottom: 0.5rem !important;
  }
  "
)

# Footer -----------------------------------------------------------------------

theme <- bslib::bs_add_rules(theme, "
  body { padding-bottom: 48px; }
  .app-footer {
    position: fixed; 
    left: 0; 
    right: 0; 
    bottom: 0;
    background: var(--bs-body-bg);
    padding-top: 0.5rem !important;
    padding-bottom: 0.5rem !important;
    border-top: 1px solid var(--bs-border-color);
    text-align: center;
    font-size: 0.70rem;
    color: #6c757d;
    z-index: 1030;
    display: flex;
    align-items: center;
    justify-content: center;
  }
")

# Accordion --------------------------------------------------------------------

# Accordion: remove extra borders/shadows
theme <- bslib::bs_add_rules(theme, "
  .accordion-button,
  .accordion-button:not(.collapsed),
  .accordion-button:focus {
    border: none !important;
    box-shadow: none !important;
    outline: none !important;
  }
")

# Results rection --------------------------------------------------------------

theme <- bslib::bs_add_rules(theme, "
:root{
    --sample-size-label-font-size: .8rem;
    --sample-size-value-font-size: 2rem;
    --badge-font-size: .85rem;
  }
  .metric{
    border:1px solid var(--bs-border-color);
    border-radius:var(--bs-border-radius);
    padding:.25rem .6rem;
    background:var(--bs-body-bg);
    font-size:var(--badge-font-size);
  }

  .parameters-stack{
    display: flex;
    flex-direction: row;
    gap: .5rem;
  }
  .parameters-stack > .metric{
    align-self: flex-start;
    width: max-content;
  }
  .result-box .card-body{
    display:flex;
    align-items:center;
    gap:.75rem;
  }
  
  .bslib-value-box .value-box-value{
    font-size: 1rem !important;
    text-align: left !important;
    overflow-x: clip;
    margin-bottom: 0 !important;
    column-gap: 0;
  }
  .bslib-value-box .value-box-area{
    padding: 1.5rem 1.5rem 1.5rem 1rem !important;
    flex-direction: row !important;
    align-items: center;
  }
  
  .sample-size-label{
    font-size:var(--sample-size-label-font-size);
    color:var(--bs-secondary-color);
    text-transform:uppercase;
    letter-spacing:.02em;
    margin-bottom:.1rem;
  }
  .sample-size-value{
    font-size:var(--sample-size-value-font-size);
    font-weight:500;
  }
")

# value-box
theme <- bslib::bs_add_rules(theme, "
  .value-box {
    border: 0 !important;
    border-radius: 14px;
    background: rgba(11,114,133,.06);           /* Primary color with low opacity */
    box-shadow: inset 0 0 0 1px rgba(11,114,133,.12);
  }
  .value-box .value-box-title {
    color: var(--bs-primary);
    font-weight: 600;
  }
  .value-box .value-box-value {
    font-size: 2.8rem;
    font-weight: 800;
  }
  .value-box .value-box-showcase {
    background: transparent;
    align-self: center;
  }
  .value-box .value-box-showcase .bi {
    font-size: 1.9rem;
    color: var(--bs-primary);
    background: #fff;
    border-radius: 50%;
    padding: 8px;
    box-shadow: 0 2px 6px rgba(0,0,0,.08);
  }
  "
)

# Tooltips ---------------------------------------------------------------------

theme <- bslib::bs_add_rules(theme, "
.hint {
  cursor: help;
  color: var(--bs-primary);
  font-weight: 500;
  position: relative;
  --tt-x: 0px;
  --tt-y: 0px;
  --tt-maxw: 320px;
  --tt-gap: 10px;
  --tt-pad: 8px 12px;
  --tt-radius: 10px;
  --tt-shadow: 0 12px 28px rgba(0,0,0,.12);
}

.hint::after {
  content: attr(data-hint);
  position: fixed;
  left: var(--tt-x);
  top: var(--tt-y);
  transform: translate(12px, calc(-100% - var(--tt-gap)));
  background: var(--bs-tertiary-bg);
  color: var(--bs-body-color);
  border: 1px solid var(--bs-border-color);
  border-radius: var(--tt-radius);
  padding: var(--tt-pad);
  max-width: var(--tt-maxw);
  box-shadow: var(--tt-shadow);
  white-space: normal;
  overflow-wrap: anywhere;
  pointer-events: none;
  z-index: 1002;
  opacity: 0;
  visibility: hidden;
  transition: opacity .12s ease, visibility .12s ease;
  font-size: 12px;
  line-height: 1.45;
}

.hint:hover::after { opacity: 1; visibility: visible; }

.hint::before {
  content: '';
  position: fixed;
  left: var(--tt-x);
  top: var(--tt-y);
  transform: translate(12px, -6px);
  width: 10px;
  height: 10px;
  background: var(--bs-tertiary-bg);
  border-left: 1px solid var(--bs-border-color);
  border-top: 1px solid var(--bs-border-color);
  box-shadow: -2px -2px 6px rgba(0,0,0,.04);
  z-index: 1001;
  opacity: 0;
  visibility: hidden;
  transition: opacity .12s ease, visibility .12s ease;
  clip-path: polygon(0 0, 100% 0, 0 100%);
}

.hint:hover::before { opacity: 1; visibility: visible; }

.hint[data-pos='bottom']::after {
  transform: translate(12px, var(--tt-gap));
}

.hint[data-pos='bottom']::before {
  transform: translate(12px, 6px) rotate(180deg);
}
")

theme <- bslib::bs_add_rules(theme, "
.hint-icon {
  font-size: 1rem;
  color: var(--bs-secondary-color);
  margin-left: 3px;
  cursor: help;
  opacity: 0.7;
  vertical-align: super;
  line-height: 1;
}
.hint-icon:hover { opacity: 1; }
")

# JS ---------------------------------------------------------------------------

tooltip_js <- "
document.addEventListener('mousemove', function(e){
  const t = e.target.closest('.hint');
  if(!t) return;
  const maxW = 320, margin = 16;
  let x = e.clientX, y = e.clientY;
  x = Math.min(x, window.innerWidth - maxW - margin);
  t.style.setProperty('--tt-x', x + 'px');
  t.style.setProperty('--tt-y', y + 'px');
  if (y < 90) t.setAttribute('data-pos','bottom');
  else t.removeAttribute('data-pos');
});
"