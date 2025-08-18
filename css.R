# Theme Configuration ----------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B7285",
  base_font = font_google("Inter"),
  code_font  = font_google("Fira Mono"),
  heading_font = font_google("Inter")
)

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

# Help hint and info icon styles for tooltips
theme <- bslib::bs_add_rules(theme, "
  .help-hint { cursor: help; }
  .info-icon { cursor: help; font-weight: 600; }
  "
)