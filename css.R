library(bslib)

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B7285",

  "nav-link-color" = "bs-primary-text-emphasis",

  base_font = font_google("Inter"),
  code_font  = font_google("Fira Mono"),
  heading_font = font_google("Inter")
)

# Left panel input blocks spacing
theme <- bslib::bs_add_rules(theme, "
  .input-block { margin-bottom: .75rem; display: flow-root; }
  .input-block > h5 { margin: 0 0 .25rem 0; font-size: 1.25rem; }
  
  /* Ensure no special-casing for last block margins */
  .input-block:last-child { margin-bottom: .75rem !important; }
  "
)

# Two-column header layout
theme <- bslib::bs_add_rules(theme, "
  .cols-tight {
    --tight-gap: 0.75rem;
    gap: var(--tight-gap) !important;
    padding-left: var(--tight-gap);
    padding-right: var(--tight-gap);
  }
  "
)

# Sidebar height
theme <- bslib::bs_add_rules(theme, "
  .sidebar-card { min-height: 100vh; }
  "
)

# Match card-header style to accordion-button
theme <- bslib::bs_add_rules(theme, "
  .card-header {
    background-color: var(--bs-primary-bg-subtle);
    color: var(--bs-primary-text-emphasis);
  }
  "
)

# Title and subtitle
theme <- bslib::bs_add_rules(theme, "
  .navbar .navbar-brand {
    margin: 0 !important;
  }

  .navbar .navbar-brand .name-title {
  font-size: 2rem;
  }
  .navbar .navbar-brand .name-subtitle {
  font-size: 1rem; font-weight: 400; color: var(--bs-gray-500);
  }
  "
)

# Sticky footer
theme <- bslib::bs_add_rules(theme, "
  body { padding-bottom: 32px; }
  .app-footer {
    position: fixed; left: 0; right: 0; bottom: 0;
    background: var(--bs-body-bg);
    padding-top: 0.5rem !important;
    padding-bottom: 0.5rem !important;
    border-top: 1px solid var(--bs-border-color);
    text-align: center;
    font-size: 0.85rem;
    color: #6c757d;
  }
  "
)

# Remove default extra borders on active accordion-button
theme <- bslib::bs_add_rules(theme, "
  .accordion-button {
    border: none !important;
    box-shadow: none !important;
  }
  .accordion-button:not(.collapsed) {
    border: none !important;
    box-shadow: none !important;
  }
  .accordion-button:focus {
    border: none !important;
    box-shadow: none !important;
    outline: none !important;
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

# Results header row layout and right-side box divider
theme <- bslib::bs_add_rules(theme, "
  .results-header-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  .results-right-box {
    border-left: 1px solid var(--bs-border-color);
    padding-left: 15px;
  }
  "
)

# Gap spacing
theme <- bslib::bs_add_rules(theme, "
  .bslib-gap-spacing {
    gap: none;
  }
  "
)

# Help hint and info icon styles for tooltips
theme <- bslib::bs_add_rules(theme, "
  .help-hint { cursor: help; }
  .info-icon { cursor: help; font-weight: 600; }
  "
)