# ------------------------------------------------------------------------------
# ESTILO
# Creamos un código con el estilo que le queremos añadir a nuestra aplicación
# Fondo, letras, títulos, etc
# ------------------------------------------------------------------------------

library(shiny)

appTheme <- function() {
  tagList(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap"
    ),
    
    tags$style(HTML("

/* BASE (fondo, letras, etc) */

body {
  background-color: #2A2F3A;
  color: #ffffff;
  font-family: 'Montserrat', sans-serif;
}

h1, h2, h3, h4 {
  color: #ffffff;
  font-weight: 700;
}

p {
  color: #e6e6e6;
  font-size: 16px;
}

.app-container {
  max-width: 1100px;
  margin: 0 auto;
  padding: 36px 20px 24px 20px;
  text-align: center;
}

/* TÍTULOS (añadimos los cuadros de títulos) */

.header-box {
  margin-bottom: 22px;
  padding: 20px 22px 24px 22px;
  background: rgba(255,255,255,0.06);
  border: 1px solid rgba(255,255,255,0.12);
  border-radius: 16px;
}

.header-box p {
  margin-top: 10px;
  color: #e6e6e6;
}

/* BOTONES (dos tipos, principal y de warning o secundario) */

.btn {
  font-weight: 700;
  border-radius: 10px !important;
  border: none !important;
}

.btn-primary {
  background-color: #4C8DFF !important;
  color: #ffffff !important;
}

.btn-warning {
  background-color: #F2C14E !important;
  color: #1b1b1b !important;
}

.btn-danger {
  background-color: #E85D5D !important;
  color: #ffffff !important;
}

/* DETALLES PARA LOS DROPDOWNS */

details.dropdown {
  background: rgba(255,255,255,0.06);
  border: 1px solid rgba(255,255,255,0.16);
  border-radius: 14px;
  margin: 12px 0;
}

details.dropdown summary {
  cursor: pointer;
  font-weight: 700;
  padding: 12px 16px;
  list-style: none;
}

details.dropdown summary::-webkit-details-marker {
  display: none;
}

details.dropdown summary::after {
  content: '\\25BC'; /* FLECHA PARA ABAJO */
  float: right;
  font-size: 12px;
  margin-top: 4px;
}

details.dropdown[open] summary::after {
  content: '\\25B2'; /* FLECHA PARA ARRIBA */
}

details.dropdown .dropdown-body {
  padding: 14px 16px 16px 16px;
  background: rgba(0,0,0,0.10);
  border-top: 1px solid rgba(255,255,255,0.12);
  border-radius: 0 0 14px 14px;
}

/* INPUTS */

.shiny-input-container {
  margin-bottom: 12px;
}

.shiny-input-container label {
  color: #ffffff;
  font-weight: 600;
  text-align: left;
  display: block;
}

.shiny-input-container input,
.shiny-input-container select,
.shiny-input-container .selectize-input {
  background-color: rgba(0,0,0,0.22) !important;
  color: #ffffff !important;
  border: 1px solid rgba(255,255,255,0.24) !important;
  border-radius: 10px !important;
}

.selectize-input > input {
  color: #ffffff !important;
}

.selectize-input input::placeholder {
  color: rgba(255,255,255,0.65) !important;
}

/* Dropdown para selección */
.selectize-dropdown {
  background-color: #1f2530 !important;
  color: #ffffff !important;
  border: 1px solid rgba(255,255,255,0.20) !important;
}

.selectize-dropdown .option {
  color: #ffffff !important;
}

.selectize-dropdown .active {
  background-color: rgba(76,141,255,0.25) !important;
}

/* INPUTS DE FILA */

.select-row {
  display: flex;
  justify-content: center;
  gap: 22px;
  margin: 10px 0;
  flex-wrap: wrap;
}

.select-row .shiny-input-container {
  max-width: 300px;
  margin: 0;
}

/* PERSONALIZACIÓN DE LA TABLA */

table {
  border-collapse: collapse;
  margin: 0 auto;
  background-color: #f7f8fb !important;
  color: #111827 !important;
}

th {
  background-color: #e9edf5 !important;
  color: #111827 !important;
  font-weight: 800 !important;
  text-align: center !important;
  border-top: 1px solid #d7deea !important;
}

td {
  background-color: #f7f8fb !important;
  color: #111827 !important;
  text-align: center !important;
  border-top: 1px solid #e3e8f3 !important;
}

tr:nth-child(even) td {
  background-color: #eef2f9 !important;
}

/* PASSADOR DE PAGINAS */

.pager {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 14px;
  margin: 14px 0 6px 0;
}

.pager .btn {
  min-width: 44px;
  font-weight: 800;
}

.pager-info {
  color: #ffffff;
  font-weight: 600;
}

/* MODAL */

.modal-content,
.modal-content * {
  color: #000000 !important;
}

/* FOOTER */

.footer {
  margin-top: 24px;
  padding-top: 14px;
  border-top: 1px solid rgba(255,255,255,0.12);
  color: #e6e6e6;
  text-align: center;
  font-size: 14px;
}

.footer a {
  color: #ffffff;
  font-weight: 700;
  text-decoration: underline;
}

    "))
  )
}


