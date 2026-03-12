GML_PARCELA_V4 : dialog {
  label = "Generador GML v4 - Parcela (EPSG:25830)";
  : column {
    : boxed_column {
      label = "Salida";
      : row {
        : edit_box {
          key = "outdir";
          label = "Carpeta";
          edit_width = 50;
        }
        : button {
          key = "browse";
          label = "...";
          width = 6;
        }
      }
    }

    : boxed_column {
      label = "Identificador";
      : row {
        : edit_box {
          key = "prefix";
          label = "Prefijo localId";
          edit_width = 20;
        }
        : text {
          label = "Ej: TEMP-YYYYMMDD-HHMMSS-NOMBRE";
        }
      }
    }

    spacer;

    : row {
      : button { key = "accept"; label = "Aceptar"; is_default = true; }
      : button { key = "cancel"; label = "Cancelar"; is_cancel = true; }
    }
  }
}
