;;; ============================================================================
;;; gmlv4.lsp - Generador GML v4 (INSPIRE CP 4.0) para parcelas desde ZWCAD
;;; Basado en el enfoque de chapulincatastral/generador-gml (polilíneas cerradas,
;;; detección de texto interior, islas interiores), adaptado a:
;;;  - Comando: gmlv4
;;;  - EPSG fijo: 25830
;;;  - 1 GML por parcela (por polilínea exterior seleccionada)
;;;  - localId temporal: TEMP-YYYYMMDD-HHMMSS-<NOMBRE_LIMPIO>
;;;  - Nombre fichero: YYYYMMDD-LOCALID.gml
;;;  - Limpieza de texto para que localId sea válido (A-Z0-9_)
;;; Licencia: GPLv3 (como el proyecto original).
;;; ============================================================================

(vl-load-com)

;; -----------------------------
;; Utilidades fecha/hora
;; -----------------------------
(defun gmlv4:pad2 (n) (if (< n 10) (strcat "0" (itoa n)) (itoa n)))

(defun gmlv4:now_yyyymmdd_hhmmss ( / d)
  ;; getvar "CDATE" => yyyymmdd.hhmmss (real)
  ;; Convertimos a string con 6 decimales y extraemos partes.
  (setq d (rtos (getvar "CDATE") 2 6)) ; p.ej. "20260312.123045"
  (list
    (substr d 1 8)             ; YYYYMMDD
    (substr d 10 6)            ; HHMMSS
  )
)

;; -----------------------------
;; Limpieza de texto para IDs
;; - MAYÚSCULAS
;; - Solo A-Z 0-9 _
;; - Sustituye cualquier otro char por _
;; - Colapsa __ a _
;; - Trim _ inicio/fin
;; - Máx 32
;; -----------------------------
(defun gmlv4:is-alnum (c / a)
  (setq a (ascii c))
  (or (and (>= a 48) (<= a 57))
      (and (>= a 65) (<= a 90)))
)

(defun gmlv4:clean_name (s / i out c lastUnd)
  (if (null s) (setq s ""))
  (setq s (strcase s))
  (setq i 1 out "" lastUnd nil)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((gmlv4:is-alnum c)
        (setq out (strcat out c))
        (setq lastUnd nil)
      )
      (T
        (if (not lastUnd)
          (progn
            (setq out (strcat out "_"))
            (setq lastUnd T)
          )
        )
      )
    )
    (setq i (1+ i))
  )
  ;; trim underscores
  (while (and (> (strlen out) 0) (= (substr out 1 1) "_"))
    (setq out (substr out 2))
  )
  (while (and (> (strlen out) 0) (= (substr out (strlen out) 1) "_"))
    (setq out (substr out 1 (1- (strlen out))))
  )
  (if (= out "") (setq out "SIN_NOMBRE"))
  (if (> (strlen out) 32) (setq out (substr out 1 32)))
  out
)

;; -----------------------------
;; Geometría: extracción de vértices
;; -----------------------------
(defun gmlv4:has_bulge (edata / bulges)
  (setq bulges (vl-remove-if-not '(lambda (x) (= 42 (car x))) edata))
  (while (and bulges (= 0.0 (cdr (car bulges))))
    (setq bulges (cdr bulges))
  )
  (if bulges T nil)
)

(defun gmlv4:poly_vertices (edata / pts)
  ;; puntos (10 . (x y [z])) en orden
  (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) edata)))
  pts
)

(defun gmlv4:poslist_from_pts (pts / out)
  ;; gml:posList con pares x y separados por espacios, y el primer punto repetido al final
  (defun pt2 (p)
    (strcat (rtos (car p) 2 2) " " (rtos (cadr p) 2 2))
  )
  (setq out "")
  (foreach p pts
    (setq out (strcat out (pt2 p) " "))
  )
  ;; Cerrar anillo repitiendo 1º
  (if pts
    (setq out (strcat out (pt2 (car pts))))
  )
  out
)

;; -----------------------------
;; Detección de TEXT/MTEXT interior e islas interiores
;; - Similar a la lógica del generador original: ssget "_WP" con el contorno
;; -----------------------------
(defun gmlv4:inner_objects (pts)
  (ssget "_WP" pts)
)

(defun gmlv4:first_text_inside (ss / n i e ed txt)
  (setq txt nil)
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (and (< i n) (null txt))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (cond
          ((= (cdr (assoc 0 ed)) "TEXT")
            (setq txt (cdr (assoc 1 ed)))
          )
          ((= (cdr (assoc 0 ed)) "MTEXT")
            (setq txt (cdr (assoc 1 ed)))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  txt
)

(defun gmlv4:inner_islands (ss / n i e ed islands)
  ;; devuelve lista de listas de pts (polilíneas cerradas interiores)
  (setq islands '())
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq e (ssname ss i))
        (setq ed (entget e))
        (if (and (member (cdr (assoc 0 ed)) '("LWPOLYLINE" "POLYLINE"))
                 (= (cdr (assoc 70 ed)) 1))
          (progn
            (if (gmlv4:has_bulge ed)
              (progn
                (alert "ERROR: Se ha encontrado una isla con arcos/bulges. Convierte a segmentos rectos.")
                (exit)
              )
            )
            (setq islands (cons (gmlv4:poly_vertices ed) islands))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  islands
)

;; -----------------------------
;; Plantilla GML (INSPIRE CP 4.0) - Parcela
;; Nota: mantenemos esquema cp/4.0 como en el repo original v4 beta.
;; EPSG fijo 25830.
;; -----------------------------
(defun gmlv4:gml_header ()
  (strcat
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    "<!-- Parcela Catastral (INSPIRE CP 4.0) -->\n"
    "<!-- Generado por gmlv4 (basado en chapulincatastral/generador-gml) -->\n"
    "<gml:FeatureCollection gml:id=\"ES.SDGC.CP\"\n"
    "  xmlns:gml=\"http://www.opengis.net/gml/3.2\"\n"
    "  xmlns:cp=\"http://inspire.ec.europa.eu/schemas/cp/4.0\"\n"
    "  xmlns:base=\"http://inspire.ec.europa.eu/schemas/base/3.3\"\n"
    "  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
    "  xsi:schemaLocation=\"http://inspire.ec.europa.eu/schemas/cp/4.0 http://inspire.ec.europa.eu/schemas/cp/4.0/CadastralParcels.xsd\">\n"
  )
)

(defun gmlv4:gml_footer ()
  "</gml:FeatureCollection>\n"
)

(defun gmlv4:gml_parcel_member (localId posExterior posInteriors / gml interiors)
  (setq interiors "")
  (foreach isl posInteriors
    (setq interiors
      (strcat interiors
        "                      <gml:interior>\n"
        "                        <gml:LinearRing>\n"
        "                          <gml:posList srsDimension=\"2\">"
        isl
        "</gml:posList>\n"
        "                        </gml:LinearRing>\n"
        "                      </gml:interior>\n"
      )
    )
  )

  (setq gml
    (strcat
      "  <gml:featureMember>\n"
      "    <cp:CadastralParcel gml:id=\"ES.LOCAL.CP." localId "\">\n"
      "      <cp:beginLifespanVersion xsi:nil=\"true\" nilReason=\"other:unpopulated\"></cp:beginLifespanVersion>\n"
      "      <cp:geometry>\n"
      "        <gml:MultiSurface gml:id=\"MS_" localId "\" srsName=\"urn:ogc:def:crs:EPSG::25830\">\n"
      "          <gml:surfaceMember>\n"
      "            <gml:Surface gml:id=\"S_" localId "\" srsName=\"urn:ogc:def:crs:EPSG::25830\">\n"
      "              <gml:patches>\n"
      "                <gml:PolygonPatch>\n"
      "                  <gml:exterior>\n"
      "                    <gml:LinearRing>\n"
      "                      <gml:posList srsDimension=\"2\">" posExterior "</gml:posList>\n"
      "                    </gml:LinearRing>\n"
      "                  </gml:exterior>\n"
      interiors
      "                </gml:PolygonPatch>\n"
      "              </gml:patches>\n"
      "            </gml:Surface>\n"
      "          </gml:surfaceMember>\n"
      "        </gml:MultiSurface>\n"
      "      </cp:geometry>\n"
      "      <cp:inspireId>\n"
      "        <base:Identifier>\n"
      "          <base:localId>" localId "</base:localId>\n"
      "          <base:namespace>ES.LOCAL.CP</base:namespace>\n"
      "        </base:Identifier>\n"
      "      </cp:inspireId>\n"
      "      <cp:label/>\n"
      "      <cp:nationalCadastralReference/>\n"
      "    </cp:CadastralParcel>\n"
      "  </gml:featureMember>\n"
    )
  )
  gml
)

;; -----------------------------
;; Escritura fichero
;; -----------------------------
(defun gmlv4:write_text_file (path content / f)
  (setq f (open path "w"))
  (if (null f)
    (progn (alert (strcat "ERROR: No se puede escribir el fichero:\n" path)) (exit))
    (progn (princ content f) (close f))
  )
)

(defun gmlv4:choose_output_folder ( / p)
  ;; Pedimos un archivo .txt "dummy" para capturar directorio (como hace el original).
  (setq p (getfiled "Elige carpeta de salida (selecciona/crea un fichero temporal .txt)" (getvar "DWGPREFIX") "txt" 1))
  (if p (vl-filename-directory p) nil)
)

;; -----------------------------
;; Dialogo DCL (si existe)
;; -----------------------------
(defun gmlv4:run_dialog ( / dcl_id ok outdir prefix)
  (setq outdir "")
  (setq prefix "TEMP")

  (if (not (findfile "GML_PARCELA_4.dcl"))
    (list nil nil) ; sin DCL
    (progn
      (setq dcl_id (load_dialog "GML_PARCELA_4.dcl"))
      (if (not dcl_id) (list nil nil)
        (progn
          (if (not (new_dialog "GML_PARCELA_V4" dcl_id))
            (progn (unload_dialog dcl_id) (list nil nil))
            (progn
              (set_tile "prefix" prefix)
              (set_tile "outdir" outdir)

              (action_tile "browse" "(setq outdir (gmlv4:choose_output_folder))(if outdir (set_tile \"outdir\" outdir))")
              (action_tile "cancel" "(done_dialog 0)")
              (action_tile "accept" "(setq prefix (get_tile \"prefix\"))(setq outdir (get_tile \"outdir\"))(done_dialog 1)")

              (setq ok (start_dialog))
              (unload_dialog dcl_id)

              (if (= ok 1)
                (list outdir prefix)
                (list nil nil)
              )
            )
          )
        )
      )
    )
  )
)

;; -----------------------------
;; Comando principal
;; -----------------------------
(defun c:gmlv4 ( / ss n i ent ed pts insideSS nameRaw nameClean islands now ymd hms prefix outdir localId filePath gml)
  (setq ss (ssget '((0 . "POLYLINE,LWPOLYLINE") (70 . 1))))
  (if (null ss)
    (progn (prompt "\nNo se han seleccionado polilíneas cerradas.") (princ))
    (progn
      (setq n (sslength ss))

      ;; Dialogo: carpeta salida + prefijo
      (setq dlg (gmlv4:run_dialog))
      (setq outdir (car dlg))
      (setq prefix (cadr dlg))
      (if (or (null outdir) (= outdir ""))
        (progn
          ;; fallback sin DCL o cancelado: pedir carpeta por getfiled
          (setq outdir (gmlv4:choose_output_folder))
          (if (null outdir)
            (progn (alert "Cancelado.") (exit))
          )
          (setq prefix "TEMP")
        )
      )

      ;; Fecha/hora
      (setq now (gmlv4:now_yyyymmdd_hhmmss))
      (setq ymd (car now))
      (setq hms (cadr now))

      (setq i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq ed (entget ent))

        ;; Rechazar arcos
        (if (gmlv4:has_bulge ed)
          (progn
            (alert "ERROR: La polilínea seleccionada contiene arcos/bulges. Convierte a segmentos rectos.")
            (exit)
          )
        )

        (setq pts (gmlv4:poly_vertices ed))
        (setq insideSS (gmlv4:inner_objects pts))

        ;; Nombre interior (texto) o fallback 1A/1B...
        (setq nameRaw (gmlv4:first_text_inside insideSS))
        (if (or (null nameRaw) (= nameRaw ""))
          (setq nameRaw (strcat "P" (itoa (1+ i))))
        )
        (setq nameClean (gmlv4:clean_name nameRaw))

        ;; Islas interiores
        (setq islandsPts (gmlv4:inner_islands insideSS))

        ;; Convertir islas a posList
        (setq islandsPos '())
        (foreach islPts islandsPts
          (setq islandsPos (cons (gmlv4:poslist_from_pts islPts) islandsPos))
        )

        ;; localId temporal (incluye nombre limpio)
        (setq localId (strcat prefix "-" ymd "-" hms "-" nameClean))

        ;; Nombre de fichero: YYYYMMDD-LOCALID.gml
        (setq filePath (strcat outdir "\\" ymd "-" localId ".gml"))

        ;; GML final
        (setq gml
          (strcat
            (gmlv4:gml_header)
            (gmlv4:gml_parcel_member localId (gmlv4:poslist_from_pts pts) islandsPos)
            (gmlv4:gml_footer)
          )
        )

        (gmlv4:write_text_file filePath gml)

        (setq i (1+ i))
      )

      (alert (strcat "Generación finalizada.\nParcelas exportadas: " (itoa n) "\nCarpeta: " outdir))
      (princ)
    )
  )
)
