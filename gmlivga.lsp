;;; ============================================================================
;;; gmlivga.lsp - Generador GML INSPIRE CP 3.0 compatible IVGA (Catastro)
;;; Comando: gmlivga
;;; - 1 GML = 1 parcela (1 polilínea cerrada exterior)
;;; - EPSG fijo: 25830
;;; - Admite islas: polilíneas cerradas interiores => gml:interior
;;; - localId: TEMP-YYYYMMDD-HHMMSS-<NOMBRE_LIMPIO>
;;; - fichero: YYYYMMDD-LOCALID.gml
;;; - Limpieza NOMBRE_LIMPIO: A-Z0-9_, mayúsculas, max 32
;;; ============================================================================

(vl-load-com)

;; -----------------------------
;; Fecha/hora (YYYYMMDD, HHMMSS)
;; -----------------------------
(defun ivga:now_yyyymmdd_hhmmss ( / d)
  (setq d (rtos (getvar "CDATE") 2 6)) ; "20260316.133430"
  (list (substr d 1 8) (substr d 10 6))
)

;; -----------------------------
;; Limpieza de texto para IDs
;; -----------------------------
(defun ivga:is-alnum (c / a)
  (setq a (ascii c))
  (or (and (>= a 48) (<= a 57))
      (and (>= a 65) (<= a 90)))
)

(defun ivga:clean_name (s / i out c lastUnd)
  (if (null s) (setq s ""))
  (setq s (strcase s))
  (setq i 1 out "" lastUnd nil)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((ivga:is-alnum c)
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
;; Geometría: bulges / vértices
;; -----------------------------
(defun ivga:has_bulge (edata / bulges)
  (setq bulges (vl-remove-if-not '(lambda (x) (= 42 (car x))) edata))
  (while (and bulges (= 0.0 (cdr (car bulges))))
    (setq bulges (cdr bulges))
  )
  (if bulges T nil)
)

(defun ivga:poly_vertices (edata / pts)
  (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) edata)))
  pts
)

(defun ivga:poslist_from_pts (pts / out)
  (defun pt2 (p) (strcat (rtos (car p) 2 2) " " (rtos (cadr p) 2 2)))
  (setq out "")
  (foreach p pts (setq out (strcat out (pt2 p) " ")))
  (if pts (setq out (strcat out (pt2 (car pts)))))
  out
)

;; -----------------------------
;; Objetos interiores (texto e islas)
;; -----------------------------
(defun ivga:inner_objects (pts)
  (ssget "_WP" pts)
)

(defun ivga:first_text_inside (ss / n i e ed txt)
  (setq txt nil)
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (and (< i n) (null txt))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (cond
          ((= (cdr (assoc 0 ed)) "TEXT")  (setq txt (cdr (assoc 1 ed))))
          ((= (cdr (assoc 0 ed)) "MTEXT") (setq txt (cdr (assoc 1 ed))))
        )
        (setq i (1+ i))
      )
    )
  )
  txt
)

(defun ivga:inner_islands (ss / n i e ed islands)
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
            (if (ivga:has_bulge ed)
              (progn
                (alert "ERROR: Isla con arcos/bulges. Convierte a segmentos rectos.")
                (exit)
              )
            )
            (setq islands (cons (ivga:poly_vertices ed) islands))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  islands
)

;; -----------------------------
;; Carpeta salida (simple)
;; -----------------------------
(defun ivga:choose_output_folder ( / p)
  (setq p (getfiled "Elige carpeta de salida (selecciona/crea un fichero temporal .txt)" (getvar "DWGPREFIX") "txt" 1))
  (if p (vl-filename-directory p) nil)
)

;; -----------------------------
;; GML CP 3.0 (IVGA)
;; -----------------------------
(defun ivga:gml_header ()
  ;; Catastro IVGA suele validar CP 3.0.
  ;; schemaLocation puede variar; si el validador pide otro, lo ajustamos.
  (strcat
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<gml:FeatureCollection gml:id=\"ES.SDGC.CP\"\n"
    "  xmlns:gml=\"http://www.opengis.net/gml/3.2\"\n"
    "  xmlns:cp=\"http://inspire.ec.europa.eu/schemas/cp/3.0\"\n"
    "  xmlns:base=\"http://inspire.ec.europa.eu/schemas/base/3.3\"\n"
    "  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
    "  xsi:schemaLocation=\"http://inspire.ec.europa.eu/schemas/cp/3.0 http://inspire.ec.europa.eu/schemas/cp/3.0/CadastralParcels.xsd\">\n"
  )
)

(defun ivga:gml_footer ()
  "</gml:FeatureCollection>\n"
)

(defun ivga:gml_polygon (posExterior posInteriors / interiors)
  (setq interiors "")
  (foreach isl posInteriors
    (setq interiors
      (strcat interiors
        "                <gml:interior>\n"
        "                  <gml:LinearRing>\n"
        "                    <gml:posList srsDimension=\"2\">" isl "</gml:posList>\n"
        "                  </gml:LinearRing>\n"
        "                </gml:interior>\n"
      )
    )
  )

  (strcat
    "          <gml:surfaceMember>\n"
    "            <gml:Polygon gml:id=\"poly1\" srsName=\"urn:ogc:def:crs:EPSG::25830\">\n"
    "              <gml:exterior>\n"
    "                <gml:LinearRing>\n"
    "                  <gml:posList srsDimension=\"2\">" posExterior "</gml:posList>\n"
    "                </gml:LinearRing>\n"
    "              </gml:exterior>\n"
    interiors
    "            </gml:Polygon>\n"
    "          </gml:surfaceMember>\n"
  )
)

(defun ivga:gml_parcel_member (localId posExterior posInteriors / geom)
  (setq geom
    (strcat
      "      <cp:geometry>\n"
      "        <gml:MultiSurface gml:id=\"MS_" localId "\" srsName=\"urn:ogc:def:crs:EPSG::25830\">\n"
      (ivga:gml_polygon posExterior posInteriors)
      "        </gml:MultiSurface>\n"
      "      </cp:geometry>\n"
    )
  )

  (strcat
    "  <gml:featureMember>\n"
    "    <cp:CadastralParcel gml:id=\"ES.LOCAL.CP." localId "\">\n"
    "      <cp:inspireId>\n"
    "        <base:Identifier>\n"
    "          <base:localId>" localId "</base:localId>\n"
    "          <base:namespace>ES.LOCAL.CP</base:namespace>\n"
    "        </base:Identifier>\n"
    "      </cp:inspireId>\n"
    geom
    "      <cp:label/>\n"
    "      <cp:nationalCadastralReference/>\n"
    "    </cp:CadastralParcel>\n"
    "  </gml:featureMember>\n"
  )
)

(defun ivga:write_text_file (path content / f)
  (setq f (open path "w"))
  (if (null f)
    (progn (alert (strcat "ERROR: No se puede escribir:\n" path)) (exit))
    (progn (princ content f) (close f))
  )
)

;; -----------------------------
;; Comando principal: 1 parcela
;; -----------------------------
(defun c:gmlivga ( / ss ent ed pts insideSS nameRaw nameClean islandsPts islandsPos now ymd hms localId outdir filePath gml)
  ;; Selección: el usuario elige 1 polilínea cerrada exterior
  (setq ss (ssget "_:S" '((0 . "POLYLINE,LWPOLYLINE") (70 . 1))))
  (if (null ss)
    (progn (prompt "\nNo se ha seleccionado ninguna polilínea cerrada.") (princ))
    (progn
      (setq ent (ssname ss 0))
      (setq ed (entget ent))

      ;; arcos/bulges -> no permitido
      (if (ivga:has_bulge ed)
        (progn (alert "ERROR: La polilínea exterior contiene arcos/bulges. Convierte a segmentos rectos.") (exit))
      )

      (setq pts (ivga:poly_vertices ed))
      (setq insideSS (ivga:inner_objects pts))

      ;; nombre: texto interior o P1
      (setq nameRaw (ivga:first_text_inside insideSS))
      (if (or (null nameRaw) (= nameRaw "")) (setq nameRaw "P1"))
      (setq nameClean (ivga:clean_name nameRaw))

      ;; islas interiores
      (setq islandsPts (ivga:inner_islands insideSS))
      (setq islandsPos '())
      (foreach islPts islandsPts
        (setq islandsPos (cons (ivga:poslist_from_pts islPts) islandsPos))
      )

      ;; salida
      (setq outdir (ivga:choose_output_folder))
      (if (null outdir) (progn (alert "Cancelado.") (exit)))

      (setq now (ivga:now_yyyymmdd_hhmmss))
      (setq ymd (car now))
      (setq hms (cadr now))

      (setq localId (strcat "TEMP-" ymd "-" hms "-" nameClean))
      (setq filePath (strcat outdir "\\" ymd "-" localId ".gml"))

      (setq gml
        (strcat
          (ivga:gml_header)
          (ivga:gml_parcel_member localId (ivga:poslist_from_pts pts) islandsPos)
          (ivga:gml_footer)
        )
      )

      (ivga:write_text_file filePath gml)
      (alert (strcat "OK (IVGA).\nArchivo:\n" filePath))
      (princ)
    )
  )
)
