;;; ============================================================================
;;; gmlivga.lsp - Generador GML para validador IVGA (Catastro)
;;;
;;; Basado en el patrón del GML descargado de Catastro (WFS 2.0 wrapper + CP 4.0):
;;; - Root: <FeatureCollection xmlns="http://www.opengis.net/wfs/2.0"> ... </FeatureCollection>
;;; - Contenido: <member><cp:CadastralParcel>...</cp:CadastralParcel></member>
;;; - schemaLocation: WFS 2.0 + CP 4.0
;;; - srsName: http://www.opengis.net/def/crs/EPSG/0/25830
;;; - Geometría: MultiSurface -> Surface -> PolygonPatch -> exterior/interior -> posList
;;;
;;; Reglas de identificación (según FAQ Catastro coordinación):
;;; - Si el usuario conoce la Referencia Catastral (RC): localId = RC, namespace = ES.SDGC.CP
;;; - Si NO existe en Catastro: namespace = ES.LOCAL.CP y localId unívoco (TEMP-YYYYMMDD-HHMMSS-NOMBRE)
;;;
;;; Además:
;;; - 1 GML = 1 parcela (selección de 1 polilínea cerrada exterior)
;;; - EPSG fijo 25830
;;; - Soporte de islas: polilíneas cerradas interiores => gml:interior
;;; - cp:nationalCadastralReference:
;;;     - con RC: RC
;;;     - sin RC: localId temporal (opción A)
;;; - cp:label:
;;;     - texto interior si existe
;;;     - si no, "P1"
;;; - cp:areaValue (m2): calculado desde la polilínea
;;; - cp:referencePoint: centro del bounding box (simple y válido)
;;;
;;; Comando: gmlivga
;;; ============================================================================

(vl-load-com)

;; -----------------------------
;; Fecha/hora
;; -----------------------------
(defun ivga:now_yyyymmdd_hhmmss ( / d)
  (setq d (rtos (getvar "CDATE") 2 6)) ; "20260316.133430"
  (list (substr d 1 8) (substr d 10 6))
)

(defun ivga:timestamp_iso ( / now ymd hms)
  (setq now (ivga:now_yyyymmdd_hhmmss))
  (setq ymd (car now))
  (setq hms (cadr now))
  (strcat (substr ymd 1 4) "-" (substr ymd 5 2) "-" (substr ymd 7 2)
          "T" (substr hms 1 2) ":" (substr hms 3 2) ":" (substr hms 5 2))
)

;; -----------------------------
;; String helpers
;; -----------------------------
(defun ivga:trim (s)
  (vl-string-trim " \t\r\n" (if s s ""))
)

(defun ivga:remove_spaces (s / out i c)
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (/= c " ") (setq out (strcat out c)))
    (setq i (1+ i))
  )
  out
)

;; -----------------------------
;; Limpieza nombre para IDs temporales
;; -----------------------------
(defun ivga:is-alnum (c / a)
  (setq a (ascii c))
  (or (and (>= a 48) (<= a 57))
      (and (>= a 65) (<= a 90)))
)

(defun ivga:is-alnum-str (s / i ok c)
  (setq i 1 ok T)
  (while (and ok (<= i (strlen s)))
    (setq c (substr s i 1))
    (if (not (ivga:is-alnum c)) (setq ok nil))
    (setq i (1+ i))
  )
  ok
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
;; Preguntar RC (opcional)
;; -----------------------------
(defun ivga:ask_refcat ( / rc)
  (setq rc (getstring T "\nReferencia Catastral (20) [Enter=sin RC]: "))
  (setq rc (strcase (ivga:remove_spaces (ivga:trim rc))))
  (if (= rc "")
    nil
    (progn
      (if (and (= (strlen rc) 20) (ivga:is-alnum-str rc))
        rc
        (progn
          (alert "La RC debe tener 20 caracteres (A-Z/0-9). Vuelve a intentarlo o pulsa Enter.")
          (ivga:ask_refcat)
        )
      )
    )
  )
)

;; -----------------------------
;; Geometría: bulges / vertices / posList / bbox / area
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

(defun ivga:bbox_center (pts / xmin xmax ymin ymax p x y)
  (if (null pts) (list 0.0 0.0)
    (progn
      (setq p (car pts))
      (setq xmin (car p) xmax (car p) ymin (cadr p) ymax (cadr p))
      (foreach p (cdr pts)
        (setq x (car p) y (cadr p))
        (if (< x xmin) (setq xmin x))
        (if (> x xmax) (setq xmax x))
        (if (< y ymin) (setq ymin y))
        (if (> y ymax) (setq ymax y))
      )
      (list (/ (+ xmin xmax) 2.0) (/ (+ ymin ymax) 2.0))
    )
  )
)

(defun ivga:poly_area_int (ent / obj a)
  (setq obj (vlax-ename->vla-object ent))
  (setq a (vla-get-area obj))
  (fix (+ a 0.5)) ; redondeo a entero
)

;; -----------------------------
;; Interior objects: texto e islas
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
  ;; Polilíneas cerradas interiores -> islas.
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
;; Output folder / file write
;; -----------------------------
(defun ivga:choose_output_folder ( / p)
  (setq p (getfiled "Elige carpeta de salida (selecciona/crea un fichero temporal .txt)" (getvar "DWGPREFIX") "txt" 1))
  (if p (vl-filename-directory p) nil)
)

(defun ivga:write_text_file (path content / f)
  (setq f (open path "w"))
  (if (null f)
    (progn (alert (strcat "ERROR: No se puede escribir:\n" path)) (exit))
    (progn (princ content f) (close f))
  )
)

;; -----------------------------
;; XML escape
;; -----------------------------
(defun ivga:xml_escape (s)
  (if (null s) (setq s ""))
  (setq s (vl-string-subst "&amp;" "&" s))
  (setq s (vl-string-subst "&lt;" "<" s))
  (setq s (vl-string-subst "&gt;" ">" s))
  (setq s (vl-string-subst "&quot;" "\"" s))
  s
)

;; -----------------------------
;; GML IVGA (WFS + CP 4.0)
;; -----------------------------
(defun ivga:gml_header (ts)
  (strcat
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    "<!--Parcela Catastral de la D.G. del Catastro.-->\n"
    "<!--La precisión es la que corresponde nominalmente a la escala de captura de la cartografía-->\n"
    "<FeatureCollection"
    " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
    " xmlns:gml=\"http://www.opengis.net/gml/3.2\""
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
    " xmlns:cp=\"http://inspire.ec.europa.eu/schemas/cp/4.0\""
    " xmlns:gmd=\"http://www.isotc211.org/2005/gmd\""
    " xsi:schemaLocation=\"http://www.opengis.net/wfs/2.0 http://schemas.opengis.net/wfs/2.0/wfs.xsd"
    " http://inspire.ec.europa.eu/schemas/cp/4.0 http://inspire.ec.europa.eu/schemas/cp/4.0/CadastralParcels.xsd\""
    " xmlns=\"http://www.opengis.net/wfs/2.0\""
    " timeStamp=\"" ts "\" numberMatched=\"1\" numberReturned=\"1\">\n"
  )
)

(defun ivga:gml_footer ()
  "</FeatureCollection>\n"
)

(defun ivga:gml_multisurface (id posExterior posInteriors / interiors)
  ;; id se usa para formar gml:id de MultiSurface y Surface (estilo Catastro)
  (setq interiors "")
  (foreach isl posInteriors
    (setq interiors
      (strcat interiors
        "  <gml:interior>\n"
        "      <gml:LinearRing>\n"
        "          <gml:posList srsDimension=\"2\">" isl "</gml:posList>\n"
        "      </gml:LinearRing>\n"
        "  </gml:interior>\n"
      )
    )
  )

  (strcat
    "<gml:MultiSurface gml:id=\"MultiSurface_" id "\" srsName=\"http://www.opengis.net/def/crs/EPSG/0/25830\">\n"
    "  <gml:surfaceMember>\n"
    "  <gml:Surface gml:id=\"Surface_" id ".1\" srsName=\"http://www.opengis.net/def/crs/EPSG/0/25830\">\n"
    "  <gml:patches>\n"
    "  <gml:PolygonPatch>\n"
    "  <gml:exterior>\n"
    "      <gml:LinearRing>\n"
    "          <gml:posList srsDimension=\"2\">" posExterior "</gml:posList>\n"
    "      </gml:LinearRing>\n"
    "  </gml:exterior>\n"
    interiors
    "  </gml:PolygonPatch>\n"
    "  </gml:patches>\n"
    "  </gml:Surface>\n"
    "  </gml:surfaceMember>\n"
    "</gml:MultiSurface>\n"
  )
)

(defun ivga:gml_member (namespace localId label area refPt posExterior posInteriors / escapedLabel parcelGmlId multiId ncr)
  (setq escapedLabel (ivga:xml_escape label))

  ;; gml:id del feature (Catastro: ES.SDGC.CP.<localId>)
  (setq parcelGmlId (strcat namespace "." localId))

  ;; ids geom
  (setq multiId (strcat namespace "." localId))

  ;; national cadastral reference: RC si existe, si no localId temporal (opción A)
  (setq ncr localId)

  (strcat
    "<member>\n"
    "<cp:CadastralParcel gml:id=\"" parcelGmlId "\">\n"
    "<cp:areaValue uom=\"m2\">" (itoa area) "</cp:areaValue>\n"
    "<cp:beginLifespanVersion xsi:nil=\"true\" nilReason=\"http://inspire.ec.europa.eu/codelist/VoidReasonValue/Unpopulated\"></cp:beginLifespanVersion>\n"
    "<cp:endLifespanVersion xsi:nil=\"true\" nilReason=\"http://inspire.ec.europa.eu/codelist/VoidReasonValue/Unpopulated\"></cp:endLifespanVersion>\n"
    "<cp:geometry>\n"
    (ivga:gml_multisurface (strcat parcelGmlId) posExterior posInteriors)
    "</cp:geometry>\n"
    "<cp:inspireId>\n"
    "<Identifier xmlns=\"http://inspire.ec.europa.eu/schemas/base/3.3\">\n"
    "  <localId>" localId "</localId>\n"
    "  <namespace>" namespace "</namespace>\n"
    "</Identifier>\n"
    "</cp:inspireId>\n"
    "<cp:label>" escapedLabel "</cp:label>\n"
    "<cp:nationalCadastralReference>" ncr "</cp:nationalCadastralReference>\n"
    "<cp:referencePoint>\n"
    "<gml:Point gml:id=\"ReferencePoint_" parcelGmlId "\" srsName=\"http://www.opengis.net/def/crs/EPSG/0/25830\">\n"
    "  <gml:pos>" (rtos (car refPt) 2 2) " " (rtos (cadr refPt) 2 2) "</gml:pos>\n"
    "</gml:Point>\n"
    "</cp:referencePoint>\n"
    "</cp:CadastralParcel>\n"
    "</member>\n"
  )
)

;; -----------------------------
;; Command: gmlivga (1 parcela)
;; -----------------------------
(defun c:gmlivga ( / ss ent ed pts insideSS nameRaw label nameClean islandsPts islandsPos now ymd hms outdir filePath gml area refPt ts rc namespace localId)

  (prompt "\nGML IVGA (Catastro): selecciona 1 polilínea cerrada (parcela).")

  ;; Selección única
  (setq ss (ssget "_:S" '((0 . "POLYLINE,LWPOLYLINE") (70 . 1))))
  (if (null ss)
    (progn (prompt "\nNo se ha seleccionado ninguna polilínea cerrada.") (princ))
    (progn
      (setq ent (ssname ss 0))
      (setq ed (entget ent))

      (if (ivga:has_bulge ed)
        (progn (alert "ERROR: La polilínea exterior contiene arcos/bulges. Convierte a segmentos rectos.") (exit))
      )

      (setq pts (ivga:poly_vertices ed))
      (setq insideSS (ivga:inner_objects pts))

      ;; label: texto interior si hay, si no P1
      (setq nameRaw (ivga:first_text_inside insideSS))
      (if (or (null nameRaw) (= nameRaw "")) (setq nameRaw "P1"))
      (setq label nameRaw)

      ;; islas
      (setq islandsPts (ivga:inner_islands insideSS))
      (setq islandsPos '())
      (foreach islPts islandsPts
        (setq islandsPos (cons (ivga:poslist_from_pts islPts) islandsPos))
      )

      ;; área y reference point
      (setq area (ivga:poly_area_int ent))
      (setq refPt (ivga:bbox_center pts))

      ;; carpeta salida
      (setq outdir (ivga:choose_output_folder))
      (if (null outdir) (progn (alert "Cancelado.") (exit)))

      ;; timestamp WFS y fecha para nombre fichero
      (setq ts (ivga:timestamp_iso))
      (setq now (ivga:now_yyyymmdd_hhmmss))
      (setq ymd (car now))
      (setq hms (cadr now))

      ;; Preguntar RC opcional
      (setq rc (ivga:ask_refcat))

      (if rc
        (progn
          ;; Con RC conocida (parcela existente / o se desea conservar RC)
          (setq namespace "ES.SDGC.CP")
          (setq localId rc)
        )
        (progn
          ;; Sin RC: parcela no existente en Catastro => ES.LOCAL.CP + id unívoco
          (setq namespace "ES.LOCAL.CP")
          (setq nameClean (ivga:clean_name nameRaw))
          (setq localId (strcat "TEMP-" ymd "-" hms "-" nameClean))
        )
      )

      ;; Nombre fichero: YYYYMMDD-LOCALID.gml
      (setq filePath (strcat outdir "\\" ymd "-" localId ".gml"))

      (setq gml
        (strcat
          (ivga:gml_header ts)
          (ivga:gml_member namespace localId label area refPt (ivga:poslist_from_pts pts) islandsPos)
          (ivga:gml_footer)
        )
      )

      (ivga:write_text_file filePath gml)
      (alert (strcat "OK (IVGA).\nArchivo:\n" filePath))
      (princ)
    )
  )
)
