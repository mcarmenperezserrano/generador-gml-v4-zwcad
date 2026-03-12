# Generador GML v4 para ZWCAD

Este es un plugin para ZWCAD que genera archivos GML v4 para parcelas catastrales, creando un archivo por cada polilínea exterior seleccionada. También puede manejar polilíneas interiores opcionales y texto interno para nombrar los archivos de salida.
# 1) Descarga los archivos

Necesitas estos 2 archivos en tu PC:

    gmlv4.lsp
    GML_PARCELA_4.dcl

Recomendación: crea una carpeta, por ejemplo: C:\ZWCAD\gmlv4\ y mete los dos ahí.
# 2) Añade la carpeta a las rutas de soporte (para que encuentre el DCL)

En ZWCAD:
    1. Escribe OPTIONS en la línea de comandos.
    2. Pestaña Files.
    3. Despliega Support File Search Path.
    4. Pulsa Add… y añade la carpeta donde has copiado los archivos C:\ZWCAD\plugins\gmlv4\
    5. Apply / OK.
    
Esto es importante para que ZWCAD pueda cargar GML_PARCELA_4.dcl y te salga el diálogo.

# 3) Carga el LISP

Opción manual (la más fácil):

    Escribe APPLOAD
    Busca C:\ZWCAD\gmlv4\gmlv4.lsp
    Pulsa Load (Cargar)
    Cierra.
    Si quieres que cargue siempre al arrancar, en la misma ventana APPLOAD suele haber “Startup Suite” (Suite de inicio). Añádelo ahí.
    
# 4) Ejecuta el comando

En la línea de comandos escribe:

    gmlv4

Si todo está bien cargado, te pedirá seleccionar entidades.
# 5) Qué seleccionar (importante)

Selecciona solo polilíneas cerradas que sean el contorno exterior de cada parcela:

    Tipos permitidos: LWPOLYLINE / POLYLINE
    Deben estar cerradas
    No se admiten arcos/bulges: si hay curvas, el plugin da error (convierte a segmentos rectos antes).

# 6) Rellena el diálogo

Te saldrá el diálogo (si encuentra el DCL):

    Carpeta: donde guardar los .gml
    Prefijo localId: por defecto TEMP

Si no sale el diálogo, el plugin te pedirá una carpeta mediante un selector “tipo guardar .txt” (fallback). El resultado es el mismo.
# 7) Qué genera

Genera 1 GML por cada polilínea exterior seleccionada con:

    CRS fijo: EPSG:25830
    Nombre fichero: YYYYMMDD-LOCALID.gml (fecha sin guiones)
    localId: TEMP-YYYYMMDD-HHMMSS-<NOMBRE_LIMPIO>

El <NOMBRE_LIMPIO> sale de:

    un TEXT/MTEXT dentro de la parcela, si existe
    si no, usa P1, P2, etc.

# Detalles a tener en cuenta en ZWCAD

    Las entidades deben ser LWPOLYLINE/POLYLINE cerradas.
    Si la polilínea tiene arcos (bulge), el plugin aborta con error (convierte a segmentos rectos antes).
    Si no aparece el diálogo, normalmente es porque no encuentra el .dcl (revisar el paso 2).


Si me dices qué versión de ZWCAD usas (y si es ZWCAD estándar o ZWCAD Mechanical), te indico exactamente dónde está la opción de “Support File Search Path” en tu interfaz.

### Ejemplo de nombre de archivo
Para una fecha de 2026-03-12, localId TEMP-20260312-122924-1A.el archivo generado será: `2026-03-12-TEMP-20260312-122924-1A.gml`.
