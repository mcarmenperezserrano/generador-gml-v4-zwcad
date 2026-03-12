# Generador GML v4 para ZWCAD

Este es un plugin para ZWCAD que genera archivos GML v4 para parcelas catastrales, creando un archivo por cada polilínea exterior seleccionada. También puede manejar polilíneas interiores opcionales y texto interno para nombrar los archivos de salida.

## Instalación
1. Copie el archivo `generador-gml-v4-zwcad.lsp` en la carpeta de soporte de ZWCAD.
2. Asegúrese de que ZWCAD pueda encontrar el plugin cargándolo en la línea de comando.

## Uso
1. Abra ZWCAD y cargue el plugin usando el comando `GMLV4`.
2. Seleccione las polilíneas exteriores que representan las parcelas.
3. Elija una carpeta de salida y establezca un prefijo para `localId`.
4. El archivo GML se generará y se guardará en la carpeta seleccionada con el formato `FECHA-LOCALID.gml`. 

### Ejemplo de nombre de archivo
Para una fecha de 2026-03-12, localId TEMP-20260312-122924-1A.el archivo generado será: `2026-03-12-TEMP-20260312-122924-1A.gml`.