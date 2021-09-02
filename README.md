## ArchIsoMyDrive

[![Release](https://img.shields.io/github/v/release/daltomi/ArchIsoMyDrive)](https://github.com/daltomi/ArchIsoMyDrive/releases/latest)

Utilidad GUI para copiar un archivo ISO a un disco USB.

Sólo para GNU/Linux.

<img src="https://github.com/daltomi/ArchIsoMyDrive/raw/master/screenshot/main.jpg"/>

____

#### Características:

1. Detecta automáticamente cuando se conecta o desconecta un dispositivo.

2. Verifica que la capacidad del dispositivo no sea menor que el tamaño del ISO.

3. Permite pausar/resumir la copia.

4. Muestra información detallada del dispositivo.

<img src="https://github.com/daltomi/ArchIsoMyDrive/raw/master/screenshot/info.jpg"/>

5. Permite listar y montar las particiones del archivo ISO.

<img src="https://github.com/daltomi/ArchIsoMyDrive/raw/master/screenshot/mount.jpg"/>

6. Permite generar varias sumas de verificación.

<img src="https://github.com/daltomi/ArchIsoMyDrive/raw/master/screenshot/checksums.jpg"/>

7. Permite clonar el dispositivo hacia un archivo.

<img src="https://github.com/daltomi/ArchIsoMyDrive/raw/master/screenshot/clone.jpg"/>


**Administrador**

Se necesitan permisos de administrador para usar el programa.

____

### Desarrollo

Se utiliza un _mix_ entre **C99** y **FreePascal** utilizando el IDE **Lazarus**.

**Interfaz Gráfica**

Según el paquete de Lazarus que se instale la de **GTK2** o **Qt5**. Vea: `pacman -Ss lazarus`

**Librerías, dependencias**

Se utiliza _librhash_, _libudev_ , _libmount_ , _libblkid_ y afines a los mismos, `fpc`, `lazarus`, `pgkconf`, `make`, `gcc`.

**Compilación**

Primero debe compilar el código fuente de C, en el directorio `c_in`.

Compilador: `GCC - C99`

```bash
make
```
Luego compilar las fuentes del proyecto con Lazarus v2.0+

```bash
lazbuild --bm=Release ArchIsoMyDrive.lpi
```

El nombre del programa ArchIsoMyDrive es: `archisomydrv`

##### Lenguajes

Por defecto la interfaz está en inglés.

Los catálogos de los lenguajes(*.po) se encuentran en  `locale` y deben ser
copiados manualmente hacia `/usr/share/ArchIsoMyDrive/locale`

### Colaboración

* Testers

Compilar el código fuente de C con: `make debug`

Compilar el proyecto de Lazarus con: `lazbuild --bm=Debug ArchIsoMyDrive.lpi`


* Traductores

En el directorio `locale`, copie el archivo `archisomydrv.po` con el nombre del lenguaje que desea.

Por ejemplo: `cp archisomydrv.po archisomydrv.es_MX.UTF-8.po`

Utilice algún editor de catálogos, por ejemplo `poedit`, para modificar el nuevo archivo.


