      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0342                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0342                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.11.2018 / 13:38:31 / T13AE34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01396!22.11.18!2.1 ! NEU                       ! HEIDER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1660!18.07.22! NEUES ATTRIBUT PGEB-GES      ! HEIDER   *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *************************************************************** *
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0342
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0342.
           05  PI-DRAN0342-KEY.
180722       10 PI-DRAN0342-GEIG-PGEB-GES       PIC S9(4) COMP.
             10 PI-DRAN0342-GEIG-VORSCHLAG-NR   PIC  X(0009).
      *
       01  PO-DRAN0342.
           05  PO-DRAN0342-RC                   PIC  X(003).
               88 PO-DRAN0342-RC-OK                  VALUE 'OK '.
               88 PO-DRAN0342-RC-NOT-FOUND           VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************

