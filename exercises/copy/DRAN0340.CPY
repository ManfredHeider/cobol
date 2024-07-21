      *****************************************************************
      *    COPY-MEMBER: DRAN0340                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0340                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1660!18.07.22! NEUES ATTRIBUT PGEB-GES      ! HEIDER   *
      *            !        !                              !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0340
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0340.
           05  PI-DRAN0340-KEY.

180722       10 PI-DRAN0340-GEIG-PGEB-GES       PIC S9(4) COMP.
             10 PI-DRAN0340-GEIG-VORSCHLAG-NR   PIC  X(0009).

           05  PI-DRAN0340-DATEN.

             10 PI-DRAN0340-GEIG-KENNUNG-UP     PIC  X(0008).
             10 PI-DRAN0340-GEIG-PIN            PIC S9(9) COMP.
             10 PI-DRAN0340-GEIG-ANREDE         PIC  X(0005).
             10 PI-DRAN0340-GEIG-EMAIL          PIC  X(0050).
             10 PI-DRAN0340-GEIG-GEBURT-DATUM   PIC  X(010).
             10 PI-DRAN0340-GEIG-IBAN           PIC  X(0034).
             10 PI-DRAN0340-GEIG-NAME-K-INH     PIC  X(0026).
             10 PI-DRAN0340-GEIG-OKI-ZEITPUNKT  PIC  X(0026).
             10 PI-DRAN0340-GEIG-STRASSE        PIC  X(0045).
             10 PI-DRAN0340-GEIG-TELEFON        PIC  X(0030).
             10 PI-DRAN0340-GEIG-TITEL          PIC  X(0020).
             10 PI-DRAN0340-GEIG-VNAME-K-INH    PIC  X(0026).
      *
       01  PO-DRAN0340.
           05  PO-DRAN0340-RC                   PIC  X(003).
               88 PO-DRAN0340-RC-OK             VALUE 'OK '.
               88 PO-DRAN0340-RC-NOT-FOUND      VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
