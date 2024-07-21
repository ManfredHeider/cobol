      *****************************************************************
      *    COPY-MEMBER: DRAN0372                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0372                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01472!16.07.20!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0372
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MIT DATUM UND LAUFZEIT <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0372.
           05  PI-DRAN0372-KEY.
             10 PI-DRAN0372-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0372-PROD-PROD-SCHL   PIC S9(4) COMP.
             10 PI-DRAN0372-ANLZ-GUELTIG-AB   PIC  X(010).
             10 PI-DRAN0372-ANLZ-LZ-VON   PIC S9(4) COMP.

      *
       01  PO-DRAN0372.
           05  PO-DRAN0372-RC                     PIC  X(003).
               88 PO-DRAN0372-RC-OK               VALUE 'OK '.
               88 PO-DRAN0372-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRAN0372-RC-MULT-ROWS        VALUE 'MUR'.
      *
           05  PO-DRAN0372-DATEN.

             10 PO-DRAN0372-ANLZ-LFD-NR   PIC S9(4) COMP.
             10 PO-DRAN0372-ANLZ-GUELTIG-BIS  PIC  X(010).
             10 PO-DRAN0372-ANLZ-KENNUNG-INS   PIC  X(0008).
             10 PO-DRAN0372-ANLZ-KENNUNG-UPD   PIC  X(0008).
             10 PO-DRAN0372-ANLZ-LZ-BIS   PIC S9(4) COMP.
             10 PO-DRAN0372-ANLZ-SATZ   PIC S9(4)V9(4) COMP-3.
             10 PO-DRAN0372-ANLZ-ZEIT-INS   PIC  X(0026).
             10 PO-DRAN0372-ANLZ-ZEIT-UPD   PIC  X(0026).
      *
      ******* ENDE COPY-MEMBER ***************************************

