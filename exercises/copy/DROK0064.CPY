      *****************************************************************
      *    COPY-MEMBER: DROK0064                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0064                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.12.2006/13:28:51
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0064
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0064.
           05  PI-DROK0064-KEY.


             10 PI-DROK0064-AK-KEY   PIC X(26).
             

           05  PI-DROK0064-DATEN.


             10 PI-DROK0064-AK-OKI-ENDE   PIC X(26).
             10 PI-DROK0064-AK-OKI-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PI-DROK0064-AK-OKI-SQL-ANZAHL   PIC S9(9) COMP.
             10 PI-DROK0064-AK-OKI-START   PIC X(26).
             10 PI-DROK0064-AK-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PI-DROK0064-AK-SQL-ANZAHL   PIC S9(9) COMP.
             10 PI-DROK0064-AK-SQL-ENDE   PIC X(26).
             10 PI-DROK0064-AK-SQL-START   PIC X(26).
             10 PI-DROK0064-AK-SQL-STATUS   PIC X(10).
             10 PI-DROK0064-AK-STATUS   PIC X(1).
             10 PI-DROK0064-AK-LOGMAS-START        PIC X(026).
             10 PI-DROK0064-AK-LOGMAS-ENDE         PIC X(026).
      *
       01  PO-DROK0064.
           05  PO-DROK0064-RC                 PIC  X(003).
               88 PO-DROK0064-RC-OK                        VALUE 'OK '.
               88 PO-DROK0064-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
