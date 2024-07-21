      *****************************************************************
      *    COPY-MEMBER: DROK0063                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0063                    *
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
      *    FÜR DIE FUNKTION DROK0063
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0063.
           05  PI-DROK0063-KEY.


             10 PI-DROK0063-AK-KEY   PIC X(26).
             
      *
       01  PO-DROK0063.
           05  PO-DROK0063-RC                 PIC  X(003).
               88 PO-DROK0063-RC-OK                        VALUE 'OK '.
               88 PO-DROK0063-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0063-DATEN.


             10 PO-DROK0063-AK-OKI-ENDE   PIC X(26).
             10 PO-DROK0063-AK-OKI-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0063-AK-OKI-SQL-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0063-AK-OKI-START   PIC X(26).
             10 PO-DROK0063-AK-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0063-AK-SQL-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0063-AK-SQL-ENDE   PIC X(26).
             10 PO-DROK0063-AK-SQL-START   PIC X(26).
             10 PO-DROK0063-AK-SQL-STATUS   PIC X(10).
             10 PO-DROK0063-AK-STATUS   PIC X(1).
             

      *
      ******* ENDE COPY-MEMBER **************************************
