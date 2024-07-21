      ****************************************************************
      *    COPY-MEMBER: DRKI0958 FUNKTION:                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    13.01.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00081!13.01.00!     NEU                        !WHO       *
      *DBO-10227!11.08.22!Längenänderung                  !u131774   *
      *         !        !GHBGUTHABENANT von 10 auf 11 B  !          * 
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0958                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION:                                                 *
      *              - ALLE GUTHABENBUCHUNGEN ZU EINER GESELLSCHAFT  *
      *                ERMITTELN                                     *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0958.
           05  PI-DRKI0958-PROGRAMM             PIC  X(008).
           05  PI-DRKI0958-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0958-DATEN.
             10  PI-DRKI0958-GES-VNR-VON        PIC  X(013).
             10  PI-DRKI0958-GES-VNR-BIS        PIC  X(013).

      ***
       01 PO-DRKI0958.
          05 PO-DRKI0958-RC                     PIC  X(003).
             88 PO-DRKI0958-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0958-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0958-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0958-KEY.
             10 PO-DRKI0958-GHBBUCHUNGLFDNR     PIC S9(4) COMP.
             10 PO-DRKI0958-GHBFONDID           PIC S9(4) COMP.
             10 PO-DRKI0958-GHBGHALFDNR         PIC S9(4) COMP.
             10 PO-DRKI0958-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PO-DRKI0958-GHBVERTRAGSID       PIC X(13).
          05 PO-DRKI0958-DATEN.
             10 PO-DRKI0958-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0958-GHBBETRLBUWKZ        PIC X(01).
             10 PO-DRKI0958-GHBBUCHUNGSDATUM     PIC X(10).
             10 PO-DRKI0958-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI0958-GHBKZFINHLETZTBUID   PIC S9(4)      COMP.
             10 PO-DRKI0958-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0958-GHBSTLETZTBUWKZ      PIC X(01).

      *
      ******** Ende Copy-Member **************************************

