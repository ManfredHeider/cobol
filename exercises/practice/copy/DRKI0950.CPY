      ****************************************************************
      *    COPY-MEMBER: DRKI0950 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    15.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00069!23.12.99!     NEU                        !WHO       *
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
      *    FÜR DIE FUNKTION DRKI0950                                 *
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
       01 PI-DRKI0950.
           05  PI-DRKI0950-PROGRAMM             PIC  X(008).
           05  PI-DRKI0950-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0950-KEY.
             10 PI-DRKI0950-GHBVERTRAGSID       PIC  X(013).
             10 PI-DRKI0950-GHBFONDID           PIC S9(4) COMP.
             10 PI-DRKI0950-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PI-DRKI0950-GHBGHALFDNR         PIC S9(4) COMP.

      ***
       01 PO-DRKI0950.
          05 PO-DRKI0950-RC                     PIC  X(003).
             88 PO-DRKI0950-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0950-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0950-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0950-KEY.
             10 PO-DRKI0950-GHBBUCHUNGLFDNR     PIC S9(4) COMP.
             10 PO-DRKI0950-GHBFONDID           PIC S9(4) COMP.
             10 PO-DRKI0950-GHBGHALFDNR         PIC S9(4) COMP.
             10 PO-DRKI0950-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PO-DRKI0950-GHBVERTRAGSID       PIC X(13).
          05 PO-DRKI0950-DATEN.
             10 PO-DRKI0950-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0950-GHBBETRLBUWKZ        PIC X(1).
             10 PO-DRKI0950-GHBBUCHUNGSDATUM     PIC X(10).
             10 PO-DRKI0950-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI0950-GHBKZFINHLETZTBUID   PIC S9(4) COMP.
             10 PO-DRKI0950-GHBNETTOBETRAG       PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0950-GHBNETTOBETWKZ       PIC X(1).
             10 PO-DRKI0950-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0950-GHBSTLETZTBUWKZ      PIC X(1).
             10 PO-DRKI0950-GHBGEBBETRAG         PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0950-GHBGEBSCHLUESSEL     PIC S9(4) COMP.
             10 PO-DRKI0950-GHBGEBTEXT           PIC  X(20).



      *
      ******** Ende Copy-Member **************************************

