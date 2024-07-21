      ****************************************************************
      *    COPY-MEMBER: DRKI1519 FUNKTION:                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    23.03.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00327!03.03.03!     NEU                        !WHO       *
      * -------------------------------------------------------------*
      * FKI01437!22.01.14!     Erw. Funkt 1519 für AGI    !SWZ       *
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
      *    FÜR DIE FUNKTION DRKI1519                                 *
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
       01 PI-DRKI1519.
           05  PI-DRKI1519-PROGRAMM             PIC  X(008).
           05  PI-DRKI1519-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1519-DATEN.
             10 PI-DRKI1519-GES-VNR-VON         PIC  X(013).
             10 PI-DRKI1519-GES-VNR-BIS         PIC  X(013).
             10 PI-DRKI1519-GHBFONDID           PIC S9(4) COMP.
             10 PI-DRKI1519-GHBGHALFDNR         PIC S9(4) COMP.
             10 PI-DRKI1519-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.

      ***
       01 PO-DRKI1519.
          05 PO-DRKI1519-RC                     PIC  X(003).
             88 PO-DRKI1519-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1519-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1519-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1519-KEY.
             10 PO-DRKI1519-GHBBUCHUNGLFDNR     PIC S9(4) COMP.
             10 PO-DRKI1519-GHBFONDID           PIC S9(4) COMP.
             10 PO-DRKI1519-GHBGHALFDNR         PIC S9(4) COMP.
             10 PO-DRKI1519-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PO-DRKI1519-GHBVERTRAGSID       PIC X(13).
          05 PO-DRKI1519-DATEN.
             10 PO-DRKI1519-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1519-GHBBETRLBUWKZ        PIC X(01).
             10 PO-DRKI1519-GHBBUCHUNGSDATUM     PIC X(10).
             10 PO-DRKI1519-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI1519-GHBKZFINHLETZTBUID   PIC S9(4)      COMP.
             10 PO-DRKI1519-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1519-GHBSTLETZTBUWKZ      PIC X(01).
220114       10 PO-DRKI1519-GHBNETTOBETRAG       PIC S9(9)V9(2) COMP-3.
220114       10 PO-DRKI1519-GHBNETTOBETWKZ       PIC X(1).
220114       10 PO-DRKI1519-GHBGEBBETRAG         PIC S9(9)V9(2) COMP-3.
220114       10 PO-DRKI1519-GHBGEBSCHLUESSEL     PIC S9(4) COMP.
220114       10 PO-DRKI1519-GHBGEBTEXT           PIC  X(20).

      *
      ******** Ende Copy-Member **************************************
