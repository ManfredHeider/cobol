      ****************************************************************
      *    COPY-MEMBER: DRKI1737 FUNKTION:                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    13.01.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00885!10.11.04!     NEU                        !WHO       *
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
      *    FÜR DIE FUNKTION DRKI1737                                 *
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
       01 PI-DRKI1737.
           05  PI-DRKI1737-PROGRAMM             PIC  X(008).
           05  PI-DRKI1737-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1737-DATEN.
             10  PI-DRKI1737-GES-VNR-VON        PIC  X(013).
             10  PI-DRKI1737-GES-VNR-BIS        PIC  X(013).
             10  PI-DRKI1737-GHBFONDID          PIC S9(4) COMP.
             10  PI-DRKI1737-GHBGHALFDNR        PIC S9(4) COMP.
             10  PI-DRKI1737-GHBGUTHABPRODLFDNR PIC S9(4) COMP.
             10  PI-DRKI1737-GHBBETRAGLETZTBUCH PIC S9(9)V9(2) COMP-3.
             10  PI-DRKI1737-GHBBUCHUNGSDATUM   PIC X(10).
             10  PI-DRKI1737-GHBGUTHABENANT     PIC S9(7)V9(4) COMP-3.
             10  PI-DRKI1737-GHBKZFINHLETZTBUID PIC S9(4) COMP.


      ***
       01 PO-DRKI1737.
          05 PO-DRKI1737-RC                     PIC  X(003).
             88 PO-DRKI1737-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1737-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1737-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1737-KEY.
             10 PO-DRKI1737-GHBBUCHUNGLFDNR     PIC S9(4) COMP.
             10 PO-DRKI1737-GHBFONDID           PIC S9(4) COMP.
             10 PO-DRKI1737-GHBGHALFDNR         PIC S9(4) COMP.
             10 PO-DRKI1737-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PO-DRKI1737-GHBVERTRAGSID       PIC X(13).
          05 PO-DRKI1737-DATEN.
             10 PO-DRKI1737-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1737-GHBBETRLBUWKZ        PIC X(01).
             10 PO-DRKI1737-GHBBUCHUNGSDATUM     PIC X(10).
             10 PO-DRKI1737-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI1737-GHBKZFINHLETZTBUID   PIC S9(4)      COMP.
             10 PO-DRKI1737-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1737-GHBSTLETZTBUWKZ      PIC X(01).

      *
      ******** Ende Copy-Member **************************************

