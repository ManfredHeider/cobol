      ****************************************************************
      *    COPY-MEMBER: DRKI1216 FUNKTION:                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    23.03.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00466!23.03.01!     NEU                        !WHO       *
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
      *    FÜR DIE FUNKTION DRKI1216                                 *
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
       01 PI-DRKI1216.
           05  PI-DRKI1216-PROGRAMM             PIC  X(008).
           05  PI-DRKI1216-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1216-DATEN.
             10  PI-DRKI1216-GES-VNR-VON        PIC  X(013).
             10  PI-DRKI1216-GES-VNR-BIS        PIC  X(013).

      ***
       01 PO-DRKI1216.
          05 PO-DRKI1216-RC                     PIC  X(003).
             88 PO-DRKI1216-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1216-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1216-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1216-KEY.
             10 PO-DRKI1216-GHBBUCHUNGLFDNR     PIC S9(4) COMP.
             10 PO-DRKI1216-GHBFONDID           PIC S9(4) COMP.
             10 PO-DRKI1216-GHBGHALFDNR         PIC S9(4) COMP.
             10 PO-DRKI1216-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PO-DRKI1216-GHBVERTRAGSID       PIC X(13).
          05 PO-DRKI1216-DATEN.
             10 PO-DRKI1216-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1216-GHBBETRLBUWKZ        PIC X(01).
             10 PO-DRKI1216-GHBBUCHUNGSDATUM     PIC X(10).
             10 PO-DRKI1216-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI1216-GHBKZFINHLETZTBUID   PIC S9(4)      COMP.
             10 PO-DRKI1216-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1216-GHBSTLETZTBUWKZ      PIC X(01).

      *
      ******** Ende Copy-Member **************************************

