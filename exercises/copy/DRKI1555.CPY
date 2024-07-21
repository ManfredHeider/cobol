      ****************************************************************
      *    COPY-MEMBER: DRKI1555                                     *
      *              Funktion für ZXKI0313/LIST-OZP GUTHABENANTEIL   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Renate Biehl
      *    DATUM:    02.06.2003                                      *
      *    -------------------------------------------------------   *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00327!02.06.03!     NEU                        !R. Biehl  *
      *DBO-10080!09.08.22!Feldlängenerweiterung für       !          *
      *         !        !GHAGUTHABENANT S9(7)V9(4) COMP-3!u131774   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1555                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ITEM, ITEMBT, MITARBEITER, ERMITTELN LIST       *
      *              - ALLE ITEM-Eintraege mit bestimmtem Status     *
      *                ERMITTELN                                     *
      *              - Alle IBTVBNR zum jeweiligen ITEM-Eintrag      *
      *                ERMITTELN                                     *
      *              - MITARBPERSONENID zur IBTVBNR ERMITTELN        *

      ****************************************************************
       01 PI-DRKI1555.
           05  PI-DRKI1555-PROGRAMM             PIC  X(008).
           05  PI-DRKI1555-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1555-GHAVERTRAGSID        PIC  X(013).
           05  PI-DRKI1555-GHAGUTHABPRODLFDNR   PIC  S9(4) COMP.
      ***
       01 PO-DRKI1555.
          05 PO-DRKI1555-RC                     PIC  X(003).
             88 PO-DRKI1555-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1555-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1555-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1555-KEY.
             10 PO-DRKI1555-GHAVERTRAGSID        PIC X(13).
             10 PO-DRKI1555-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PO-DRKI1555-GHAFONDID            PIC S9(4) COMP.
             10 PO-DRKI1555-GHALFDNR             PIC S9(4) COMP.
          05 PO-DRKI1555-DATEN.
             10 PO-DRKI1555-GHAFONDTEXT          PIC X(10).
             10 PO-DRKI1555-GHAGUTHABEN          PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1555-GHAGUTHABENWKZ       PIC X(01).
             10 PO-DRKI1555-GHAGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI1555-GHAKUMULAUSZAHLUNG   PIC S9(13)V  COMP-3.
             10 PO-DRKI1555-GHAKUMAUSZWKZ        PIC X(01).
             10 PO-DRKI1555-GHAKUMULEINZAHLUNG   PIC S9(13)V  COMP-3.
             10 PO-DRKI1555-GHAKUMEINZWKZ        PIC X(01).
             10 PO-DRKI1555-GHAWERTENTWGES       PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1555-GHAWERTENTWLFDJAHR   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1555-GHALIEFERDATUM       PIC X(10).
      *
      ******** Ende Copy-Member **************************************
      *
      *---------------------------------------------------------------*
