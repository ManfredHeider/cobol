      ****************************************************************
      *    COPY-MEMBER: DRKI0959 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *                 für LIST-OZP ZXKI0313                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HSE
      *    DATUM:    06.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00081!13.01.99!     NEU                        !WHO       *
      * AKI0303 !20.11.02!     GHALIEFERDATUM neu         !R.Biehl   *
      *DBO-10080!09.08.22!Feldlängenerweiterung           !          *
      *         !        !GHAGUTHABENANT S9(7)V9(4) COMP-3!u131774   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0959                                 *
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
       01 PI-DRKI0959.
           05  PI-DRKI0959-PROGRAMM             PIC  X(008).
           05  PI-DRKI0959-UFKT                 PIC  X(002).
           05  PI-DRKI0959-KEY-VON              PIC  X(013).
           05  PI-DRKI0959-KEY-BIS              PIC  X(013).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
      ***
       01 PO-DRKI0959.
          05 PO-DRKI0959-RC                     PIC  X(003).
             88 PO-DRKI0959-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0959-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0959-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0959-KEY.
             10 PO-DRKI0959-GHAVERTRAGSID        PIC X(13).
             10 PO-DRKI0959-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PO-DRKI0959-GHAFONDID            PIC S9(4) COMP.
             10 PO-DRKI0959-GHALFDNR             PIC S9(4) COMP.
          05 PO-DRKI0959-DATEN.
             10 PO-DRKI0959-GHAFONDTEXT          PIC X(10).
             10 PO-DRKI0959-GHAGUTHABEN          PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0959-GHAGUTHABENWKZ       PIC X(01).
             10 PO-DRKI0959-GHAGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI0959-GHAKUMULAUSZAHLUNG   PIC S9(13)V  COMP-3.
             10 PO-DRKI0959-GHAKUMAUSZWKZ        PIC X(01).
             10 PO-DRKI0959-GHAKUMULEINZAHLUNG   PIC S9(13)V  COMP-3.
             10 PO-DRKI0959-GHAKUMEINZWKZ        PIC X(01).
             10 PO-DRKI0959-GHAWERTENTWGES       PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI0959-GHAWERTENTWLFDJAHR   PIC S9(3)V9(2) COMP-3.
201102       10 PO-DRKI0959-GHALIEFERDATUM       PIC X(10).
      *
      ******** Ende Copy-Member **************************************

