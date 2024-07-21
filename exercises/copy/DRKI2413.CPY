      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2413.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2413.CPv  $
      *
      *   Rev 2.5   Jul 24 2017 13:57:58   t13ex27
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en-Abgleich
      *
      *   Rev 2.4   Jun 20 2017 14:01:58   t13ex27
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en-Abgleich
      *
      *   Rev 2.3   Nov 24 2010 10:32:10   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:50:52   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:58:50   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Jun 28 2010 10:58:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2413  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!13.08.10!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2413.
          05 PI-DRKI2413-PROGRAMM               PIC X(008).
          05 PI-DRKI2413-UFKT                   PIC 9(002).
          05 PI-DRKI2413-VETVERTRAGSID          PIC X(013).
      *
xxxx17    05  PO-DRKI2413-ROWS-FETCHED           PIC S9(9) COMP.
      *
       01 PO-DRKI2413.
          05 PO-DRKI2413-DATEN.
           10 PO-DRKI2413-VETPRODUKTID         PIC S9(4) USAGE COMP
                                               OCCURS 100.
           10 PO-DRKI2413-VEZGESCHAEFTSZW      PIC X(2)
                                               OCCURS 100.
           10 PO-DRKI2413-VEZSPARTE-KNR        PIC S9(15)V USAGE COMP-3
                                               OCCURS 100.

          05 PO-DRKI2413-RC                       PIC  X(003).
             88 PO-DRKI2413-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2413-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI2413-RC-LEER                      VALUE 'LER'.


