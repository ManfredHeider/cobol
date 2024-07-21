      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2935.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2935.CPv  $
      *
      *   Rev 2.8   Oct 05 2020 16:53:10   u130176
      *AKI01793: Gen-Basisdaten - Korrekturmaßnahmen Handhabung sto
      *rnierte/abgelaufene Verträge und Vertragsteile - Korrektur B
      *ornierte Vertragsteile"
      *
      *   Rev 2.7   Mar 12 2015 13:32:36   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.6   Dec 03 2014 13:29:20   t13ex15
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD ** Uwes***
      *
      *   Rev 2.5   Nov 10 2014 11:36:50   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.4   Sep 29 2014 14:14:28   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.3   Aug 20 2014 17:29:40   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.2   Aug 12 2014 09:48:14   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.1   Aug 06 2014 11:01:26   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.0   Jul 28 2014 15:31:56   t13ex20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2935 Produkte und Beiträge aus           *
      *                          KI-Tabellen ermitteln               *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01312!28.07.14! 01 ! NEUERSTELLUNG             ! UZI      *
      * -------------------------------------------------------------*
180215* AKI01337!18.02.15!2.6 ! KLZ Anpassungen           ! SWZ      *
      * -------------------------------------------------------------*
      * AKI01793!31.08.20!2.8 ! Vertragszustandid hinzug. ! BLO      *
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRKI2935.
          05 PI-DRKI2935-DATEN.
             10 PI-DRKI2935-PARTGESELLSCHAFTID   PIC  S9(4) COMP.
             10 PI-DRKI2935-VERVERTRAGSID        PIC  X(013).
             10 PI-DRKI2935-VERVERTRAGSNUMMER    PIC  X(015).
             10 PI-DRKI2935-VERGESAMTBEITRAG     PIC  S9(9)V9(2) COMP-3.
             10 PI-DRKI2935-VERGESBEITRWKZ       PIC  X(001).
             10 PI-DRKI2935-VERZAHLUNGSWEISEID   PIC  S9(4) COMP.
             10 PI-DRKI2935-VERVERTRAGSBEGINN    PIC  X(010).
             10 PI-DRKI2935-TAGESDATUM-EUR       PIC  X(010).
             10 PI-DRKI2935-FOLGEAUFRUF          PIC  X(001).
             10 PI-DRKI2935-READ-FIRST           PIC  X(001).
             10 PI-DRKI2935-VERVERTRZUSTANDID    PIC  X(02).

      *
       01 PO-DRKI2935.
          05 PO-DRKI2935-DATEN.
             10 PO-DRKI2935-PRODUKTID            PIC 9(04).
             10 PO-DRKI2935-VERS-Summe           PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-VERS-Summe-Dyn       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-Brutto       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-Netto1-ASC   PIC  S9(9)V9(2) COMP-3.
180215       10 PO-DRKI2935-BEITRAG-Netto1-EZ    PIC  S9(9)V9(2) COMP-3.
180215       10 PO-DRKI2935-BEITRAG-Netto1-OEZ   PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-Netto1       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-Netto2       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-GEMZW        PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-D-Brutto     PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-D-Netto1     PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-BEITRAG-D-Netto2     PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-GHPEIGENVERMITTELT   PIC  X(001).
             10 PO-DRKI2935-GHPOFFNUNGSDATUM     PIC  X(010).
             10 PO-DRKI2935-GHPSCHLIESSDATUM     PIC  X(010).
             10 PO-DRKI2935-PRODLFDNR            PIC  S9(4) COMP.
             10 PO-DRKI2935-GHPABSCHLUSSVBNR     PIC  S9(9) COMP.
             10 PO-DRKI2935-WKZ                  PIC  X(001).
             10 PO-DRKI2935-GESGUTHAB            PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-MON-BETRAG           PIC  S9(9)V9(2) COMP-3.
             10 PO-DRKI2935-FOLGEAUFRUF          PIC  X(001).
             10 PO-DRKI2935-READ-FIRST           PIC  X(001).
             10 PO-DRKI2935-Satz-ausgeben        PIC  X(001).
             10 PO-DRKI2935-GHPDEPOTART          PIC  X(001).
             10 PO-DRKI2935-TOPZINS-KZ           PIC  X(001).
             10 PO-DRKI2935-Fehler-Nummer     PIC  9(4).
             10 PO-DRKI2935-Fehler-Feldname   PIC X(015).
             10 PO-DRKI2935-Fehler-Vertrag    PIC X(15) JUSTIFIED RIGHT.
             10 PO-DRKI2935-Fehler-Var-1      PIC X(15).
             10 PO-DRKI2935-Fehler-Var-2      PIC X(15).
             10 PO-DRKI2935-Fehler-Var-3      PIC X(15).
             10 PO-DRKI2935-Fehler-Var-4      PIC X(15).
             10 PO-DRKI2935-Fehler-Var-5      PIC X(15).

          05 PO-DRKI2935-RC                      PIC  X(003).
             88 PO-DRKI2935-RC-OK                         VALUE 'OK '.
             88 PO-DRKI2935-RC-NOK                        VALUE 'NOK'.
             88 PO-DRKI2935-RC-LEER                       VALUE 'LER'.



