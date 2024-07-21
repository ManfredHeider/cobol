      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3385.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI3385.CPv  $
      *
      *   Rev 2.6   Nov 22 2018 11:18:46   t13ae52
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.5   Nov 05 2018 16:25:26   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.4   Nov 05 2018 15:58:22   t13ex78
      *
      *   Rev 2.3   Oct 31 2018 17:12:24   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.2   Oct 29 2018 12:07:36   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.1   Oct 25 2018 14:49:24   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.0   Aug 21 2018 16:23:32   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für DEPOTBESTAND          VON Generali Invest *
      *                                                                *
      *    SATZLÄNGE   400                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              ! WHO       *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
         05  'XX'-DTA-SATZ.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(001).
      *            SATZART = 'A'
               15  'XX'-V-ABSENDER      PIC  X(027).
      *            ABSENDER = 'GEI'
               15  'XX'-V-DAT-STAND     PIC  9(008).
      *            Datum = 'TTMMJJJJ' Stand
               15  'XX'-V-DATUM         PIC  9(008).
      *            Datum = 'TTMMJJJJ' Erstellung
               15  'XX'-V-EMPFAENGER    PIC  X(027).
      *            EMPFAENGER = 'ALAG'
               15  'XX'-V-TEXT          PIC  X(020).
      *            'DEPOTBESTAND'
               15  'XX'-V-ART           PIC  X(001).
      *            K = Komplettbestand, D für Delta
               15  FILLER               PIC  X(308).
      *            RESERVE
      *
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                      PIC X(01).
      *            SATZART        = 'C'
               20  'XX'-GES                     PIC 9(02).
      *            GESELLSCHAFT   = '42'
               20  'XX'-KEY.
                   25  'XX'-VNR                 PIC X(15).
                   25  FILLER   REDEFINES 'XX'-VNR.
                       30  'XX'-VNR-NUM         PIC 9(15).
                   25  'XX'-WKN                 PIC X(06).
      *            WERTPAPIERKENN-NUMMER
               20  'XX'-ISIN                    PIC X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  'XX'-ART                     PIC 9(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  'XX'-STUECKE-ODER-WERT       PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  'XX'-ORG-WKZ                 PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  'XX'-KURSWERT-EUR            PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  'XX'-KUM-AUT-WIEDER          PIC  9(11)V99.
      *            Kumulierte automatische Wiederanlage
               20  'XX'-INV-KAP-AKT             PIC 9(11)V99.
      *            investiertes Kapital aktuell
               20  'XX'-KUM-WIEDER-VORJ         PIC 9(11)V99.
      *            Kumulierte automatische Wiederanlage 31.12. des
      *            Vorjahres
               20  'XX'-DURCH-KURS              PIC 9(05)V99.
      *            durchschnittlicher Einstandskurs
               20  'XX'-QUALITAETSFLAG          PIC 9(01).
      *            Qualitätsflag zum Einstandskurs
               20  'XX'-REALISIERTE-GUV         PIC S9(11)V99.
      *            Realisierte Gewinne/Verlust
               20  'XX'-KUM-BRUTTO-ERTR           PIC 9(11)V99.
      *            kum. Bruttobetrag Ertragsausschüttung
               20  'XX'-ANTEILE-FREI              PIC 9(07)V9(04).
      *            Anzahl der Anteile ohne Sperrschlüssel 26xx
               20  'XX'-VL-SPERRE-DAT1            PIC 9(08).
      *            Ende der Sperrfrist für die unten angegebenen Anteile
               20  'XX'-ANTEILE-GESP1             PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g Termin gesperrten Anteile
               20  'XX'-VL-SPERRE-DAT2            PIC 9(08).
      *            Ende der zweiten Sperrfrist für die angegebenen. Ante
               20  'XX'-ANTEILE-GESP2             PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 2. Termin gesperrten Anteile
               20  'XX'-VL-SPERRE-DAT3            PIC 9(08).
      *            Ende der dritten Sperrfrist für die angegebenen. Ante
               20  'XX'-ANTEILE-GESP3             PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 3. Termin gesperrten Anteile
               20  'XX'-ANZ-ANT-GEKUE-OIF         PIC 9(07)V9(04).
      *            Anzahl wegen unwiderruflicher Rückgabe
      *            gekündigte Anteile bei offenen Immobilienfonds
               20  'XX'-KUM-BRUTTO-BETRAG-OIF     PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften in
      *            EUR bei offenen Immobilienfonds in Liquidation ab
      *            Zeitpunkt der Kündigung - gesamt
               20  'XX'-KUM-BR-BETR-OIF-VJ        PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften
      *            in EUR bei offenen Immobilienfonds in Liquidation
      *            - per 31.12. des Vorjahres
               20  'XX'-KUM-BRUTTO-BETRAG-VJ      PIC 9(11)V99.
      *            Kumulierter Bruttobetrag Ertragsgutschriften in
      *            EUR gesamt - per 31.12. des Vorjahres
               20  'XX'-WERT-PAPIER-BEZ           PIC X(050).
      *            Wertpapierbezeichnung
               20  'XX'-RUECKNAHME-PREIS          PIC S9(005)V9999.
      *            Rücknahmepreis
               20  'XX'-N-REAL-GW-VL-EUR-INKL   PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-VH-INKL    PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-EUR-OHNE   PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-VH-OHNE    PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
 
               15  FILLER                     PIC  X(040).
      *            RESERVE
 
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             '9'
               20  'XX'-N-ANZ           PIC 9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER               PIC X(395).
      *            RESERVE
 
 
 
 
 
 

