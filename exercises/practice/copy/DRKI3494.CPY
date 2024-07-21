      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3494.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI3494.CPv  $
      *
      *   Rev 2.6   Nov 22 2018 11:18:38   t13ae52
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.5   Nov 05 2018 16:25:16   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.4   Nov 05 2018 15:57:54   t13ex78
      *
      *   Rev 2.3   Oct 31 2018 17:12:16   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.2   Oct 29 2018 12:07:20   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.1   Oct 25 2018 14:49:14   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.0   Sep 13 2018 14:42:32   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *--------------------------------------------------------------*
      *                                                              *
      *  Copy-Member für Ausgabedatei DEPOTBESTAND G42 Generali Inv. *
      *    ----------------------------------------------------------*
      *                                                              *
      *  Satzlänge - 710 Bytes                                       *
      *                                                              *
      *--------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!17.09.18! 2.1! Neuerstellung             ! tfl      *
      * -------------------------------------------------------------*
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
 
       01  'XX'-SATZ.
      * Verarbeitungskennzeichen
      * I = Insert, U = Update, D= Delete
           05  'XX'-VERARB-KZ                       PIC X(01).
      * Satzart = 'C'
           05  'XX'-SA                              PIC X(01).
      * Gesellschaft = '42'
           05  'XX'-GES                             PIC 9(02).
      * Vertragsnummer mit führenden Nullen
           05  'XX'-VNR                             PIC 9(15).
      * Wertpapierkennnummer (WKN)
           05  'XX'-WKN                             PIC X(06).
      * ISIN
           05  'XX'-ISIN                            PIC X(12).
      * Stand des Guthabens (aus dem A-Satz des DTA entnommen)
           05  'XX'-GUTHABEN-DATUM                  PIC X(08).
      * Art des Papiers
      * 1 = Stücke
      * 2 = Nominalwerte
           05  'XX'-ART                             PIC X(01).
           05  'XX'-ART-KZ                          PIC X(01).
      * Stückzahl oder Nominalwert in Originalwährung
           05  'XX'-STUECKE-ODER-WERT               PIC 9(12)V9999.
           05  'XX'-STUECKE-ODER-WERT-KZ            PIC X(01).
      * Originalwährung des Papiers
           05  'XX'-ORG-WKZ                         PIC X(03).
           05  'XX'-ORG-WKZ-KZ                      PIC X(01).
      * Kurswert in EUR
           05  'XX'-KURSWERT-EUR                    PIC 9(10)V99.
           05  'XX'-KURSWERT-EUR-KZ                 PIC X(01).
      * Kumulierte automatische Wiederanlage
           05  'XX'-KUM-AUT-WIEDER                  PIC 9(11)V99.
           05  'XX'-KUM-AUT-WIEDER-KZ               PIC X(01).
      * investiertes Kapital aktuell
           05  'XX'-INV-KAP-AKT                     PIC 9(11)V99.
           05  'XX'-INV-KAP-AKT-KZ                  PIC X(01).
      * Kumulierte automatische Wiederanlage 31.12. des Vorjahres
           05  'XX'-KUM-WIEDER-VORJ                 PIC S9(11)V99.
           05  'XX'-KUM-WIEDER-VORJ-KZ              PIC X(01).
      * durchschnittlicher Einstandskurs
           05  'XX'-DURCH-KURS                      PIC 9(05)V99.
           05  'XX'-DURCH-KURS-KZ                   PIC X(01).
      * Qualitätsflag zum Einstandskurs
           05  'XX'-QUALITAETSFLAG                  PIC 9(01).
           05  'XX'-QUALITAETSFLAG-KZ               PIC X(01).
      * Realisierte Gewinne/Verluste
           05  'XX'-REALISIERTE-GUV                 PIC S9(11)V99.
           05  'XX'-REALISIERTE-GUV-KZ              PIC X(01).
      * kum. Bruttobetrag Ertragsausschüttung
           05  'XX'-KUM-BRUTTO-ERTR                 PIC 9(11)V99.
           05  'XX'-KUM-BRUTTO-ERTR-KZ              PIC X(01).
      * Anzahl der Anteile ohne Sperrschlüssel 26xx
           05  'XX'-ANTEILE-FREI                    PIC 9(07)V9(04).
           05  'XX'-ANTEILE-FREI-KZ                 PIC X(01).
      * Ende der Sperrfrist für die unten angegebenen Anteile
           05  'XX'-VL-SPERRE-DAT1                  PIC X(08).
           05  'XX'-VL-SPERRE-DAT1-KZ               PIC X(01).
      * Anzahl der bis zum o.g Termin gesperrten Anteile
           05  'XX'-ANTEILE-GESP1                   PIC 9(07)V9(04).
           05  'XX'-ANTEILE-GESP1-KZ                PIC X(01).
      * Ende der zweiten Sperrfrist für die angegebenen. Ante
           05  'XX'-VL-SPERRE-DAT2                  PIC X(08).
           05  'XX'-VL-SPERRE-DAT2-KZ               PIC X(01).
      * Anzahl der bis zum o.g. 2. Termin gesperrten Anteile
           05  'XX'-ANTEILE-GESP2                   PIC 9(07)V9(04).
           05  'XX'-ANTEILE-GESP2-KZ                PIC X(01).
      * Ende der dritten Sperrfrist für die angegebenen. Ante
           05  'XX'-VL-SPERRE-DAT3                  PIC X(08).
           05  'XX'-VL-SPERRE-DAT3-KZ               PIC X(01).
      * Anzahl der bis zum o.g. 3. Termin gesperrten Anteile
           05  'XX'-ANTEILE-GESP3                   PIC 9(07)V9(04).
           05  'XX'-ANTEILE-GESP3-KZ                PIC X(01).
      * Anzahl wegen unwiderruflicher Rückgabe
      * gekündigte Anteile bei offenen Immobilienfonds
           05  'XX'-ANZ-ANT-GEKUE-OIF               PIC 9(07)V9(04).
           05  'XX'-ANZ-ANT-GEKUE-OIF-KZ            PIC X(01).
      * Kumulierter Bruttobetrag der Ertragsgutschriften in
      * EUR bei offenen Immobilienfonds in Liquidation ab
      * Zeitpunkt der Kündigung - gesamt
           05  'XX'-KUM-BRUTTO-BETRAG-OIF           PIC 9(11)V99.
           05  'XX'-KUM-BR-BETR-OIF-KZ              PIC X(01).
      * Kumulierter Bruttobetrag der Ertragsgutschriften
      * in EUR bei offenen Immobilienfonds in Liquidation
      * - per 31.12. des Vorjahres
           05  'XX'-KUM-BR-BETR-OIF-VJ              PIC 9(11)V99.
           05  'XX'-KUM-BR-BET-OIF-VJ-KZ            PIC X(01).
      * Kumulierter Bruttobetrag Ertragsgutschriften in
      * EUR gesamt - per 31.12. des Vorjahres
           05  'XX'-KUM-BRUTTO-BETRAG-VJ            PIC 9(11)V99.
           05  'XX'-KUM-BRUT-BETRAG-VJ-KZ           PIC X(01).
      * Wertpapierbezeichnung
           05  'XX'-WERT-PAPIER-BEZ                 PIC X(050).
           05  'XX'-WERT-PAPIER-KZ                  PIC X(001).
      * Rücknahmepreis
           05  'XX'-RUECKNAHME-PREIS                PIC S9(005)V9999.
           05  'XX'-RUECKNAHME-KZ                   PIC X(001).
      * nicht realisierter Gewinn/ Verlust in Euro  inkl.
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05  'XX'-N-REAL-GW-VL-EUR-INKL           PIC S9(11)V99.
           05  'XX'-N-R-G-V-EURO-INKL-KZ            PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05  'XX'-N-REAL-GW-VL-VH-INKL            PIC S9(06)V99.
           05  'XX'-N-R-G-V-VH-INKL-KZ              PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Euro ohne .
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05  'XX'-N-REAL-GW-VL-EUR-OHNE           PIC S9(11)V99.
           05  'XX'-N-R-G-V-EURO-OHNE-KZ            PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Prozent ohne
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05  'XX'-N-REAL-GW-VL-VH-OHNE            PIC S9(06)V99.
           05  'XX'-N-R-G-V-VH-OHNE-KZ              PIC X(01).
      * Reserve
           05  FILLER                               PIC X(164).
 
 
 
 
 

