      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1708.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI1708.CPv  $
      *
      *   Rev 2.12   Nov 27 2014 10:15:02   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.11   Jul 07 2014 13:47:08   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF
      *DeuB
      *a Depotbestand
      *
      *   Rev 2.10   Jul 23 2013 11:02:56   t13ex39
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF
      *
      *   Rev 2.9   Dec 18 2012 06:41:20   t13ex39
      *AKI01165: OnlineKI: Erweiterung Anzeige zu db VL-Depots
      *
      *   Rev 2.8   Jul 11 2011 16:16:14   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.8   Apr 15 2011 14:00:00   T13EX18
      *AKI01040: Barausschüttung; Mapping

      *   Rev 2.7   Oct 28 2010 16:45:36   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.6   Oct 05 2010 13:49:36   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.5   May 06 2008 11:43:00   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.4   06 May 2008 10:13:40   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.3   21 Apr 2008 10:31:04   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.2   21 Apr 2008 10:27:04   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.1   Oct 13 2004 14:08:36   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.0   Sep 08 2004 10:29:58   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER für KSDS DEPOTBESTAND VON DEUTSCHE BANK         *
      *    ---------------------------------------------------         *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   700                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00249!15.09.04! 1.0! NEU                       !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
140408* AKI00708!14.02.08!    ! zus. Attribute            !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
090310* AKI00892!09.03.10!    ! zus. Attribute            !WHO       *
      * -------------------------------------------------------------*
090310* AKI01040!15.04.11!    ! zus. Attribut             !MDI       *
      *         !        !    ! KUM-BRUTTO-ERTRAGSGUTSCHR !          *
231012* -------------------------------------------------------------*
231012* AKI01165!23.10-12!    ! zusätzliche Attribute zu  ! WHO      *
      *         !        !    ! VL-Sperrfristen           !          *
140613*--------------------------------------------------------------*
140613* AKI01228!14.05.13!    ! zusätzliche Attribute zu  ! WHO      *
      *         !        !    ! Liquidation OIF und Ant-  !          *
      *         !        !    ! eilsrückgab OIF           !          *
130514*--------------------------------------------------------------*
130514* AKI01227!13.05.14!    ! zusätzliche Attribute zu  ! WHO      *
      *         !        !    ! OIF                       !          *
      *--------------------------------------------------------------*
061014* AKI01327!06.10.14! Erweiterungen für              ! WHO      *
      *         !        ! Wertentwicklung                !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1708.
      *
               20  KSDS-SA                         PIC  X(001).
      *            SATZART        = '6'
               20  KSDS-GES                        PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  KSDS-KEY.
                   25  KSDS-VNR.
                       30 KSDS-FILIALNUMMER        PIC  9(003).
                       30 KSDS-KUNDENNUMMER        PIC  9(007).
                       30 KSDS-UNTERKONTONUMMER    PIC  9(002).
                       30 KSDS-WAEHRUNG            PIC  9(003).
                   25 KSDS-WP-KENN-NR              PIC  X(006).
               20  FILLER REDEFINES KSDS-KEY.
                   30  KSDS-VNR-1                  PIC  9(010).
                   30  FILLER                      PIC  X(011).
               20  KSDS-WP-KENN-ISIN               PIC  X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  KSDS-Art                        PIC  X(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  KSDS-STUECKE-ODER-WERT          PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  KSDS-ORG-WKZ                    PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  KSDS-KURSWERT-EUR               PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  KSDS-KURSWERT-ORG-WG            PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  KSDS-KUM-EIN                    PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  KSDS-KUM-AUS                    PIC  9(11)V99.
      *            Kumulierte Auszahlung
               20  KSDS-DEL-KZ                     PIC  X(01).
      *            LÖSCHKENNZEICHEN
140408         20  KSDS-KUM-AUT-WIEDER-ANLAGE   PIC  9(11)V99.
      *            Kumulierte automatische Wiederanlage
090310         20  KSDS-INV-KAPITAL-AKT         PIC 9(11)V99.
>>    *            inverstiertes Kapital aktuell
               20  KSDS-INV-KAPITAL-VOR-JAHR    PIC 9(11)V99.
      *            inverstiertes Kapital Vorjahr
               20  KSDS-KUM-UEBERTRAEGE         PIC S9(11)V99.
      *            Kumulierte Übertraäge
               20  KSDS-KUM-AUTOM-WIEDER-VORJ   PIC S9(11)V99.
      *            Kumulierte automatische Wiederanlage 31.12. des
      *            Vorjahres
               20  KSDS-DURCH-EINSTANDSKURS     PIC 9(05)V99.
      *            durchschnittlicher Einstandskurs
               20  KSDS-QUALITETSFLAG           PIC 9(01).
      *            Qualitätsflag zum Einstandskurs
<<             20  KSDS-REALISIERTE-GEW-VERL    PIC S9(11)V99.
090310*            Realisierte Gewinne/Verluste
150411         20  KSDS-KUM-BRUTTO-ERTRAGSGUTSCHR PIC 9(11)V99.
150411*            kum. Bruttobetrag Ertragsausschüttung
231012         20  KSDS-ANTEILE-FREI              PIC 9(07)V9(04).
>>    *            Anzahl der Anteile ohne Sperrschlüssel 26xx
               20  KSDS-VL-SPERRE-DAT1            pic X(08).
      *            Ende der Sperrfrist für die unten angegebenen Anteile
               20  KSDS-ANTEILE-GESPERRT1         PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g Termin gesperrten Anteile
               20  KSDS-VL-SPERRE-DAT2            pic X(08).
      *            Ende der zweiten Sperrfrist für die angegebenen. Ante
               20  KSDS-ANTEILE-GESPERRT2         PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 2. Termin gesperrten Anteile
               20  KSDS-VL-SPERRE-DAT3            pic X(08).
<<    *            Ende der dritten Sperrfrist für die angegebenen. Ante
231012         20  KSDS-ANTEILE-GESPERRT3         PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 3. Termin gesperrten Anteile
140613         20  KSDS-ANZ-ANTEILE-GEKUE-OIF     PIC 9(07)V9(04).
>>    *            Anzahl wegen unwiderruflicher Rückgabe
      *            gekündigte Anteile bei offenen Immobilienfonds
               20  KSDS-KUM-AUS-AKT-HALB-JAHR     PIC 9(11)V99.
      *            Kumulierte Auszahlungen aktuelles Kalenderhalbjahr
      *            (ohne Abgänge aus Überträgen) Feld 38
               20  KSDS-KUM-BRUTTO-BETRAG-OIF     PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften in
      *            EUR bei offenen Immobilienfonds in Liquidation ab
      *            Zeitpunkt der Kündigung - gesamt
               20  KSDS-KUM-BRUTTO-BETRAG-OIF-VJ  PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften
      *            in EUR bei offenen Immobilienfonds in Liquidation
      *            - per 31.12. des Vorjahres
               20  KSDS-KUM-BRUTTO-BETRAG-VJ      PIC 9(11)V99.
      *            Kumulierter Bruttobetrag Ertragsgutschriften in
      *            EUR gesamt - per 31.12. des Vorjahres
               20  KSDS-ANZ-ANTEILE-VERFUE-OIF    PIC 9(07)V9(04).
      *            Verfügbarer / kündbarer Anteilsbestand
      *            bei offenen Immobilienfonds    (35)
               20  KSDS-ANZ-ANTEILE-GESPER-OIF    PIC 9(07)V9(04).
      *            Noch gesperrter Anteilbestand
<<    *            aufgrund Haltefristen bei offenen Immobilienfonds
130613*            (36)
130514         20  KSDS-ANT-VORJU13-OIF           PIC 9(07)V9(04).
130514*            Antahl Anteile mit erwerb vor 22.07.2013
130514         20  KSDS-BER-AUSGE-KONTINGENT      PIC 9(06)V9(02).
130514         20  KSDS-ANLEGER-ID-OIF            PIC x(014).
130514*            Anleger ID
061014         20  KSDS-WERT-PAPIER-BEZ           PIC X(050).
>>    *            Wertpapierbezeichnung
               20  KSDS-RUECKNAHME-PREIS          PIC S9(005)V9999.
      *            Rücknahmepreis
               20  KSDS-N-REAL-GW-VL-EUR-INKL   PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  KSDS-N-REAL-GW-VL-VH-INKL    PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  KSDS-N-REAL-GW-VL-EUR-OHNE   PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  KSDS-N-REAL-GW-VL-VH-OHNE    PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
<<
061014         20  FILLER                       PIC  X(205).
130514*            RESERVE
