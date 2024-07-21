      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1704.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI1704.CPv  $
      *
      *   Rev 2.13   Dec 02 2020 14:28:18   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT  DEPOTSTAMM
      *
      *   Rev 2.12   Dec 02 2020 07:33:52   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT  DEPOTSTAMM
      *
      *   Rev 2.11   Nov 27 2014 10:14:56   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.10   Jul 07 2014 13:47:14   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF
      *DeuB
      *a Depotbestand
      *
      *   Rev 2.9   Jul 23 2013 11:02:50   t13ex39
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF
      *
      *   Rev 2.8   Dec 18 2012 06:41:10   t13ex39
      *AKI01165: OnlineKI: Erweiterung Anzeige zu db VL-Depots
      *
      *   Rev 2.7   Jul 11 2011 16:16:10   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.7   Apr 14 2011 14:00:00   T13EX18
      *AKI01040: Barausschüttung; Mapping

      *   Rev 2.6   Oct 28 2010 16:45:26   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.5   Oct 05 2010 13:49:28   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.4   06 May 2008 10:13:50   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.3   21 Apr 2008 10:31:18   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.2   21 Apr 2008 10:27:16   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.1   Oct 13 2004 14:08:28   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.0   Jul 14 2004 14:25:02   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI DEPOTBESTAND DEUTSCHE BANK     *
      *    ----------------------------------------------------------  *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   710                                             *
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
      * AKI00892!09.03.10! 1.1! zus. Attribute            !W.Hornung *
      *         !        !    ! DeuBa Depotrente          !          *
      *         !        !    !                           !          *
      * AKI01040!15.04.11! 1.2! zus. Attribute            !M. Dicks  *
      *         !        !    ! DeuBa Barausschüttung     !          *
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
      *--------------------------------------------------------------*
      * AKI01790!25.06.20! BNK-VNR REDEFINIERT            ! WHO      *
      *         !        !                                !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI1704.
               20  BNK-VERARB-KZ                   PIC  X(001).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
      *                D = DELETE
      *
               20  BNK-SA                         PIC  X(001).
      *            SATZART        = 'C'
               20  BNK-GES                        PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  BNK-VNR                        PIC  9(015).
      *            VERTRAGSNUMMER (DEPOTNUMMER)
               20  FILLER REDEFINES BNK-VNR.
                   25 BNK-VNR-1-10                PIC 9(010).
                   25 BNK-UNR                     PIC X(002).
                   25 FILLER                      PIC X(003).
               20  BNK-WP-KENN-NR                 PIC X(06).
      *            WERTPAPIERKENN-NUMMER
               20  BNK-AEND-WP-KENN-NR-KZ         PIC X(01).
      *            ÄNDERUNG-KZ. WERTPAPIERKENN-NUMMER
               20  BNK-WP-KENN-ISIN               PIC X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  BNK-AEND-WP-KENN-ISIN-KZ       PIC X(01).
      *            ÄNDERUNG-KZ. WERTPAPIERKENN-NUMMER ISIN
               20  BNK-ART                        PIC X(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  BNK-AEND-ART-KZ                PIC  X(01).
      *            ÄNDERUNG-KZ. ART DES PAPIERS
               20  BNK-STUECKE-ODER-WERT           PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  BNK-AEND-STUECKE-ODER-WERT-KZ   PIC  X(01).
      *            ÄNDERUNG-KZ. STÜCKANZAHL ODER NOMINALWERT IN
      *            ORIGINALWÄHRUNG
               20  BNK-ORG-WKZ                     PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  BNK-AEND-ORG-WKZ-KZ             PIC  X(01).
      *            ÄNDERUNG-KZ. ORIGINAL-WÄHRUNG DES PAPIERES
               20  BNK-KURSWERT-EUR                PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  BNK-AEND-KURSWERT-EUR-KZ        PIC  X(01).
      *            ÄNDERUNG-KZ. KURSWERT IN EUR
               20  BNK-KURSWERT-ORG-WG             PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  BNK-AEND-KURSWERT-ORG-WG-KZ     PIC  X(01).
      *            ÄNDERUNG-KZ. KURSWERT IN ORG-WÄHRUNG
               20  BNK-GUTHABEN-DATUM-VON          PIC  X(10).
      *            Stand Guthaben
               20  BNK-KUM-EIN                     PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  BNK-AEND-KUM-EIN-KZ             PIC  X(01).
      *            ÄNDERUNG-KZ. Kumulierte Einzahlung
               20  BNK-KUM-AUS                     PIC  9(11)V99.
      *            Kumulierte Auszahlung
               20  BNK-AEND-KUM-AUS-KZ             PIC  X(01).
      *            ÄNDERUNG-KZ. Kumulierte Auszahlung
140408         20  BNK-KUM-AUT-WIEDER-ANLAGE           PIC  9(11)V99.
      *            Kumulierte automatische Wiederanlage
140408         20  BNK-KUM-AUT-WIEDER-ANLAGE-KZ        PIC  x(01).
      *            Kumulierte automatische Wiederanlage
090310         20  BNK-INV-KAPITAL-AKT                 PIC 9(11)V99.
>>    *            inverstiertes Kapital aktuell
               20  BNK-AEND-INV-KAPITAL-AKT-KZ         PIC X(01).
      *            ÄNDERUNG-KZ. iverstiertes Kapital aktuell
               20  BNK-INV-KAPITAL-VOR-JAHR            PIC 9(11)V99.
      *            inverstiertes Kapital Vorjahr
               20  BNK-AEND-INV-KAPITAL-VJ-KZ          PIC X(01).
      *            ÄNDERUNG-KZ. iverstiertes Kapital Vorjahr
               20  BNK-KUM-UEBERTRAEGE                 PIC S9(11)V99.
      *            Kumulierte Übertraäge
               20  BNK-AEND-KUM-UEBERTRAEGE-KZ         PIC X(01).
      *            ÄNDERUNG-KZ. Kumulierte Übertraäge
               20  BNK-KUM-AUTOM-WIEDER-VORJ           PIC S9(11)V99.
      *            Kumulierte automatische Wiederanlage 31.12. des
      *            Vorjahres
               20  BNK-AEND-AUTOM-WIEDER-VORJ-KZ       PIC X(01).
      *            ÄNDERUNG-KZ. Kumulierte Überträge Vorjahr
               20  BNK-DURCH-EINSTANDSKURS             PIC 9(05)V99.
      *            durchschnittlicher Einstandskurs
               20  BNK-AEND-DURCH-KURS-KZ              PIC X(01).
      *            durchschnittlicher Einstandskurs
               20  BNK-QUALITETSFLAG                   PIC 9(01).
      *            Qualitätsflag zum Einstandskurs
               20  BNK-AEND-QUALITETSFLAG-KZ           PIC X(01).
      *            ÄNDERUNG-KZ. Qualitätsflag zum Einstandskurs
               20  BNK-REALISIERTE-GEW-VERL            PIC S9(11)V99.
      *            Realisierte Gewinne/Verluste
<<             20  BNK-AEND-REALISIERTE-GEW-KZ         PIC X(01).
090310*            ÄNDERUNG-KZ. Realisierte Gewinne/Verluste
150411         20  BNK-KUM-BRUTTO-ERTRAGSGUTSCHR       PIC 9(11)V99.
150411*            kum. Bruttobetrag Ertragsausschüttung
150411         20  BNK-AEND-KUM-BRUTTO-ERTR-KZ         PIC X(01).
150111*            Änderungs-KZ kum. Bruttobetrag Ertragsausschüttung
231012         20  BNK-ANTEILE-FREI                    PIC 9(07)V9(04).
>>    *            Anzahl der Anteile ohne Sperrschlüssel 26xx
               20  BNK-AEND-ANTEILE-FREI-KZ            PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-VL-SPERRE-DAT1                  pic X(08).
      *            Ende der Sperrfrist für die unten angegebenen Anteile
               20  BNK-AEND-VL-SPERRE-DAT1-KZ          PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-ANTEILE-GESPERRT1               PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g Termin gesperrten Anteile
               20  BNK-AEND-ANTEILE-GESPERRT1-KZ       PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-VL-SPERRE-DAT2                  pic X(08).
      *            Ende der zweiten Sperrfrist für die angegebenen. Ante
               20  BNK-AEND-VL-SPERRE-DAT2-KZ          PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-ANTEILE-GESPERRT2               PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 2. Termin gesperrten Anteile
               20  BNK-AEND-ANTEILE-GESPERRT2-KZ       PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-VL-SPERRE-DAT3                  pic X(08).
      *            Ende der dritten Sperrfrist für die angegebenen. Ante
               20  BNK-AEND-VL-SPERRE-DAT3-KZ          PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-ANTEILE-GESPERRT3               PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 3. Termin gesperrten Anteile
<<             20  BNK-AEND-ANTEILE-GESPERRT3-KZ       PIC X(01).
231012*            ÄNDERUNG-KZ.
140613         20  BNK-ANZ-ANTEILE-GEKUE-OIF           PIC 9(07)V9(04).
>>    *            Anzahl wegen unwiderruflicher Rückgabe
      *            gekündigte Anteile bei offenen Immobilienfonds
               20  BNK-AEND-ANZ-ANT-GEKUE-OIF-KZ        PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-KUM-AUS-AKT-HALB-JAHR            PIC 9(11)V99.
      *            Kumulierte Auszahlungen aktuelles Kalenderhalbjahr
      *            (ohne Abgänge aus Überträgen)
               20  BNK-AEND-KUM-AUS-AKT-H-JAHR-KZ      PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-KUM-BRUTTO-BETRAG-OIF           PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften in
      *            EUR bei offenen Immobilienfonds in Liquidation ab
      *            Zeitpunkt der Kündigung - gesamt
               20  BNK-AEND-KUM-BRUT-BETR-OIF-KZ       PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-KUM-BRUTTO-BETRAG-OIF-VJ        PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften
      *            in EUR bei offenen Immobilienfonds in Liquidation
      *            - per 31.12. des Vorjahres
               20  BNK-AEND-KUM-BR-BET-OIF-VJ-KZ       PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-KUM-BRUTTO-BETRAG-VJ            PIC 9(11)V99.
      *            Kumulierter Bruttobetrag Ertragsgutschriften in
      *            EUR gesamt - per 31.12. des Vorjahres
               20  BNK-AEND-KUM-BRUT-BETRAG-VJ-KZ      PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-ANZ-ANTEILE-VERFUE-OIF          PIC 9(07)V9(04).
      *            Verfügbarer / kündbarer Anteilsbestand
      *            bei offenen Immobilienfonds
               20  BNK-AEND-ANZ-ANT-VERFUE-OIF-KZ      PIC X(01).
      *            ÄNDERUNG-KZ.
               20  BNK-ANZ-ANTEILE-GESPER-OIF          PIC 9(07)V9(04).
      *            Noch gesperrter Anteilbestand
      *            aufgrund Haltefristen bei offenen Immobilienfonds
               20  BNK-AEND-ANZ-ANT-GESPER-OIF-KZ      PIC X(01).
      *            ÄNDERUNG-KZ.
130514         20  BNK-ANT-VORJU13-OIF                  PIC 9(07)V9(04).
130514*            Antahl Anteile mit erwerb vor 22.07.2013
130514         20  BNK-AEND-ANT-VORJU13-OIF-KZ          PIC X(001).
130514         20  BNK-BER-AUSGE-KONTINGENT             PIC 9(06)V9(02).
130514*            Bereits ausgenutzes Halbjahres-Kontingent WKKN
130514         20  BNK-AEND-BER-AUSGE-KONTING-KZ        PIC X(001).
130514         20  BNK-ANLEGER-ID-OIF                   PIC X(014).
130514*            Anleger ID
130514         20  BNK-END-ANLEGER-ID-OIF-KZ            PIC X(001).
061014         20  BNK-WERT-PAPIER-BEZ            PIC X(050).
>>    *            Wertpapierbezeichnung
               20  BNK-AEND-WERT-PAPIER-KZ        PIC X(001).
               20  BNK-RUECKNAHME-PREIS           PIC S9(005)V9999.
      *            Rücknahmepreis
               20  BNK-AEND-RUECKNAHME-KZ         PIC X(001).
               20  BNK-N-REAL-GW-VL-EUR-INKL      PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  BNK-AEND-N-R-G-V-EURO-INKL-KZ  PIC X(01).
      *            J WENN BIC GEÄNDERT
               20  BNK-N-REAL-GW-VL-VH-INKL       PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  BNK-AEND-N-R-G-V-VH-INKL-KZ    PIC X(01).
      *            J WENN BIC GEÄNDERT
               20  BNK-N-REAL-GW-VL-EUR-OHNE      PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  BNK-AEND-N-R-G-V-EURO-OHNE-KZ  PIC X(01).
      *            J WENN BIC GEÄNDERT
               20  BNK-N-REAL-GW-VL-VH-OHNE       PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  BNK-AEND-N-R-G-V-VH-OHNE-KZ    PIC X(01).
      *            J WENN BIC GEÄNDERT
<<             20  FILLER                         PIC X(164).
061014*            RESERVE


