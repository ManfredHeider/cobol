      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1703.CPY  $
      *    $Revision:   2.17  $
      *
      *    $Log:   L:/mf/CPY/DRKI1703.CPv  $
      *
      *   Rev 2.17   Dec 02 2020 14:52:46   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.16   Dec 02 2020 10:52:28   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.15   Dec 02 2020 10:31:58   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.14   Dec 02 2020 09:55:50   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.13   Dec 02 2020 09:45:08   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.12   Nov 23 2020 17:16:00   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *B E S T A N D
      *
      *   Rev 2.11   Nov 27 2014 10:14:52   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.10   Jul 07 2014 13:47:24   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF
      *DeuB
      *a Depotbestand
      *
      *   Rev 2.9   Jul 23 2013 11:02:44   t13ex39
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF
      *
      *   Rev 2.8   Dec 18 2012 06:42:28   t13ex39
      *AKI01165: OnlineKI: Erweiterung Anzeige zu 'XX' VL-Depots
      *
      *   Rev 2.7   Jul 11 2011 16:16:04   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.7   Apr 15 2011 14:00:00   T13EX18
      *AKI01040: Barausschüttung; Mapping
      *
      *   Rev 2.6   Oct 28 2010 16:45:12   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.5   Oct 05 2010 13:49:22   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.4   06 May 2008 10:13:46   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.3   21 Apr 2008 10:31:10   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.2   21 Apr 2008 10:27:10   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.1   Oct 13 2004 14:08:20   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.0   Jul 14 2004 14:24:46   t13ex39
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR ÄNDERUNGSBESTAND DWS DEPOTBESTAND           *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 700 bytes (Erweiterung von bisher 300 Bytes)        *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00249!10.09.04!01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00708!14.04.08!    ! neues Attribut            ! WHO      *
      * -------------------------------------------------------------*
090310* AKI00892!09.03.10!    ! zus. Attribute Depotren-  !WHO       *
      *         !        !    ! dite                      !          *
      * -------------------------------------------------------------*
150411* AKI01040!15.04.11!    ! zus. Attribut             !MDI       *
150411*         !        !    ! Barausschüttung           !          *
150411*         !        !    ! Erweiterung der Satzlänge !          *
150411*         !        !    ! von 200 auf 300 Bytes     !          *
150411*         !        !    ! (s. Datenspiegel 3.3.2011)!          *
231012* -------------------------------------------------------------*
231012* AKI01165!23.10-12!    ! zusätzliche Attribute zu  ! WHO      *
      *         !        !    ! VL-Sperrfristen           !          *
      *         !        !    !                           !          *
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
      *---------------------------------------------------------------
      * AKI01805!02.12.20! DB --> 'XX'                    ! ATAR     *
      *         !        ! KUM-BRUTTO-ERTRAGSGUTSCHR      !          *
      *         !        !    -> KUM-BRUTTO-ERTRAGSGUTS   !          *
      *         !        !                                !          *
      *         !        !    KUM-BRUTTO-BETRAG-OIF-VJ    !          *
      *         !        !    -> KUM-BRUTTO-BET-OIF-VJ    !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1703.
      *
           10  'XX'-VORSATZ.
      *
               20  'XX'-V-SA            PIC X(001).
      *            SATZART = '1'
               20  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = ''XX' AG'
               20  'XX'-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTBESTAND     TÄGLICH   ŸBESTANDT'
      *            DEPOTBESTAND     KOMPLETT  ŸBESTANDK'
               20  'XX'-V-WKZ           PIC X(003).
      *            WÄHRUNGS-KENNZ. =  'EUR'
140613         20  FILLER             PIC X(668).

      *            RESERVE

      *
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                      PIC  X(001).
      *            SATZART        = '5'
               20  'XX'-GES                     PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  'XX'-KEY.
                   25  'XX'-VNR.
                       35  'XX'-FILIAL-NR       PIC 9(03).
                       35  'XX'-KDNR            PIC 9(07).
                       35  'XX'-UNTER-DEP-NR    PIC 9(02).
                       35  'XX'-WKZ             PIC 9(03).
                   25  FILLER   REDEFINES 'XX'-VNR.
                       35  'XX'-VNR-1           PIC 9(10).
                       35  FILLER             PIC X(05).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25  'XX'-WP-KENN-NR          PIC X(06).
      *            WERTPAPIERKENN-NUMMER
               20  'XX'-WP-KENN-ISIN            PIC X(12).
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
               20  'XX'-KURSWERT-ORG-WG         PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  'XX'-KUM-EIN                 PIC  9(11)V99.
      *            KUMULIERTE EINZAHLUNG
               20  'XX'-KUM-AUS                 PIC  9(11)V99.
      *            KUMULIERTE AUSZAHLUNG
               20  'XX'-DEL-KZ                  PIC  X(01).
      *            LÖSCH-KENNZEICHEN
140408         20  'XX'-KUM-AUT-WIEDER-ANLAGE   PIC  9(11)V99.
      *            KUMULIERTE AUTOMATISCHE WIEDERANLAGE
090310         20  'XX'-INV-KAPITAL-AKT         PIC 9(11)V99.
>>    *            INVERSTIERTES KAPITAL AKTUELL
               20  'XX'-INV-KAPITAL-VOR-JAHR    PIC 9(11)V99.
      *            INVERSTIERTES KAPITAL VORJAHR
               20  'XX'-KUM-UEBERTRAEGE         PIC S9(11)V99.
      *            KUMULIERTE ÜBERTRAÄGE
               20  'XX'-KUM-AUTOM-WIEDER-VORJ   PIC 9(11)V99.
      *            KUMULIERTE AUTOMATISCHE WIEDERANLAGE 31.12. DES
      *            VORJAHRES
               20  'XX'-DURCH-EINSTANDSKURS     PIC 9(05)V99.
      *            DURCHSCHNITTLICHER EINSTANDSKURS
               20  'XX'-QUALITETSFLAG           PIC 9(01).
      *            QUALITÄTSFLAG ZUM EINSTANDSKURS
<<             20  'XX'-REALISIERTE-GEW-VERL    PIC S9(11)V99.
090310*            REALISIERTE GEWINNE/VERLUST
150411         20  'XX'-KUM-BRUTTO-ERTRAGSGUTS PIC 9(11)V99.
150411*            KUM. BRUTTOBETRAG ERTRAGSAUSSCHÜTTUNG
231012         20  'XX'-ANTEILE-FREI              PIC 9(07)V9(04).
>>    *            ANZAHL DER ANTEILE OHNE SPERRSCHLÜSSEL 26XX
               20  'XX'-VL-SPERRE-DAT1            PIC X(08).
      *            ENDE DER SPERRFRIST FÜR DIE UNTEN ANGEGEBENEN ANTEILE
               20  'XX'-ANTEILE-GESPERRT1         PIC 9(07)V9(04).
      *            ANZAHL DER BIS ZUM O.G TERMIN GESPERRTEN ANTEILE
               20  'XX'-VL-SPERRE-DAT2            PIC X(08).
      *            ENDE DER ZWEITEN SPERRFRIST FÜR DIE ANGEGEBENEN. ANTE
               20  'XX'-ANTEILE-GESPERRT2         PIC 9(07)V9(04).
      *            ANZAHL DER BIS ZUM O.G. 2. TERMIN GESPERRTEN ANTEILE
               20  'XX'-VL-SPERRE-DAT3            PIC X(08).
<<    *            ENDE DER DRITTEN SPERRFRIST FÜR DIE ANGEGEBENEN. ANTE
231012         20  'XX'-ANTEILE-GESPERRT3         PIC 9(07)V9(04).
      *            ANZAHL DER BIS ZUM O.G. 3. TERMIN GESPERRTEN ANTEILE
140613         20  'XX'-ANZ-ANTEILE-GEKUE-OIF     PIC 9(07)V9(04).
>>    *            ANZAHL WEGEN UNWIDERRUFLICHER RÜCKGABE
      *            GEKÜNDIGTE ANTEILE BEI OFFENEN IMMOBILIENFONDS
               20  'XX'-KUM-AUS-AKT-HALB-JAHR     PIC 9(11)V99.
      *            KUMULIERTE AUSZAHLUNGEN AKTUELLES KALENDERHALBJAHR
      *            (OHNE ABGÄNGE AUS ÜBERTRÄGEN)
               20  'XX'-KUM-BRUTTO-BETRAG-OIF     PIC 9(11)V99.
      *            KUMULIERTER BRUTTOBETRAG DER ERTRAGSGUTSCHRIFTEN IN
      *            EUR BEI OFFENEN IMMOBILIENFONDS IN LIQUIDATION AB
      *            ZEITPUNKT DER KÜNDIGUNG - GESAMT
               20  'XX'-KUM-BRUTTO-BET-OIF-VJ  PIC 9(11)V99.
      *            KUMULIERTER BRUTTOBETRAG DER ERTRAGSGUTSCHRIFTEN
      *            IN EUR BEI OFFENEN IMMOBILIENFONDS IN LIQUIDATION
      *            - PER 31.12. DES VORJAHRES
               20  'XX'-KUM-BRUTTO-BETRAG-VJ      PIC 9(11)V99.
      *            KUMULIERTER BRUTTOBETRAG ERTRAGSGUTSCHRIFTEN IN
      *            EUR GESAMT - PER 31.12. DES VORJAHRES
               20  'XX'-ANZ-ANTEILE-VERFUE-OIF    PIC 9(07)V9(04).
      *            VERFÜGBARER / KÜN'XX'ARER ANTEILSBESTAND
      *            BEI OFFENEN IMMOBILIENFONDS
               20  'XX'-ANZ-ANTEILE-GESPER-OIF    PIC 9(07)V9(04).
<<    *            NOCH GESPERRTER ANTEILBESTAND
140613*            AUFGRUND HALTEFRISTEN BEI OFFENEN IMMOBILIENFONDS
130514         20  'XX'-ANT-VORJU13-OIF           PIC 9(07)V9(04).
130514*            ANTAHL ANTEILE MIT ERWERB VOR 22.07.2013
130514         20  'XX'-BER-AUSGE-KONTINGENT      PIC 9(06)V9(02).
130514*            BEREITS AUSGENUTZES HALBJAHRES-KONTINGENT WKKN
130514         20  'XX'-ANLEGER-ID-OIF            PIC X(014).
130514*            ANLEGER ID
061014         20  'XX'-WERT-PAPIER-BEZ           PIC X(050).
>>    *            WERTPAPIERBEZEICHNUNG
               20  'XX'-RUECKNAHME-PREIS          PIC S9(005)V9999.
      *            RÜCKNAHMEPREIS
               20  'XX'-N-REAL-GW-VL-EUR-INKL   PIC S9(11)V99.
      *            NICHT REALISIERTER GEWINN/ VERLUST IN EURO  INKL.
      *           BARAUSSCHÜTTUNGEN  ALLER BESTÄNDE IN DIESEM UNTERDEPOT
               20  'XX'-N-REAL-GW-VL-VH-INKL    PIC S9(06)V99.
      *            NICHT REALISIERTER GEWINN/ VERLUST IN PROZENT  INKL.
      *           BARAUSSCHÜTTUNGEN  ALLER BESTÄNDE IN DIESEM UNTERDEPOT
               20  'XX'-N-REAL-GW-VL-EUR-OHNE   PIC S9(11)V99.
      *            NICHT REALISIERTER GEWINN/ VERLUST IN EURO OHNE .
      *           BARAUSSCHÜTTUNGEN  ALLER BESTÄNDE IN DIESEM UNTERDEPOT
               20  'XX'-N-REAL-GW-VL-VH-OHNE    PIC S9(06)V99.
      *            NICHT REALISIERTER GEWINN/ VERLUST IN PROZENT OHNE
      *           BARAUSSCHÜTTUNGEN  ALLER BESTÄNDE IN DIESEM UNTERDEPOT
061014
<<             15  FILLER                     PIC  X(205).
      *            RESERVE

           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             '9'
               20  'XX'-N-ANZ           PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
140613         20  FILLER              PIC X(692).

      *            RESERVE
