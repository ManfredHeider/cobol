      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1716.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1716.CPv  $
      *
      *   Rev 2.7   Dec 02 2020 14:28:26   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT  DEPOTSTAMM
      *
      *   Rev 2.6   Dec 02 2020 07:33:58   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT  DEPOTSTAMM
      *
      *   Rev 2.5   Jan 17 2018 09:36:54   t13ex78
      *AKI01562: Programme und Copies
      *
      *   Rev 2.4   Jan 07 2016 14:16:56   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.3   Nov 27 2014 10:14:40   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.2   Sep 02 2013 12:17:40   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Depotstammsatz
      *
      *   Rev 2.1   Oct 13 2004 14:08:54   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.0   Sep 15 2004 15:30:02   t13ex39
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI DEPOTBSTAMM DEUTSCHE BANK      *
      *    ------------------------------------------------------      *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   076                                             *
290713*    SATZLÄNGE   210                                             *
061014*    SATZLÄNGE   313                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00249!15.09.04! 1.0! NEU                       !WHO       *
      *--------------------------------------------------------------*
290713* AKI01204!29.07.13!    !NEUE FELDER EINGEFÜGT f. SEPA!WMI     *
290713*         !        !    !  UND DEN FILLER ANGEPASST   !        *
      *--------------------------------------------------------------*
061014* AKI01327!06.10.14! Erweiterungen für              ! WHO      *
      *         !        ! Wertentwicklung                !          *
      *--------------------------------------------------------------*
161115* AKI01396!16.11.15! neues Attrib. SONDERKONDITIONEN! WHO      *
      *--------------------------------------------------------------*
100118* AKI01562!10.01.18! neues Attribut WPHGDATUM       ! tfl      *
      *--------------------------------------------------------------*
      * AKI01790!25.06.20! BNK-VNR REDEFINIERT            ! WHO      *
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
 
       01  DRKI1716.
      *
               20  BNK-VERARB-KZ                  PIC X(01).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
      *                D = Delete
               20  BNK-SA                         PIC X(01).
      *            SATZART        = 'C'
               20  BNK-GES                        PIC 9(02).
      *            GESELLSCHAFT   = '48'
               20  BNK-VNR                        PIC 9(15).
      *            VERTRAGSNUMMER (DEPOTNUMMER)
               20  FILLER REDEFINES BNK-VNR.
                   25 BNK-VNR-1-10                PIC 9(010).
                   25 BNK-UNR                     PIC X(002).
                   25 FILLER                      PIC X(003).
               20  BNK-DAT-DEPOT-EROEFF           PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  BNK-AEND-DAT-DEPOT-EROEFF-KZ   PIC X(01).
      *            ÄNDERUNG-KZ. DEPOTERÖFFNUNG
               20  BNK-DAT-DEPOT-SCHLIES          PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  BNK-AEND-DAT-DEPOT-SCHLIES-KZ  PIC X(01).
      *            ÄNDERUNG-KZ. DATUM DEPOTSCHLIEßUNG
               20  BNK-RISIKOKLASSE               PIC X(01).
      *            RISIKOKLASSE
               20  BNK-AEND-RISIKOKLASSE-KZ       PIC X(01).
      *            ÄNDERUNG-KZ. RISIKOKLASSE
               20  BNK-ANLAGEZIEL                 PIC X(01).
      *            ANLAGEZIEL
               20  BNK-AEND-ANLAGEZIEL-KZ         PIC X(01).
      *            ÄNDERUNG-KZ. ANLAGEZIEL
               20  BNK-DEPOT-ART                  PIC X(01).
      *            V = VL-Depot
               20  BNK-AEND-DEPOT-ART-KZ          PIC X(01).
      *            ÄNDERUNG-KZ. ANLAGEZIEL
290713         20  BNK-IBAN                       PIC X(34).
>>    *            IBAN des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
               20  BNK-AEND-IBAN-KZ               PIC X(01).
      *            J WENN IBAN GEÄNDERT
               20  BNK-BIC                        PIC X(11).
      *            BIC des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
               20  BNK-AEND-BIC-KZ                PIC X(01).
      *            J WENN BIC GEÄNDERT
               20  BNK-MANDREF                    PIC X(35).
      *            Mandatsreferenz
               20  BNK-AEND-MANDREF-KZ            PIC X(01).
      *            J WENN MANDATSREFERENZ GEÄNDERT
               20  BNK-NACHNAME-BZ                PIC X(28).
      *            Kontoinhaber des Belastungskontos zur Lastschrift
      *            Depotpreiseinzug
    <<         20  BNK-AEND-NACHNAME-BZ-KZ        PIC X(01).
290713*            J WENN KONTOINHABER GEÄNDERT
061014         20  BNK-N-REAL-GW-VL-EUR-INKL      PIC S9(11)V99.
>>    *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
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
               20 BNK-REAL-GW-EURO-EROEFF         PIC S9(09)V99.
      *           Realisierte Gewinne/Verluste in Euro seit Eröffnung
<<    *           pro Unterdepot
061114         20  BNK-AEND-G-V-EURO-EROEFF-KZ    PIC X(01).
      *           J WENN GEÄNDERT
161115         20  BNK-SONDER-KONDITIONEN         PIC X(001).
      *           Sonderkonditionen für VBs
161115         20  BNK-AEND-SONDER-KONDITIONEN-KZ PIC X(001).
      *           J WENN GEÄNDERT
      *
100118         20  BNK-WPHGDATUM                  PIC X(008).
100118*           Datum letzte Aktualisierung WpHG
100118         20  BNK-AEND-WPHGDATUM-KZ          PIC X(001).
161115*        20  BNK-FILLER                     PIC X(098).
100118         20  BNK-FILLER                     PIC X(089).
      *            RESERVE
 



