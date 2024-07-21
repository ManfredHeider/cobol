      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1715.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1715.CPv  $
      *
      *   Rev 2.7   Jan 17 2018 09:36:44   t13ex78
      *AKI01562: Programme und Copies
      *
      *   Rev 2.6   Mar 09 2016 14:45:04   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.5   Jan 07 2016 14:16:50   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.3   Nov 27 2014 10:14:36   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.2   Sep 02 2013 12:17:34   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Depotstammsatz
      *
      *   Rev 2.1   Oct 13 2004 14:08:44   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.0   Sep 15 2004 15:29:40   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR ÄNDERUNGSBESTAND DEUTSCHE BANK DEPOTSTAMM   *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 060 bytes                                           *
290713*    Länge = 200 bytes                                           *
100118*    Länge = 250 bytes (schon länger)                            *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00249!10.09.04! 01 ! Neu                       !Hornung   *
      *----------------------------------------------------------------*
290713*AKI01204 !29.07.13! NEUE FELDER EINGEFÜGT f. SEPA  !WMI         *
290713*         !        !   UND DEN FILLER ANGEPASST     !            *
      *--------------------------------------------------------------*
061014* AKI01327!06.10.14! Erweiterungen für              ! WHO      *
      *         !        ! Wertentwicklung                !          *
      *--------------------------------------------------------------*
161115* AKI01396!16.11.15! neues Attrib. SONDERKONDITIONEN! WHO      *
      *--------------------------------------------------------------*
      * AAT00049!03.03.16! Umbau auf Replacing by XX      !DDY       *
      *         !        ! - ohne Vermerk in AREA A -     !          *
      *--------------------------------------------------------------*
100118* AKI01562!10.01.18! neues Attribut WPHGDATUM       ! tfl      *
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1715.
      *
      *
           10  'XX'-SATZ.
      *
               20  'XX'-SA                PIC  X(001).
      *            SATZART        = '5'
               20  'XX'-GES               PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  'XX'-VNR.
                   25  'XX'-FILIAL-NR     PIC 9(03).
                   25  'XX'-KDNR          PIC 9(07).
                   25  'XX'-UNTER-DEP-NR  PIC 9(02).
                   25  'XX'-WKZ           PIC 9(03).
               20  FILLER   REDEFINES 'XX'-VNR.
                   25  'XX'-VNR-1         PIC 9(10).
                   25  FILLER             PIC X(05).
               20  'XX'-DAT-DEPOT-EROEFF  PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  'XX'-DAT-DEPOT-SCHLIES PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  'XX'-RISIKOKLASSE      PIC X(01).
      *            RISIKOKLASSE
               20  'XX'-ANLAGEZIEL        PIC X(01).
      *            ANLAGEZIEL
               20  'XX'-DEPOT-ART         PIC X(01).
      *            V = VL-Depot
               20  'XX'-DEL-KZ            PIC X(01).
      *            LÖSCH-KENNZEICHEN
 
290713         20  'XX'-IBAN              PIC X(034).
>>    *            IBAN des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
 
               20  'XX'-BIC               PIC X(011).
      *            BIC des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
 
               20  'XX'-MANDREF           PIC X(035).
      *            Mandatsreferenz
 
    <<         20  'XX'-NACHNAME-BZ       PIC X(028).
290713*            Kontoinhaber des Belastungskontos zur Lastschrift
      *            Depotpreiseinzug
 
061014         20  'XX'-N-REAL-GW-VL-EUR-INKL PIC S9(11)V99.
>>    *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-VH-INKL  PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-EUR-OHNE PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-VH-OHNE  PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20 'XX'-REAL-GW-EURO-EROEFF    PIC S9(09)V99.
      *           Realisierte Gewinne/Verluste in Euro seit Eröffnung
      *           pro Unterdepot
<<    *
161115         20  'XX'-SONDER-KONDITIONEN PIC X(001).
      *
100118         20  'XX'-WPHGDATUM          PIC 9(008).
100118*           Datum letzte Aktualisierung WpHG-Bogen
 
               20  FILLER                 PIC X(042).
      *
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-V-SA          PIC X(001).
      *            SATZART = '1'
               20  'XX'-V-ABSENDER    PIC X(008).
      *            ABSENDER = 'DB AG'
               20  'XX'-V-DATUM       PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER  PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT        PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTSTAMM       TÄGLICH   ‘STAMMT  '
      *            DEPOTSTAMMN      KOMPLETT  ‘STAMMK  '
               20  'XX'-V-WKZ         PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER             PIC X(218).
 
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-N-SA          PIC X(001).
      *            SATZART             '9'
               20  'XX'-N-ANZ         PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER             PIC X(242).
 

