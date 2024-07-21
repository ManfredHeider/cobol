      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1722.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1722.CPv  $
      *
      *   Rev 2.6   Jan 17 2018 09:37:10   t13ex78
      *AKI01562: Programme und Copies
      *
      *   Rev 2.5   Jan 28 2016 10:43:10   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.4   Sep 10 2013 09:22:44   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Depotstammsatz
      *
      *   Rev 2.3   Sep 09 2013 12:10:42   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Depotstammsatz
      *
      *   Rev 2.2   Sep 02 2013 12:18:06   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Depotstammsatz
      *
      *   Rev 2.1   Oct 13 2004 14:09:10   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.0   Sep 15 2004 15:33:40   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER für KSDS DEPOTSTAMM DEUTSCHE BANK               *
      *    ----------------------------------------------------------  *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   082                                             *
290713*    SATZLÄNGE   202                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00249!15.09.04!01.0! NEU                       !WHO       *
      *--------------------------------------------------------------*
290713* AKI01204!17.07.13!    !NEUE FELDER EINGEFÜGT f. SEPA!WMI     *
290713*         !        !    !  UND DEN FILLER ANGEPASST   !        *
      *--------------------------------------------------------------*
161115* AKI01396!16.11.15! neues Attrib. SONDERKONDITIONEN! WHO      *
      *--------------------------------------------------------------*
100118* AKI01562!10.01.18! neues Attribut WPHGDATUM       ! tfl      *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1722.
      *
               20  KSDS-SA                  PIC X(001).
      *            SATZART        = '6'
               20  KSDS-GES                 PIC 9(002).
      *            GESELLSCHAFT   = '48'
 
               20  KSDS-VNR.
                   30 KSDS-FILIALNUMMER          PIC  9(003).
                   30 KSDS-KUNDENNUMMER          PIC  9(007).
                   30 KSDS-UNTERKONTONUMMER      PIC  9(002).
                   30 KSDS-WAEHRUNG              PIC  9(003).
                   30 KSDS-LFDNR                 PIC  9(002).
               20  FILELR REDEFINES KSDS-VNR.
                   30 KSDS-VNR-1                 PIC  9(010).
                   30 FILLER                     PIC  X(007).
 
               20  KSDS-DAT-DEPOT-EROEFF    PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  KSDS-DAT-DEPOT-SCHLIES   PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  KSDS-RISIKOKLASSE        PIC X(01).
      *            RISIKOKLASSE
               20  KSDS-ANLAGEZIEL          PIC X(01).
      *            ANLAGEZIEL
               20  KSDS-DEPOT-ART           PIC X(01).
      *            V = VL-DEPOT
               20  KSDS-DEL-KZ              PIC X(01).
      *            LÖSCHKENNZEICHEN
               20  KSDS-AEND-DAT            PIC X(10).
      *            ERSTELLDATUM
290713         20  KSDS-IBAN                PIC X(34).
>>    *            IBAN des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
               20  KSDS-BIC                 PIC X(11).
      *            BIC des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
               20  KSDS-MANDREF             PIC X(35).
      *            Mandatsreferenz
               20  KSDS-NACHNAME-BZ         PIC X(28).
      *            Kontoinhaber des Belastungskontos zur Lastschrift
<<    *            Depotpreiseinzugg
161115         20  KSDS-N-REAL-GW-VL-EUR-INKL   PIC S9(11)V99.
>>    *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
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
               20  KSDs-REAL-GW-EURO-EROEFF     PIC S9(09)V99.
      *           Realisierte Gewinne/Verluste in Euro seit Eröffnung
      *           pro Unterdepot
 
               20  KSDS-SONDER-KONDITIONEN  PIC X(001).
100118         20  KSDS-WPHGDATUM           PIC 9(008).
161115*        20  FILLER                   PIC X(50).
100118         20  FILLER                   PIC X(42).
      *            RESERVE
 

