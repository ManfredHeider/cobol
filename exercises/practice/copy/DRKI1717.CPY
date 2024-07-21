      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1717.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1717.CPv  $
      *
      *   Rev 2.5   Jan 17 2018 09:37:02   t13ex78
      *AKI01562: Programme und Copies
      *
      *   Rev 2.4   Jan 07 2016 14:17:02   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.3   Nov 27 2014 10:14:46   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.2   Sep 02 2013 12:17:48   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Depotstammsatz
      *
      *   Rev 2.1   Oct 13 2004 14:09:02   t13ex39
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
      *    SATZLÄNGE   070                                             *
290713*    SATZLÄNGE   200                                             *
100118*    SATZLÄNGE   250                                             *
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
061014* AKI01327!06.10.14! Erweiterungen für              ! WHO      *
      *         !        ! Wertentwicklung                !          *
      *--------------------------------------------------------------*
161115* AKI01396!16.11.15! neues Attrib. SONDERKONDITIONEN! WHO      *
      *--------------------------------------------------------------*
100118* AKI01562!10.01.18! neues Attribut WPHGDATUM       ! tfl      *
      *         !        ! neue Satzlänge 250 Bytes       !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1717.
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
               20  FILELR REDEFINES KSDS-VNR.
                   30 KSDS-VNR-1                 PIC  9(010).
                   30 FILLER                     PIC  X(005).
 
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
290713         20  KSDS-IBAN                PIC X(34).
>>    *            IBAN des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
               20  KSDS-BIC                 PIC X(11).
      *            BIC des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
               20  KSDS-MANDREF             PIC X(35).
      *            Mandatsreferenz
               20  KSDS-NACHNAME-BZ         PIC X(28).
    <<*            Kontoinhaber des Belastungskontos zur Lastschrift
290713*            Depotpreiseinzugg
 
061014         20  KSDS-N-REAL-GW-VL-EUR-INKL   PIC S9(11)V99.
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
<<    *           Realisierte Gewinne/Verluste in Euro seit Eröffnung
061014*           pro Unterdepot
 
161115         20  KSDS-SONDER-KONDITIONEN      PIC X(001).
100118         20  KSDS-WPHGDATUM               PIC 9(008).
100118         20  FILLER                       PIC X(042).
 

