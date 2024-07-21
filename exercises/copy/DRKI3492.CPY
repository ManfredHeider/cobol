      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3492.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI3492.CPv  $
      *
      *   Rev 2.9   May 10 2019 12:51:34   t13ae52
      *AKI01680: G42: Erweiterung Datenspiegel
      *
      *   Rev 2.8   Nov 22 2018 11:02:14   t13ae52
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.7   Nov 22 2018 10:47:56   t13ae52
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.6   Nov 05 2018 16:21:46   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.5   Nov 05 2018 16:07:44   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.4   Nov 05 2018 15:50:20   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTBESTAND
      *
      *   Rev 2.3   Oct 31 2018 17:03:04   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.2   Oct 29 2018 11:55:00   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.1   Oct 25 2018 14:47:16   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.0   Sep 13 2018 14:19:32   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *                                                               *
      *    Copy Member für Ausgabedatei DEPOTSTAMM G42 Generali Inv.  *
      *    --------------------------------------------------------   *
      *                                                               *
      *    SATZLÄNGE                                                  *
      *                                                               *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01615!17.09.18! 2.1! Neuerstellung             ! tfl       *
      *---------------------------------------------------------------*
      * AKI01680!15.04.19! NEUES FELD: DAT-WPHG-AKTU      !SCHUMACHER *
      *         !        ! ANPASSUNG FILLER               !           *
      *---------------------------------------------------------------*
      *         !        !                                !           *
      *         !        !                                !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
 
       01  'XX'-SATZ.
      *
           05 'XX'-VERARB-KZ                  PIC X(01).
      * Verarbeitungskennzeichen
      * I = Insert
      * U = Update
      * D = Delete
           05 'XX'-SA                         PIC X(01).
      * Satzart        = 'C'
           05 'XX'-GES                        PIC 9(02).
      * Gesellschaft   = '42'
           05 'XX'-VNR                        PIC 9(15).
      * Vertragsnummer mit führenden Nullen (Depotnummer)
           05 'XX'-DAT-DEPOT-EROEFF           PIC X(08).
           05 'XX'-DAT-DEPOT-EROEFF-KZ        PIC X(01).
      * Datum Depoteröffnung
           05 'XX'-DEPOT-STATUS               PIC X(08).
           05 'XX'-DEPOT-STATUS-KZ            PIC X(01).
      * Depotstatus
           05 'XX'-RISIKOKLASSE               PIC X(01).
           05 'XX'-RISIKOKLASSE-KZ            PIC X(01).
      * Risikoklasse
           05 'XX'-ANLAGEZIEL                 PIC X(01).
           05 'XX'-ANLAGEZIEL-KZ              PIC X(01).
      * Anlageziel
           05 'XX'-DEPOT-ART                  PIC X(01).
           05 'XX'-DEPOT-ART-KZ               PIC X(01).
      * V = VL-Depot
           05 'XX'-IBAN                       PIC X(34).
           05 'XX'-IBAN-KZ                    PIC X(01).
      * IBAN des Belastungskontos zur Lastschrift für
      * Depotpreiseinzug
           05 'XX'-BIC                        PIC X(11).
           05 'XX'-BIC-KZ                     PIC X(01).
      * BIC des Belastungskontos zur Lastschrift für
      * Depotpreiseinzug
           05 'XX'-MANDREF                    PIC X(35).
           05 'XX'-MANDREF-KZ                 PIC X(01).
      * Mandatsreferenz
           05 'XX'-NACHNAME-BZ                PIC X(28).
           05 'XX'-NACHNAME-BZ-KZ             PIC X(01).
      * Kontoinhaber des Belastungskontos zur Lastschrift
      * Depotpreiseinzug
           05 'XX'-N-REAL-GW-VL-EUR-INKL      PIC S9(11)V99.
           05 'XX'-N-R-G-V-EURO-INKL-KZ       PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Euro  inkl.
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05 'XX'-N-REAL-GW-VL-VH-INKL       PIC S9(06)V99.
           05 'XX'-N-R-G-V-VH-INKL-KZ         PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05 'XX'-N-REAL-GW-VL-EUR-OHNE      PIC S9(11)V99.
           05 'XX'-N-R-G-V-EURO-OHNE-KZ       PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Euro ohne .
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05 'XX'-N-REAL-GW-VL-VH-OHNE       PIC S9(06)V99.
           05 'XX'-N-R-G-V-VH-OHNE-KZ         PIC X(01).
      * nicht realisierter Gewinn/ Verlust in Prozent ohne
      * Barausschüttungen  aller Bestände in diesem Unterdepot
           05 'XX'-REAL-GW-EURO-EROEFF        PIC S9(09)V99.
           05 'XX'-G-V-EURO-EROEFF-KZ         PIC X(01).
      * Realisierte Gewinne/Verluste in Euro seit Eröffnung
      * pro Unterdepot
           05 'XX'-ONLINEBANKING              PIC X(01).
           05 'XX'-ONLINEBANKING-KZ           PIC X(01).
      * Realisierte Gewinne/Verluste in Euro seit Eröffnung
      * pro Unterdepot
           05 'XX'-DAT-WPHG-AKTU              PIC 9(08).
           05 'XX'-DAT-WPHG-AKTU-KZ           PIC X(01).
      *    DATUM LETZTE AKTUALISIERUNG WPHG: JJJJMMTT
 
           05 'XX'-FILLER                     PIC X(79).
      * Reserve
 
 
 
 
 
 
 


