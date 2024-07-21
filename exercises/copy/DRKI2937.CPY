      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.10.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2937.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2937.CPv  $
      *
      *   Rev 2.7   Nov 24 2020 09:17:30   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.6   Feb 24 2020 12:04:42   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.5   Mar 13 2018 09:57:22   t13ex78
      *AKI01565: Programme und Copies
      *
      *   Rev 2.4   Mar 09 2018 13:30:44   t13ex78
      *AKI01565: Programme und Copies
      *
      *   Rev 2.3   Jan 17 2018 09:37:16   t13ex78
      *AKI01562: Programme und Copies
      *
      *   Rev 2.2   Jan 07 2016 14:17:08   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.1   Nov 27 2014 10:15:06   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa
      *
      *   Rev 2.0   Oct 07 2014 10:20:18   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !AENDERUNG                       ! NAME     *
      * -------------------------------------------------------------*
      * AKI01327!06.10.14! NEU                            ! WHO      *
      *--------------------------------------------------------------*
061014* AKI01327!06.10.14! Erweiterungen für              ! WHO      *
      *         !        ! Wertentwicklung                !          *
      *--------------------------------------------------------------*
161115* AKI01396!16.11.15! neues Attrib. SONDERKONDITIONEN! WHO      *
      *--------------------------------------------------------------*
100118* AKI01562!10.01.18! neues Attribut WPHGDATUM       ! tfl      *
      *--------------------------------------------------------------*
260218* AKI01565!26.02.18! neues Attribut EMAIL - erst mal! tfl      *
      *         !        ! nur für G79 Geiger Edelmetalle !          *
      *---------!--------!--------------------------------!----------*
      * AKI01762!19.01.20! NEUES ATTRIBUT "GHPBLZKUNDEN"  ! T13EX39
      *         !        ! UND BPKBLZKUNDEN AUS KUNDENBLZ !
      *         !        ! VERARBEITEN                    !
      *---------!-----------------------------------------!----------*
      *AKI01805 !14.10.20! NEUES ATTRIBUT ESAFE           ! ATAR     *
      *                                                              *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
       01  DRKI2937.
 
           05  GHP-AKTUAL-KZ               PIC  X(01).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
           05  GHP-GES                     PIC  9(02).
           05  GHP-VNR                     PIC  9(15).
           05  GHP-LFDNR                   PIC  9(02).
      *        NUR Unterkontonummer
           05  GHP-PRODUKT-ID              PIC  S9(04).
 
           05  GHP-DATUM-EROEFFNUNG        PIC  X(10).
           05  GHP-AEND-DATUM-E-KZ         PIC  X(01).
      *        J WENN ERÖFFNUNGSDATUM GEAENDERT
 
           05  GHP-DATUM-SCHLIESSUNG       PIC  X(10).
           05  GHP-AEND-DATUM-S-KZ         PIC  X(01).
      *        J WENN SCHLIESSUNGSDATUM GEAENDERT
 
           05  GHP-VL-KZ                   PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG
 
           05  GHP-WKZ                     PIC  X(01).
           05  GHP-AEND-WKZ-KZ             PIC  X(01).
      *        J WENN WÄHRUNGSKENNZEICHEN GEAENDERT
 
           05  GHP-RISIKOKLASSE            PIC  X(01).
      *        Risikoklasse 1 - 6
           05  GHP-AEND-RISIKO-KZ          PIC  X(01).
      *        J WENN Risikoklasse GEAENDERT
 
           05  GHP-AEND-PRODUKT-ID-KZ      PIC  X(01).
      *        J wenn Produkt-ID GEAENDERT
 
           05  GHP-ANLAGE-ZIEL             PIC  X(01).
      *        ANLAGEZIEL
           05  GHP-AEND-ANLAGE-ZIEL-KZ     PIC  X(01).
      *        J Wenn geändert
 
           05  GHP-VERFUEG-KZ              PIC  X(001).
           05  GHP-AEND-VERFUEG-KZ-KZ      PIC  X(001).
           05  GHP-TELREVERS               PIC  X(001).
           05  GHP-AEND-TELREVERS-KZ       PIC  X(001).
           05  GHP-ONLREVERS               PIC  X(001).
           05  GHP-AEND-ONLREVERS-KZ       PIC  X(001).
 
           05  GHP-EIGEN-KZ-FIX            PIC  X(001).
 
           05  GHP-DEPOT-ART               PIC  X(01).
           05  GHP-AEND-DEPOT-ART-KZ       PIC  X(01).
061014     05  GHP-N-REAL-GW-VL-EUR-INKL      PIC S9(11)V99.
>>    *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  GHP-AEND-N-R-G-V-EURO-INKL-KZ  PIC X(01).
      *            J WENN BIC GEÄNDERT
           05  GHP-N-REAL-GW-VL-VH-INKL       PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  GHP-AEND-N-R-G-V-VH-INKL-KZ    PIC X(01).
      *            J WENN BIC GEÄNDERT
           05  GHP-N-REAL-GW-VL-EUR-OHNE      PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  GHP-AEND-N-R-G-V-EURO-OHNE-KZ  PIC X(01).
      *            J WENN BIC GEÄNDERT
           05  GHP-N-REAL-GW-VL-VH-OHNE       PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  GHP-AEND-N-R-G-V-VH-OHNE-KZ    PIC X(01).
      *            J WENN BIC GEÄNDERT
           05  GHP-REAL-GW-EURO-EROEFF        PIC S9(09)V99.
      *           Realisierte Gewinne/Verluste in Euro seit Eröffnung
<<    *           pro Unterdepot
061014     05  GHP-AEND-G-V-EURO-EROEFF-KZ    PIC X(01).
      *            J WENN BIC GEÄNDERT
161115     05  GHP-SONDER-KONDITIONEN         PIC X(001).
      *           Sonderkonditionen für VBs
161115     05  GHP-AEND-SONDER-KONDITIONEN-KZ PIC X(001).
      *            J WENN BIC GEÄNDERT
100118     05  GHP-WPHGDATUM                  PIC X(010).
100118*            Datum letzte Aktualisierung WpHG
100118     05  GHP-AEND-WPHGDATUM-KZ          PIC X(001).
260218*            Emailadresse des Kunden - nur für Geiger Edelmetalle
260218     05  GHP-AEND-EMAIL                 PIC X(040).
260218     05  GHP-AEND-EMAIL-KZ              PIC X(001).
 
           05  GHP-BLZ-KUNDE                  PIC X(010).
           05  GHP-AEND-BLZ-KUNDE-KZ          PIC X(001).

           05  GHP-ESAFE                      PIC  X(001).
           05  GHP-AEND-ESAFE-KZ              PIC  X(001).

100118     05  GHP-FILLER                     PIC X(033).
      *        Reserve
 
 
 



