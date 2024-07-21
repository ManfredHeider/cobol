      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3304.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3304.CPv  $
      *
      *   Rev 2.3   Nov 24 2020 09:18:32   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.2   Mar 13 2018 10:00:58   t13ex78
      *AKI01565: Programme und Copies
      *
      *   Rev 2.1   Mar 09 2018 13:41:22   t13ex78
      *AKI01565: Programme und Copies
      *
      *   Rev 2.0   Feb 27 2018 16:40:56   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      * Aktualisierungsbestand für Tabelle GUTHABENPRODUKT und für   *
      * Programm PMKI1381                                            *
      *                                                              *
      * Satzlänge - 228 Bytes                                        *
      *                                                              *
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !AENDERUNG                       ! NAME     *
      * -------------------------------------------------------------*
      * AKI01565!27.02.18! Neu f. G79                     ! tfl      *
      *---------!--------!--------------------------------!----------*
      * AKI01805!09.10.20! NEUES ATTRIBUT ESAFE           ! ATAR     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
 
       01  'XX'-SATZ.
 
           05  'XX'-AKTUAL-KZ                    PIC  X(001).
           05  'XX'-GES                          PIC  9(002).
           05  'XX'-VNR                          PIC  X(015).
           05  'XX'-VBNR                         PIC  9(007).
           05  'XX'-LFDNR                        PIC  9(002).
           05  'XX'-PRODUKT-ID                   PIC  S9(04).
           05  'XX'-DATUM-EROEFFNUNG             PIC  X(010).
           05  'XX'-AEND-DATUM-E-KZ              PIC  X(001).
           05  'XX'-DATUM-SCHLIESSUNG            PIC  X(010).
           05  'XX'-AEND-DATUM-S-KZ              PIC  X(001).
           05  'XX'-VL-KZ                        PIC  X(001).
           05  'XX'-WKZ                          PIC  X(001).
           05  'XX'-AEND-WKZ-KZ                  PIC  X(001).
           05  'XX'-RISIKOKLASSE                 PIC  X(001).
           05  'XX'-AEND-RISIKO-KZ               PIC  X(001).
           05  'XX'-AEND-PRODUKT-ID-KZ           PIC  X(001).
           05  'XX'-ANLAGE-ZIEL                  PIC  X(001).
           05  'XX'-AEND-ANLAGE-ZIEL-KZ          PIC  X(001).
           05  'XX'-VERFUEG-KZ                   PIC  X(001).
           05  'XX'-AEND-VERFUEG-KZ-KZ           PIC  X(001).
           05  'XX'-TELREVERS                    PIC  X(001).
           05  'XX'-AEND-TELREVERS-KZ            PIC  X(001).
           05  'XX'-ONLREVERS                    PIC  X(001).
           05  'XX'-AEND-ONLREVERS-KZ            PIC  X(001).
           05  'XX'-EIGEN-KZ-FIX                 PIC  X(001).
           05  'XX'-DEPOT-ART                    PIC  X(001).
           05  'XX'-AEND-DEPOT-ART-KZ            PIC  X(001).
           05  'XX'-N-REAL-GW-VL-EUR-INKL        PIC S9(11)V99.
           05  'XX'-AEND-N-R-G-V-EURO-INKL-KZ    PIC  X(001).
           05  'XX'-N-REAL-GW-VL-VH-INKL         PIC S9(06)V99.
           05  'XX'-AEND-N-R-G-V-VH-INKL-KZ      PIC  X(001).
           05  'XX'-N-REAL-GW-VL-EUR-OHNE        PIC S9(11)V99.
           05  'XX'-AEND-N-R-G-V-EURO-OHNE-KZ    PIC  X(001).
           05  'XX'-N-REAL-GW-VL-VH-OHNE         PIC S9(06)V99.
           05  'XX'-AEND-N-R-G-V-VH-OHNE-KZ      PIC  X(001).
           05  'XX'-REAL-GW-EURO-EROEFF          PIC S9(09)V99.
           05  'XX'-AEND-G-V-EURO-EROEFF-KZ      PIC  X(001).
           05  'XX'-SONDER-KONDITIONEN           PIC  X(001).
           05  'XX'-AEND-SONDER-KONDITIONEN-KZ   PIC  X(001).
           05  'XX'-WPHGDATUM                    PIC  X(010).
           05  'XX'-AEND-WPHGDATUM-KZ            PIC  X(001).
           05  'XX'-AEND-EMAIL                   PIC  X(040).
           05  'XX'-AEND-EMAIL-KZ                PIC  X(001).
           05  'XX'-ESAFE                        PIC  X(001).
           05  'XX'-AEND-ESAFE-KZ                PIC  X(001).
           05  'XX'-FILLER                       PIC  X(044).
 
 
 


