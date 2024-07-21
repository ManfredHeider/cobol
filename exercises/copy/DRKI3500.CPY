      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3500.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3500.CPv  $
      *
      *   Rev 2.2   Oct 31 2018 17:28:18   t13ae52
      *AKI01615: Erm./Aufb./Akt. KONTO (DSC)
      *
      *   Rev 2.1   Oct 30 2018 11:03:42   t13ae52
      *AKI01615: Erm./Aufb./Akt. KONTO (DSC)
      *
      *   Rev 2.0   Sep 20 2018 16:03:22   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    ÄNDERUNGSBESTAND G42 - GENERALI INVEST                      *
      *    KONTO
      *                                                                *
      *    SATZLÄNGE   200                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!21.09.18! NEUERSTELLUNG                    !SCHUMACHER *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI3500.

           05  'XX'-SATZ.
 
               10  'XX'-AEND-SA                        PIC  X(001).
      *            SATZART        = 'C'
 
               10  'XX'-AEND-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '42'
 
               10  'XX'-AEND-KEY.
                   20  'XX'-AEND-VNR                   PIC  9(015).
 
               20 'XX'-AEND-VERARB-KZ                  PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *            I = INSERT
      *            U = UPDATE
      *
 
               10  'XX'-AEND-DATUM-EROEFFNUNG.
                   15 'XX'-AEND-JAHR-EROEFFNUNG        PIC  9(004).
                   15 'XX'-AEND-MONAT-EROEFFNUNG       PIC  9(002).
                   15 'XX'-AEND-TAG-EROEFFNUNG         PIC  9(002).
               10  'XX'-AEND-DATUM-EROEFFNUNG-KZ       PIC  X(001).
      *            J = ÄNDERUNG DATUM-EROEFFNUNG
 
               10  'XX'-AEND-DATUM-SCHLIESSUNG.
                   15 'XX'-AEND-JAHR-SCHLIESSUNG       PIC  9(004).
                   15 'XX'-AEND-MONAT-SCHLIESSUNG      PIC  9(002).
                   15 'XX'-AEND-TAG-SCHLIESSUNG        PIC  9(002).
               10  'XX'-AEND-DATUM-SCHLIESSUNG-KZ      PIC  X(001).
      *            J = ÄNDERUNG DATUM-SCHLIESSUNG
 
               10  'XX'-AEND-EXTERNES-LIMIT            PIC  S9(012)V99.
               10  'XX'-AEND-EXTERNES-LIMIT-KZ         PIC  X(001).
      *            J = ÄNDERUNG EXTERNES-LIMIT
 
               10  'XX'-AEND-SALDO                     PIC  S9(012)V99.
               10  'XX'-AEND-SALDO-KZ                  PIC  X(001).
      *            J = ÄNDERUNG SALDO

               10  'XX'-AEND-KONTO-ART                 PIC  X(002).
      *             01 = KONTOFLEX;
      *             02 = TAGESGELD;
      *             03 = FESTGELD
               10  'XX'-AEND-KONTO-ART-KZ              PIC  X(001).
      *            J = ÄNDERUNG KONTO-ART
 
               10  'XX'-AEND-KUMHABENAM                PIC  S9(012)V99.
               10  'XX'-AEND-KUMHABENAM-KZ             PIC  X(001).
      *            J = ÄNDERUNG KUMHABENAM
 
               10  'XX'-AEND-KUMSOLLAM                 PIC  S9(012)V99.
               10  'XX'-AEND-KUMSOLLAM-KZ              PIC  X(001).
      *            J = ÄNDERUNG KUMSOLLAM
 
               10  'XX'-AEND-KUMHABENVM                PIC  S9(012)V99.
               10  'XX'-AEND-KUMHABENVM-KZ             PIC  X(001).
      *            J = ÄNDERUNG KUMHABENVM
 
               10  'XX'-AEND-KUMSOLLVM                 PIC  S9(012)V99.
               10  'XX'-AEND-KUMSOLLVM-KZ              PIC  X(001).
      *            J = ÄNDERUNG KUMSOLLVM
 
               10  'XX'-AEND-ZRAM                      PIC  X(005).
               10  'XX'-AEND-ZRAM-KZ                   PIC  X(001).
      *            J = ÄNDERUNG ZRAM
 
               10  'XX'-AEND-ZRVM                      PIC  X(005).
               10  'XX'-AEND-ZRVM-KZ                   PIC  X(001).
      *            J = ÄNDERUNG ZRVM
 
               10  'XX'-AEND-SOLLZINS                  PIC  S9(02)V9999.
               10  'XX'-AEND-SOLLZINS-KZ               PIC  X(001).
      *            J = ÄNDERUNG SOLLZINS
 
               10  'XX'-AEND-UEBERZZINS                PIC  S9(02)V9999.
               10  'XX'-AEND-UEBERZZINS-KZ             PIC  X(001).
      *            J = ÄNDERUNG UEBERZZINS
 
               10  'XX'-AEND-HABENZINS                 PIC  S9(02)V9999.
               10  'XX'-AEND-HABENZINS-KZ              PIC  X(001).
      *            J = ÄNDERUNG HABENZINS
 
               10  'XX'-AEND-BUCHUNGS-DAT.
                   15 'XX'-AEND-JAHR-BUCHUNG           PIC  9(004).
                   15 'XX'-AEND-MONAT-BUCHUNG          PIC  9(002).
                   15 'XX'-AEND-TAG-BUCHUNG            PIC  9(002).
               10  'XX'-AEND-BUCHUNGS-DAT-KZ           PIC  X(001).
      *            J = ÄNDERUNG BUCHUNGS-DAT
 
               10  'XX'-AEND-RESERVE                   PIC  X(028).
      *            RESERVE
 

