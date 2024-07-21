      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3287.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3287.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:31:14   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 22 2018 13:31:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01570!21.02.18!2.1 ! neu                       ! IFR
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
           05  'P'-KEY.
               10  'P'-GES                     PIC 9(02).
               10  'P'-VNR                     PIC 9(15).
               10  'P'-EZPNR                   PIC 9(04).
               10  'P'-ROLLE                   PIC X(02).
               10  'P'-LFDNR                   PIC 9(04).

      *
           05  'P'-PERSONENDATEN.
               10  'P'-NNAME                   PIC X(40).
               10  'P'-AEND-NNAME-KZ           PIC X(01).
               10  'P'-NNAME2                  PIC X(40).
               10  'P'-AEND-NNAME2-KZ          PIC X(01).
               10  'P'-VNAME                   PIC X(30).
               10  'P'-AEND-VNAME-KZ           PIC X(01).
               10  'P'-GEBDAT                  PIC 9(08).
               10  'P'-AEND-GEBDAT-KZ          PIC X(01).
               10  'P'-ANREDE                  PIC X(02).
                  88 'P'-ANREDE-HERR              VALUE '01'.
                  88 'P'-ANREDE-FRAU              VALUE '02'.
                  88 'P'-ANREDE-FRL               VALUE '03'.
                  88 'P'-ANREDE-EHELEUTE          VALUE '04'.
                  88 'P'-ANREDE-FIRMA             VALUE '05'.
                  88 'P'-ANREDE-HERREN            VALUE '06'.
                  88 'P'-ANREDE-FRAUEN            VALUE '07'.
                  88 'P'-ANREDE-REST              VALUE '08'.

               10  'P'-AEND-ANREDE-KZ          PIC X(01).
               10  'P'-GESCHL                  PIC X(01).
                  88 'P'-GESCHLECHT-W             VALUE '2'.
                  88 'P'-GESCHLECHT-M             VALUE '1'.
               10  'P'-AEND-GESCHL-KZ          PIC X(01).
               10  'P'-NNAME-ALT               PIC X(40).
               10  'P'-NNAME-2-ALT             PIC X(40).
               10  'P'-VNAME-ALT               PIC X(30).
               10  'P'-STR-1                   PIC X(45).
               10  'P'-AEND-STR-1-KZ           PIC X(01).
               10  'P'-PLZ                     PIC X(15).
               10  'P'-AEND-PLZ-KZ             PIC X(01).
               10  'P'-ORT                     PIC X(30).
               10  'P'-AEND-ORT-KZ             PIC X(01).
               10  'P'-AUSL-KZ                 PIC X(03).
               10  'P'-AEND-AUSL-KZ            PIC X(01).
      *
           05  'P'-AKTUAL-KZ                  PIC X(01).

