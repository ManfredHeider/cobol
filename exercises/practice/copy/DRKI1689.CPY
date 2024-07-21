      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1689.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1689.CPv  $
      *
      *   Rev 2.4   Jan 25 2018 07:03:34   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.3   Feb 10 2016 08:32:06   t13ex73
      *AKI01417: Infosperre Deutsche Bank Kunden (DBO-1138)
      *
      *   Rev 2.2   Mar 16 2005 11:46:34   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:34:44   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:39:00   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Kartendaten von der          *
      *                Deutschen Bank HISTORIE                         *
      *                                                                *
      *    SATZLÄNGE 102                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00082!11.06.04! Neuerstellung                    !WHO        *
      *--------!--------!----------------------------------!-----------*
      *FKI00992!10.03.05! Tagesdatum der Löschung          !WHO        *
      *----------------------------------------------------------------*
010216*AKI01417!01.02.16! KSDS-DEL-KZ Feld einfuegen       ! DDY       *
      *----------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1689.
      *
           05  KSDS-SA                             PIC  X(002).
      *            SATZART        = '11'
           05  KSDS-GES                            PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 KSDS-FILIALNUMMER            PIC  9(003).
                   15 KSDS-KUNDENNUMMER            PIC  9(007).
               10 KSDS-UNTERKONTONUMMER            PIC  9(002).
               10 KSDS-WAEHRUNG                    PIC  9(003).
      *           Währung ist immer Euro (000)
               10 KSDS-KARTENART                   PIC  X(003).
      *           EMP - EC-Karte mit PIN
      *           EOP - EC-Karte ohne PIN
      *           BMP - Kundenkarte mit PIN
      *           BOP - Kundenkarte ohne PIN
      *           JMP - Motivkundenkarte mit PIN
      *           JOP - Motivkundenkarte ohne PIN
               10 KSDS-LFDNR                       PIC  9(002).
      *           LFD. NR. FÜR PRIMARY-KEY
      *
           05  KSDS-ANZAHL-AKTIVE-KARTEN           PIC  9(003).
      *        Anzahl der aktiven Karten je Kartenart
      *        Addition aller Karten mit Kartenstatus = "AKT" je
      *        Kartenart, wenn Anzahl grösser Null Satz übertragen.
      *
100305     05  KSDS-AEND-DAT                       PIC  X(10).
100305*         ÄNDERUNGSDATUM
010216     05  KSDS-DEL-KZ                         PIC  X(001).
101217     05  KSDS-PIN                            PIC 9(009).
101217*    05  KSDS-RESERVE                        PIC X(064).
101217     05  KSDS-RESERVE                        PIC X(055).
      *            RESERVE

