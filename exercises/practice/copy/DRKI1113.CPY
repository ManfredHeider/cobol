      *
      *----------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1113.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1113.CPv  $
      *
      *   Rev 2.3   Jan 25 2018 07:02:38   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.2   Feb 10 2016 08:32:00   t13ex73
      *AKI01417: Infosperre Deutsche Bank Kunden (DBO-1138)
      *
      *   Rev 2.1   Jul 13 2003 23:37:36   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   07 Jun 2000 17:08:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Kartendaten von der          *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 100                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!21.01.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
010216*AKI01417!01.02.16! KSDS-DEL-KZ Feld einfuegen       ! DDY       *
      *----------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1113.
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
      *
           05  KSDS-ANZAHL-AKTIVE-KARTEN           PIC  9(003).
      *        Anzahl der aktiven Karten je Kartenart
      *        Addition aller Karten mit Kartenstatus = "AKT" je
      *        Kartenart, wenn Anzahl grösser Null Satz übertragen.
      *
010216     05  KSDS-DEL-KZ                         PIC  X(001).
101217     05  KSDS-PIN                            PIC 9(009).
101217*    05  KSDS-RESERVE                        PIC X(074).
101217     05  KSDS-RESERVE                        PIC X(065).
      *            RESERVE


