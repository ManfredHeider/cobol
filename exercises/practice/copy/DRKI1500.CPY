      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1500.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1500.CPv  $
      *
      *   Rev 2.2   Jan 25 2018 07:02:54   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.1   Jul 13 2003 23:33:46   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:47:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für die Ausgabedatei Änderungsbestand         *
      *                  Kartendaten von der Deutschen Bank            *
      *                                                                *
      *    SATZLÄNGE 100                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!21.01.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO      *
      *        !        !                                  !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1500.
      *
           05  BNK-SA                            PIC  X(002).
      *            SATZART        = '11'
           05  BNK-GES                           PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER           PIC  9(003).
                   15 BNK-KUNDENNUMMER           PIC  9(007).
               10 BNK-UNTERKONTONUMMER           PIC  9(002).
               10 BNK-WAEHRUNG                   PIC  9(003).
      *           Währung ist immer Euro (000)
               10 BNK-KARTENART                  PIC  X(003).
      *           EMP - EC-Karte mit PIN
      *           EOP - EC-Karte ohne PIN
      *           BMP - Kundenkarte mit PIN
      *           BOP - Kundenkarte ohne PIN
      *           JMP - Motivkundenkarte mit PIN
      *           JOP - Motivkundenkarte ohne PIN
      *
           05  BNK-VERARB-KZ                     PIC  X(001).
      *        VERARBEITUNGSKENNZEICHEN
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
           05  BNK-ANZAHL-AKTIVE-KARTEN          PIC  X(003).
      *        Anzahl der aktiven Karten je Kartenart
      *        Addition aller Karten mit Kartenstatus = "AKT" je
      *        Kartenart, wenn Anzahl grösser Null, Satz übertragen.
      *
           05  BNK-AEND-ANZ-AKT-KARTEN-KZ        PIC  X(001).
      *            J = Änderung Anzahl aktive Karten
      *
           05  BNK-LFDNR                         PIC  9(004).

101217     05  BNK-PIN                       PIC  9(009).
101217     05  BNK-AEND-PIN-KZ               PIC  X(001).

101217*    05  BNK-RESERVE                   PIC  X(069).
101217     05  BNK-RESERVE                   PIC  X(059).
      *        Reserve

