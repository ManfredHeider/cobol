      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2137.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2137.CPv  $
      *
      *   Rev 2.3   13 Nov 2007 11:17:54   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.2   30 Oct 2007 12:20:56   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   Sep 28 2007 14:17:30   t13ex52
      *AKL00664: Allfinanz: Bestandsdaten AM-Vers
      *
      *   Rev 2.0   Sep 28 2007 14:15:08   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle  VERTRAG              *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *10.10.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 160 BYTES                   STAND: 10.19.2007      *
      **************************************************************** *

       01  DRKI2137.

                    05  REOB-AUS-VTG-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
           05  VS-VERTRAGS-ID             PIC  X(013).
           05  VS-GES                     PIC  9(002).
           05  VS-VNR                     PIC  9(015).

           05  VS-INT-VNR                 PIC  9(009).
           05  VS-INT-VNR-AENDKZ          PIC  X(001).
      *        'J' wenn interne Vertragsnummer geändert

           05  VS-INT-VNR-PRUEF           PIC  9(002).
           05  VS-INT-VNR-PRUEF-AENDKZ    PIC  X(001).
      *        'J' wenn Prüfziffer für interne Vertragsnummer geändert

           05  VS-VBNR                    PIC  9(007).
           05  VS-VBNR-AENDKZ             PIC  X(001).
      *        'J' wenn Abschluss-Vermittler (Vermögensberater-Nr.)
      *            geändert

           05  VS-ZAHLWEISE               PIC S9(002).
           05  VS-ZAHLWEISE-AENDKZ        PIC  X(001).
      *        'J' wenn Zahlweise geändert

           05  VS-VERTRAGSZUSTAND         PIC S9(002).
           05  VS-VERTRAGSZUSTAND-AENDKZ  PIC  X(001).
      *        'J' wenn Vertragszustand geändert

          05  VS-AENDDATUM               PIC  X(010).
          05  VS-AENDDATUM-AENDKZ        PIC  X(001).
     **       'J' wenn Vertragszustand geändert

          05  VS-BEITRAG-GESAMT          PIC  9(011)V99.
          05  VS-BEITRAG-GESAMT-AENDKZ   PIC  X(001).
     **       'J' wenn Beitrag Gesamt geändert

          05  VS-WAEHRUNG-KZ             PIC  X(001).
          05  VS-WAEHRUNG-KZ-AENDKZ      PIC  X(001).
*     *       'J' wenn Währungskennzeichen geändert

          05  VS-VERTRAGSBEGINN          PIC  X(010).
          05  VS-VERTRAGSBEGINN-AENDKZ   PIC  X(001).
     **       'J' wenn Vertragsbeginn geändert

          05  VS-VERTRAGSABLAUF          PIC  X(010).
          05  VS-VERTRAGSABLAUF-AENDKZ   PIC  X(001).
     **       'J' wenn Vertragsablauf geändert

          05  VS-ZAHLUNGSART             PIC S9(004) BINARY.
          05  VS-ZAHLUNGSART-AENDKZ      PIC  X(001).
     **       'J' wenn Zahlungsart geändert

          05  VS-STORNO-DATUM            PIC  X(010).
          05  VS-STORNO-DATUM-AENDKZ     PIC  X(001).
      *        'J' wenn Stornodatum geändert

           05  VS-GEB-VERTR               PIC  X(001).
           05  VS-GEB-VERTR-AENDKZ        PIC  X(001).
      *        'J' wenn Gebündelt-Kz geändert

           05  FILLER                     PIC X(37).
