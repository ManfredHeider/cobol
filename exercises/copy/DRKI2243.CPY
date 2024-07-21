      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2243.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2243.CPv  $
      *
      *   Rev 2.1   29 Apr 2008 10:58:50   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten PERFORMANCE
      *
      *   Rev 2.0   14 Apr 2008 08:27:56   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * -------------------------------------------------------------*
      * AKI00708!14.02.08!    ! Neu nur für DeuBA         !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2243.
      *
               20  BNK-SA                  PIC  X(01).
      *            SATZART        = '8'
               20  BNK-GES                 PIC  9(02).
      *            GESELLSCHAFT
               20  BNK-VNR                 PIC  9(15).

      *        20  BNK-GHP-LFDNR           PIC  9(02).
               20  BNK-GHP-LFDNR           PIC  9(04).

      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  BNK-BRUTTO-PER-LDJ      PIC S9(13)V9(05).
      *            Bruttoperformance in % im lfd. Jahr
               20  BNK-BRUTTO-PER-GES      PIC S9(13)V9(05).
      *            Bruttoperformance in % gesamt
               20  BNK-BEG-BTR-PERFORM-LFD PIC X(08).
      *            Beginn Bruttoperformance in % im lfd. Jahr
               20  BNK-END-BTR-PERFORM-LFD PIC X(08).
      *            Ende Bruttoperformance in % im lfd. Jahr
               20  BNK-BEG-BTR-PERFORM-GES PIC X(08).
      *            Beginn Bruttoperformance in % gesamt
               20  BNK-END-BTR-PERFORM-GES PIC X(08).
      *            Ende Bruttoperformance in % gesamt
               20  BNK-PERFDAT            PIC  9(08).
               20  BNK-VTG-ID              PIC  X(13).
      *            Vertrags-ID für Performance-Optimierung in Akt.-PGM
               20  FILLER                 PIC  X(50).
      *            RESERVE

