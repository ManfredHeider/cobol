      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2332.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2332.CPv  $
      *
      *   Rev 2.5   Oct 30 2009 09:36:58   t13ex50
      *AKI00844: Copy für Stammdaten (KSDS)
      *
      *   Rev 2.4   Oct 30 2009 09:16:20   t13ex50
      *AKI00844: Copy für Stammdaten (KSDS)
      *
      *   Rev 2.3   Oct 27 2009 12:37:56   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.2   Oct 27 2009 12:04:40   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.1   Aug 19 2009 09:22:02   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 07 2009 07:30:30   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER für KSDS DEPOTSTAMM DWS                         *
      *    ----------------------------------------------------------  *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00844!01.08.09! 01 ! Neu                       !Hornung   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2332.
      *
               20  KSDS-SA                  PIC X(002).
      *            SATZART        = '17'
               20  KSDS-GES                 PIC 9(002).
      *            GESELLSCHAFT   = '48'

               20  KSDS-VNR.
                   30 KSDS-FILIALNUMMER          PIC  9(003).
                   30 KSDS-KUNDENNUMMER          PIC  9(007).
                   30 KSDS-ORDNUNGSNR            PIC  9(002).
               20  FILELR REDEFINES KSDS-VNR.
                   30 KSDS-VNR-1                 PIC  9(010).
                   30 FILLER                     PIC  X(002).
               20  KSDS-PORTFOLIO-NR        PIC X(10).

               20  KSDS-DAT-DEPOT-EROEFF    PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  KSDS-DAT-DEPOT-SCHLIES   PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  KSDS-DEL-KZ              PIC X(01).
      *            LÖSCHKENNZEICHEN
               20  FILLER                   PIC X(57).
      *            RESERVE

