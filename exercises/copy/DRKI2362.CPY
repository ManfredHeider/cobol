      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.09.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2362.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2362.CPv  $
      *
      *   Rev 2.2   Mar 18 2014 07:05:10   t13ex77
      *AKI01288: Wartung / Fehlerkorrekturen DeuBa / DWS
      *
      *   Rev 2.1   Oct 28 2009 11:01:32   t13ex39
      *AKI00844: DWS-Datenspiegel löschen KSDS-Dateien
      *
      *   Rev 2.0   Sep 25 2009 08:29:10   t13ex39
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
      * -------------------------------------------------------------*
260414* AKI01288!26.04.14! 02 ! Neu: Unter-Key KSDS-VNR-2 ! abn      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2362.
      *
               20  KSDS-SA                  PIC X(002).
      *            SATZART        = '17'
               20  KSDS-GES                 PIC 9(002).
      *            GESELLSCHAFT   = '48'

               20  KSDS-VNR.
                   30 KSDS-FILIALNUMMER          PIC  9(003).
                   30 KSDS-KUNDENNUMMER          PIC  9(007).
                   30 KSDS-ORDNUNGSNR            PIC  9(002).
                   30 KSDS-LFDNR                 PIC  9(002).
               20  FILELR REDEFINES KSDS-VNR.
                   30 KSDS-VNR-1                 PIC  9(010).
                   30 FILLER                     PIC  X(004).
260414         20  FILLER REDEFINES KSDS-VNR.
260414             30 KSDS-VNR-2                 PIC  9(012).
260414             30 FILLER                     PIC  X(002).
               20  KSDS-PORTFOLIO-NR        PIC X(10).

               20  KSDS-DAT-DEPOT-EROEFF    PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  KSDS-DAT-DEPOT-SCHLIES   PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  KSDS-DEL-KZ              PIC X(01).
      *            LÖSCHKENNZEICHEN
               20  KSDS-AEND-DAT            PIC X(10).
               20  FILLER                   PIC X(47).
      *            RESERVE

