      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2333.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2333.CPv  $
      *
      *   Rev 2.3   Nov 02 2009 10:19:48   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.2   Oct 27 2009 12:38:02   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.1   Aug 19 2009 09:22:08   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 07 2009 07:30:40   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI DEPOTBSTAMM DWS                *
      *    ------------------------------------------------------      *
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
      * AKI00844!01.08.09! 1.0! NEU                       !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2333.
      *
               20  BNK-SA                         PIC X(01).
      *            SATZART        = 'C'
               20  BNK-GES                        PIC 9(02).
      *            GESELLSCHAFT   = '48'
               20  BNK-VNR                        PIC 9(12).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  BNK-PORTFOLIO-NR               PIC X(10).

               20  BNK-DAT-DEPOT-EROEFF           PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  BNK-AEND-DAT-DEPOT-EROEFF-KZ   PIC X(01).
      *            ÄNDERUNG-KZ. DEPOTERÖFFNUNG
               20  BNK-DAT-DEPOT-SCHLIES          PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  BNK-AEND-DAT-DEPOT-SCHLIES-KZ  PIC X(01).
      *            ÄNDERUNG-KZ. DATUM DEPOTSCHLIEßUNG
               20  BNK-VERARB-KZ                  PIC X(01).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
      *                D = Delete
               20  FILLER                         PIC X(56).
      *            RESERVE

