      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1686.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1686.CPv  $
      *
      *   Rev 2.1   Jul 05 2004 10:34:22   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:38:10   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER für KSDS DEPOTBESTAND VON DB24 Historie         *
      *    ----------------------------------------------------------  *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   152                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00082!11.06.04!01.1! NEU                       !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1686.
      *
      *
               20  KSDS-SA                  PIC  X(001).
      *            SATZART        = '6'
               20  KSDS-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  KSDS-KEY-1.
                   25 KSDS-VNR-1            PIC 9(10).
                   25 FILLER                PIC X(13).
               20  KSDS-KEY REDEFINES KSDS-KEY-1.
                   25  KSDS-VNR             PIC  9(015).
      *                VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25  KSDS-WP-KENN-NR      PIC X(06).
      *                WERTPAPIERKENN-NUMMER
                   25  KSDS-LFDNR           PIC  9(002).
      *                LFD. NR. FÜR PRIMARY-KEY
               20  KSDS-DAT-DEPOT-EROEFF    PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  KSDS-DAT-DEPOT-SCHLIES   PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  KSDS-RISIKOKLASSE        PIC X(01).
      *            RISIKOKLASSE
               20  KSDS-ANLAGEZIEL          PIC X(01).
      *            ANLAGEZIEL
               20  KSDS-WP-KENN-ISIN        PIC X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  KSDS-Art                 PIC X(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  KSDS-STUECKE-ODER-WERT   PIC  9(12)V999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  KSDS-ORG-WKZ             PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  KSDS-KURSWERT-EUR        PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  KSDS-KURSWERT-ORG-WG     PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  KSDS-KUM-EIN             PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  KSDS-KUM-AUS             PIC  9(11)V99.
               20  FILLER                   PIC  X(25).
      *            RESERVE

