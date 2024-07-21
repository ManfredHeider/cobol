      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1345.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1345.CPv  $
      *
      *   Rev 2.2   Nov 26 2002 12:17:10   t13ex39
      *AKI00315  STAND GUTHABEN
      *
      *   Rev 2.1   Jul 30 2002 16:34:44   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER für KSDS DEPOTBESTAND VON DB24                  *
      *    ----------------------------------------------------------  *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!07.03.02!01.1! NEU                       !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00315!05.11.02!01.2! Neue Felder               !WHO       *
      *         !        !    ! kum. Ein- Auszah          !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1345.
      *
      *
               20  KSDS-SA                  PIC  X(001).
      *            SATZART        = '6'
               20  KSDS-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  KSDS-KEY-1.
                   25 KSDS-VNR-1            PIC 9(10).
                   25 FILLER                PIC X(11).
               20  KSDS-KEY REDEFINES KSDS-KEY-1.
                   25  KSDS-VNR             PIC  9(015).
      *                VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25  KSDS-WP-KENN-NR      PIC X(06).
      *                WERTPAPIERKENN-NUMMER
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

