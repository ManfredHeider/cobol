      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.09.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2363.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2363.CPv  $
      *
      *   Rev 2.2   Jun 14 2010 07:39:04   t13ex39
      *FKI01297: fehlerhafter SORT
      *
      *   Rev 2.1   Oct 28 2009 11:01:38   t13ex39
      *AKI00844: DWS-Datenspiegel löschen KSDS-Dateien
      *
      *   Rev 2.0   Sep 25 2009 08:29:20   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER für KSDS DEPOTBESTAND VON DWS                   *
      *    -----------------------------------------                   *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   202                                             *
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
      * -------------------------------------------------------------*
      * FKI01297!14.06.10! 1.1! Neue Satzlänge 202        !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2363.
      *
               20  KSDS-SA                         PIC  X(002).
      *            SATZART        = '18'
               20  KSDS-GES                        PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  KSDS-KEY.
                   25  KSDS-VNR.
                       30 KSDS-FILIALNUMMER        PIC  9(003).
                       30 KSDS-KUNDENNUMMER        PIC  9(007).
                       30 KSDS-UNTERKONTONUMMER    PIC  9(002).
                   25     KSDS-PORTFOLIO-NR        PIC  X(10).

                   25 KSDS-WP-KENN-NR              PIC  X(006).
                   25 KSDS-LFDNR                   PIC  9(002).
               20  FILLER REDEFINES KSDS-KEY.
                   30  KSDS-VNR-1                  PIC  9(010).
                   30  FILLER                      PIC  X(020).
               20  KSDS-WP-KENN-ISIN               PIC  X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  KSDS-Art                        PIC  X(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
               20  KSDS-STUECKE                    PIC  9(12)V9999.
      *            STÜCKANZAHL G
               20  KSDS-ORG-WKZ                    PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  KSDS-KURSWERT-EUR               PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  KSDS-KURSWERT-ORG-WG            PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  KSDS-KUM-EIN                    PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  KSDS-KUM-AUS                    PIC  9(11)V99.
      *            Kumulierte Auszahlung
               20  KSDS-DEL-KZ                     PIC  X(01).
      *            LÖSCHKENNZEICHEN
               20  KSDS-AEND-DAT                   PIC  X(10).
140610*        20  FILLER                          PIC  X(71).
140610         20  FILLER                          PIC  X(73).
      *            RESERVE

      *

