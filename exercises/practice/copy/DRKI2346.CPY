      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2346.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2346.CPv  $
      *
      *   Rev 2.6   Apr 15 2015 14:24:48   t13ex39
      *
      *   Rev 2.5   Mar 19 2015 13:58:44   t13ex39
      *FKI01481: DWS - diverse Anpassungen wegen Fehlerdateien
      *
      *   Rev 2.4   Mar 19 2015 12:55:54   t13ex39
      *FKI01481: DWS - diverse Anpassungen wegen Fehlerdateien
      *
      *   Rev 2.3   Nov 02 2009 10:20:10   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.2   Oct 27 2009 12:38:22   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.1   Oct 27 2009 12:05:06   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 07 2009 11:05:32   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI DEPOTBESTAND DWS               *
      *    ---------------------------------------------               *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   179                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00844!01.08.09! 1.0! NEU                       !WHO       *
      * -------------------------------------------------------------*
050215* FKI01481!05.02.15!DWS - diverse Anpassungen wegen ! WHO      *
      *         !        !Fehlerdateien                   !          *
      *         !        !BNK-VNR jetzt PIC 9(10)         !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2346.
      *
               20  BNK-SA                         PIC  X(002).
      *            SATZART        = '00'
               20  BNK-GES                        PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  BNK-KEY.
                   25  BNK-VNR                    PIC  9(010).
                   25  BNK-UNR                    PIC  9(002).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  FILLER                         PIC X(03).
               20  BNK-WP-KENN-NR                 PIC X(06).
      *            WERTPAPIERKENN-NUMMER
               20  BNK-AEND-WP-KENN-NR-KZ         PIC X(01).
      *            ÄNDERUNG-KZ. WERTPAPIERKENN-NUMMER
               20  BNK-WP-KENN-ISIN               PIC X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  BNK-AEND-WP-KENN-ISIN-KZ       PIC X(01).
      *            ÄNDERUNG-KZ. WERTPAPIERKENN-NUMMER ISIN
               20  BNK-ART                        PIC X(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
               20  BNK-AEND-ART-KZ                PIC  X(01).
      *            ÄNDERUNG-KZ. ART DES PAPIERS
               20  BNK-STUECKE-ODER-WERT           PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  BNK-AEND-STUECKE-ODER-WERT-KZ   PIC  X(01).
      *            ÄNDERUNG-KZ. STÜCKANZAHL ODER NOMINALWERT IN
      *            ORIGINALWÄHRUNG
               20  BNK-ORG-WKZ                     PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  BNK-AEND-ORG-WKZ-KZ             PIC  X(01).
      *            ÄNDERUNG-KZ. ORIGINAL-WÄHRUNG DES PAPIERES
               20  BNK-KURSWERT-EUR                PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  BNK-AEND-KURSWERT-EUR-KZ        PIC  X(01).
      *            ÄNDERUNG-KZ. KURSWERT IN EUR
               20  BNK-KURSWERT-ORG-WG             PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  BNK-AEND-KURSWERT-ORG-WG-KZ     PIC  X(01).
      *            ÄNDERUNG-KZ. KURSWERT IN ORG-WÄHRUNG
               20  BNK-GUTHABEN-DATUM-VON          PIC  X(10).
      *            Stand Guthaben
               20  BNK-KUM-EIN                     PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  BNK-AEND-KUM-EIN-KZ             PIC  X(01).
      *            ÄNDERUNG-KZ. Kumulierte Einzahlung
               20  BNK-KUM-AUS                     PIC  9(11)V99.
      *            Kumulierte Auszahlung
               20  BNK-AEND-KUM-AUS-KZ             PIC  X(01).
      *            ÄNDERUNG-KZ. Kumulierte Auszahlung
               20  BNK-VERARB-KZ                   PIC  X(001).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
      *                D = DELETE
               20  FILLER                          PIC  X(50).
      *            RESERVE





