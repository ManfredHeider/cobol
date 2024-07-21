      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1337.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1337.CPv  $
      *
      *   Rev 2.2   Nov 26 2002 12:16:42   t13ex39
      *AKI00315  STAND GUTHABEN
      *
      *   Rev 2.1   Jul 30 2002 16:33:50   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI DEPOTBESTAND DWS               *
      *    ----------------------------------------------------------  *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   162                                             *
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
      *         !        !    ! Datum Stand Guthaben      !          *
      *         !        !    !  zus. kum. Ein- Auszah    !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI1337.
      *
               20  BNK-SA                         PIC  X(001).
      *            SATZART        = 'C'
               20  BNK-GES                        PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  BNK-VNR                        PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  BNK-DAT-DEPOT-EROEFF           PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  BNK-AEND-DAT-DEPOT-EROEFF-KZ   PIC X(01).
      *            ÄNDERUNG-KZ. DEPOTERÖFFNUNG
               20  BNK-DAT-DEPOT-SCHLIES          PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  BNK-AEND-DAT-DEPOT-SCHLIES-KZ  PIC X(01).
      *            ÄNDERUNG-KZ. DATUM DEPOTSCHLIEßUNG
               20  BNK-RISIKOKLASSE               PIC X(01).
      *            RISIKOKLASSE
               20  BNK-AEND-RISIKOKLASSE-KZ       PIC X(01).
      *            ÄNDERUNG-KZ. RISIKOKLASSE
               20  BNK-ANLAGEZIEL                 PIC X(01).
      *            ANLAGEZIEL
               20  BNK-AEND-ANLAGEZIEL-KZ         PIC X(01).
      *            ÄNDERUNG-KZ. ANLAGEZIEL
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
      *            2 = NOMINALWERTE
               20  BNK-AEND-ART-KZ                PIC  X(01).
      *            ÄNDERUNG-KZ. ART DES PAPIERS
               20  BNK-STUECKE-ODER-WERT           PIC  9(12)V999.
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
               20  BNK-VERARB-KZ                   PIC  X(001).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
               20  BNK-GUTHABEN-DATUM-VON          PIC  X(10).
..    *            Stand Guthaben
..             20  BNK-KUM-EIN                     PIC  9(11)V99.
..    *            Kumulierte Einzahlung
..             20  BNK-AEND-KUM-EIN-KZ             PIC  X(01).
..    *            ÄNDERUNG-KZ. Kumulierte Einzahlung
..             20  BNK-KUM-AUS                     PIC  9(11)V99.
..    *            Kumulierte Auszahlung
..             20  BNK-AEND-KUM-AUS-KZ             PIC  X(01).
..    *            ÄNDERUNG-KZ. Kumulierte Auszahlung
..
..
..
..    *        20  FILLER                          PIC  X(51).
               20  FILLER                          PIC  X(13).
      *            RESERVE

