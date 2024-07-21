      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2844.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2844.CPv  $
      *
      *   Rev 2.3   Mar 01 2016 14:32:28   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.2   Feb 16 2016 14:09:52   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.1   Feb 18 2014 14:22:50   t13ex77
      *AKI01274: Prüfung der DeuBa-DTAs
      *
      *   Rev 2.0   Jan 27 2014 13:34:58   t13ex77
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR DEPOTTRANSAKTION DEUTSCHE BANK SICHERUNG    *
      *    --------------------------------------------------------    *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !01  ! NEU                       !ABN       *
      * -------------------------------------------------------------*
120216* AKI01419!12.02.16!02  ! BUCH-ZINS-AB erweitern    !DDY       *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      * Datensatzstruktur für die Sicherung der Buchungsdaten
      *
      *   Datum, wann der Datensatz gesichert worden ist
           05  'XX'-SICHERUNGSDATUM            PIC  9(08).
      *   Kennzeichen, ob ein Datensatz gelöscht werden soll -> 'L'
           05  'XX'-LOESCH-KZ                  PIC  X(01).
      *   Zu sichernde Daten
           05  'XX'-DATEN.
      *       Nummer des gesicherten Vertrags
               10 'XX'-BNK-GES                 PIC  9(02).
               10 'XX'-BNK-VNR                 PIC  9(15).
               10 'XX'-BNK-ANTEIL-LFDNR        PIC  9(02).
               10 'XX'-BNK-WP-KENN-NR          PIC  X(06).
               10 'XX'-BNK-WP-KENN-NR-ISIN     PIC  X(12).
               10 'XX'-BNK-BUCH-GUTHABEN       PIC  9(11)V99.
               10 'XX'-BNK-BUCH-BETRAG         PIC  9(07)V99.
               10 'XX'-BNK-BUCH-DATUM          PIC  X(10).
               10 'XX'-BNK-BUCH-SCHLUESSEL     PIC S9(03).
               10 'XX'-BNK-BUCH-STEUERBETRAG   PIC  9(07)V99.
               10 'XX'-BNK-BUCH-ANTEILE        PIC  9(06)V9999.
120216*        10 'XX'-BNK-BUCH-ZINS-AB-ST     PIC  9(06)V99.
120216         10 'XX'-BNK-BUCH-ZINS-AB-ST     PIC  9(09)V99.
               10 'XX'-BNK-VL-KZ               PIC  X(01).
               10 'XX'-BNK-WKZ                 PIC  X(01).
120216*        10 'XX'-FILLER                  PIC  X(99).
120216         10 'XX'-FILLER                  PIC  X(96).
               10 'XX'-BNK-AKTUAL-KZ           PIC  X(01).
 

