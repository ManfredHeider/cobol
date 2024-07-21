      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA102.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:45:20   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 25 2016 10:24:50   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA102                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA102 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    25.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!25.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA102                           *
      *****************************************************************
 
        05 WB-NI-VIVBA102-TAB.
           10 WB-NI-VIVBA102-ELE   PIC S9(04)  BINARY OCCURS  31.
        05 WB-NI-VIVBA102          REDEFINES   WB-NI-VIVBA102-TAB.
           10 WB-NI-BUCH-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-KTY-BUCH-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KNR-BUCH-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-BETRAG                PIC S9(04)       BINARY.
           10 WB-NI-BUCH-TEXT             PIC S9(04)       BINARY.
           10 WB-NI-KONTO-TYP             PIC S9(04)       BINARY.
           10 WB-NI-BELEGART              PIC S9(04)       BINARY.
           10 WB-NI-BELEG-NR              PIC S9(04)       BINARY.
           10 WB-NI-TURNUS                PIC S9(04)       BINARY.
           10 WB-NI-BELEG-DAT             PIC S9(04)       BINARY.
           10 WB-NI-BUCH-DAT              PIC S9(04)       BINARY.
           10 WB-NI-KOSTEN-STELLE         PIC S9(04)       BINARY.
           10 WB-NI-GEGEN-KONTO           PIC S9(04)       BINARY.
           10 WB-NI-SHKZ                  PIC S9(04)       BINARY.
           10 WB-NI-BUCH-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-STEUER-JAHR           PIC S9(04)       BINARY.
           10 WB-NI-BEZUGS-JAHR           PIC S9(04)       BINARY.
           10 WB-NI-BEZUG-LFD             PIC S9(04)       BINARY.
           10 WB-NI-ABR-NR                PIC S9(04)       BINARY.
           10 WB-NI-KTY-VERARB-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-VERARB-TYP        PIC S9(04)       BINARY.
           10 WB-NI-ZIEL-STATUS           PIC S9(04)       BINARY.
           10 WB-NI-STEUER-SCHLUESSEL     PIC S9(04)       BINARY.
           10 WB-NI-UST-ANTEIL-U          PIC S9(04)       BINARY.
           10 WB-NI-GEGEN-KONTO-INFO      PIC S9(04)       BINARY.
           10 WB-NI-BUCH-TEXT2            PIC S9(04)       BINARY.
           10 WB-NI-KTY-LEISTUNGSART      PIC S9(04)       BINARY.
           10 WB-NI-KNR-LEISTUNGSART      PIC S9(04)       BINARY.
 
        05 WB-VIVBA102.
           10 WB-BUCH-DAT-VON             PIC  X(10).
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-KTY-BUCH-TYP             PIC S9(04)       BINARY.
           10 WB-KNR-BUCH-TYP             PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-BETRAG                   PIC S9(09)V9(02) COMP-3.
           10 WB-BUCH-TEXT                PIC  X(40).
           10 WB-KONTO-TYP                PIC  X(01).
           10 WB-BELEGART                 PIC  X(03).
           10 WB-BELEG-NR                 PIC  X(07).
           10 WB-TURNUS                   PIC  X(06).
           10 WB-BELEG-DAT                PIC  X(08).
           10 WB-BUCH-DAT                 PIC  X(08).
           10 WB-KOSTEN-STELLE            PIC  X(08).
           10 WB-GEGEN-KONTO              PIC  X(12).
           10 WB-SHKZ                     PIC  X(01).
           10 WB-BUCH-LAST-UPDATE         PIC  X(26).
           10 WB-STEUER-JAHR              PIC S9(04)       BINARY.
           10 WB-BEZUGS-JAHR              PIC S9(04)       BINARY.
           10 WB-BEZUG-LFD                PIC S9(09)       BINARY.
           10 WB-ABR-NR                   PIC  X(15).
           10 WB-KTY-VERARB-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-VERARB-TYP           PIC S9(04)       BINARY.
           10 WB-ZIEL-STATUS              PIC  X(01).
           10 WB-STEUER-SCHLUESSEL        PIC S9(04)       BINARY.
           10 WB-UST-ANTEIL-U             PIC S9(09)V9(02) COMP-3.
           10 WB-GEGEN-KONTO-INFO         PIC  X(12).
           10 WB-BUCH-TEXT2               PIC  X(40).
           10 WB-KTY-LEISTUNGSART         PIC S9(04)       BINARY.
           10 WB-KNR-LEISTUNGSART         PIC S9(04)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

