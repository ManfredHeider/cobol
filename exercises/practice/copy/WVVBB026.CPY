      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB026.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:44:30   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 24 2016 14:29:08   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB026                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB026 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    24.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!24.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB026                           *
      *****************************************************************
 
        05 WB-NI-VIVBB026-TAB.
           10 WB-NI-VIVBB026-ELE   PIC S9(04)  BINARY OCCURS  12.
        05 WB-NI-VIVBB026          REDEFINES   WB-NI-VIVBB026-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-MONAT            PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-TURNUS           PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-HERKUNFT         PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-VERARB-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-BETRAG           PIC S9(04)       BINARY.
           10 WB-NI-OLZZ-UST-ANTEIL       PIC S9(04)       BINARY.
 
        05 WB-VIVBB026.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-OLZZ-MONAT               PIC  X(10).
           10 WB-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-OLZZ-TURNUS              PIC S9(09)       BINARY.
           10 WB-OLZZ-HERKUNFT            PIC  X(01).
           10 WB-OLZZ-VERARB-TYP          PIC  X(01).
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-OLZZ-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-OLZZ-BETRAG              PIC S9(09)V9(02) COMP-3.
           10 WB-OLZZ-UST-ANTEIL          PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

