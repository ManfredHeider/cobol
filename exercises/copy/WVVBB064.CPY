      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB064.CPv  $
      *
      *   Rev 2.1   May 31 2016 07:44:34   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 24 2016 14:29:14   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB064                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB064 (ALIAS)                        *
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
      * COBOL DEKLARATION FÜR VIEW VIVBB064                           *
      *****************************************************************
 
        05 WB-NI-VIVBB064-TAB.
           10 WB-NI-VIVBB064-ELE   PIC S9(04)  BINARY OCCURS  12.
        05 WB-NI-VIVBB064          REDEFINES   WB-NI-VIVBB064-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-SLM-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-KNR-SLM-TYP           PIC S9(04)       BINARY.
           10 WB-NI-SLM-TURNUS            PIC S9(04)       BINARY.
           10 WB-NI-SLM-HERKUNFT          PIC S9(04)       BINARY.
           10 WB-NI-SLM-VERARB-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-SLM-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-SLM-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-SLM-AUSZAHLUNG        PIC S9(04)       BINARY.
           10 WB-NI-SLM-UST-ANTEIL        PIC S9(04)       BINARY.
 
        05 WB-VIVBB064.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-SLM-DAT-VON              PIC  X(10).
           10 WB-KNR-SLM-TYP              PIC S9(04)       BINARY.
           10 WB-SLM-TURNUS               PIC S9(09)       BINARY.
           10 WB-SLM-HERKUNFT             PIC  X(01).
           10 WB-SLM-VERARB-TYP           PIC  X(01).
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-SLM-LAST-UPDATE          PIC  X(26).
           10 WB-KTY-SLM-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-SLM-AUSZAHLUNG           PIC S9(09)V9(02) COMP-3.
           10 WB-SLM-UST-ANTEIL           PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************

