      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA093.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:35:48   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA093 u. ZVVBB094
      *
      *   Rev 2.0   Apr 19 2016 09:24:18   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA093                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA093 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    19.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!19.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA093                           *
      *****************************************************************
 
        05 WB-NI-VIVBA093-TAB.
           10 WB-NI-VIVBA093-ELE   PIC S9(04)  BINARY OCCURS  13.
        05 WB-NI-VIVBA093          REDEFINES   WB-NI-VIVBA093-TAB.
           10 WB-NI-LZUA-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-OLZ-TYP           PIC S9(04)       BINARY.
           10 WB-NI-LZUA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-LZUA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-LZUA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-LZUA-ANSP-BET-UPFL    PIC S9(04)       BINARY.
 
        05 WB-VIVBA093.
           10 WB-LZUA-ST-NR               PIC S9(09)       BINARY.
           10 WB-KTY-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-OLZ-TYP              PIC S9(04)       BINARY.
           10 WB-LZUA-ANSPR-VOM           PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-LZUA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-LZUA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-LZUA-ANSP-BET-UPFL       PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************

