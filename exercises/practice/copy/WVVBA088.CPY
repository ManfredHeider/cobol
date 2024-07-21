      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA088.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA088.CPv  $
      *
      *   Rev 2.1   Apr 29 2016 11:37:22   t13ex30
      *AVB01400: Projekt Lucy2 - ZVVBA088, ZVVBA202
      *
      *   Rev 2.0   Apr 21 2016 13:43:52   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA088                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA088 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    22.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!22.04.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA088                           *
      *****************************************************************
 
        05 WB-NI-VIVBA088-TAB.
           10 WB-NI-VIVBA088-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVBA088          REDEFINES   WB-NI-VIVBA088-TAB.
           10 WB-NI-BOZA-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-BOZA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-BOZA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-BOZA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-BOZA-ANSP-BET-UPFL    PIC S9(04)       BINARY.
 
        05 WB-VIVBA088.
           10 WB-BOZA-ST-NR               PIC S9(09)       BINARY.
           10 WB-BOZA-ANSPR-VOM           PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-BOZA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-BOZA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-BOZA-ANSP-BET-UPFL       PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************

