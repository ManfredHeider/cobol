      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB024.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBB024.CPv  $
      *
      *   Rev 2.2   Mar 22 2006 14:54:10   t13ae28
      *ASL00216: VSWZ-neuer DCLGEN etc.
      *
      *   Rev 2.1   Mar 06 2006 15:49:56   t13ae30
      *ASL00216: VSWZ-neuer DCLGEN etc.
      *
      *   Rev 2.0   Nov 22 2005 14:26:40   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB024                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    22.11.2005                                       *
      *    STAND:    22.11.2005                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB024                           *
      ******************************************************************
        05  WB-NI-VIVBB024-TAB.
           10  WB-NI-VIVBB024-ELE  PIC S9(04)  COMP   OCCURS  29 TIMES.
        05  WB-NI-VIVBB024           REDEFINES   WB-NI-VIVBB024-TAB.
           10 WB-NI-MA-ST-NR           PIC S9(04)  COMP.
           10 WB-NI-VSWZ-LAST-UPDATE   PIC S9(04)  COMP.
           10 WB-NI-VSWZ-ZUSAGE-DAT    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-A-IG-DAT      PIC S9(04)  COMP.
           10 WB-NI-VSWZ-A-GESAMT      PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-STZ-JAHR    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-VERS-SUM    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-STZ-ANZ     PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-VERS-NR     PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-VERS-SUM    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-VERS-NR     PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-KTY-A-BERECHTIG    PIC S9(04)  COMP.
           10 WB-NI-KNR-A-BERECHTIG    PIC S9(04)  COMP.
           10 WB-NI-KTY-G-BERECHTIG    PIC S9(04)  COMP.
           10 WB-NI-KNR-G-BERECHTIG    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-DBI-DAT       PIC S9(04)  COMP.
           10 WB-NI-VSWZ-DBI-KTO-NR    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-RV-VERS-NR  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-RV-RENTE    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-RV-ABF      PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-RV-BURENTE  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-RV-VERS-NR  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-RV-RENTE    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-RV-ABF      PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-RV-BURENTE  PIC S9(04)  COMP.
           10 WB-NI-KTY-Z-BERECHTIG    PIC S9(04)  COMP.
           10 WB-NI-KNR-Z-BERECHTIG    PIC S9(04)  COMP.
        05  WB-VIVBB024.
           10 WB-MA-ST-NR          PIC S9(9)     COMP       VALUE ZERO.
           10 WB-VSWZ-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-VSWZ-ZUSAGE-DAT   PIC X(10)                VALUE SPACE.
           10 WB-VSWZ-A-IG-DAT     PIC X(10)                VALUE SPACE.
           10 WB-VSWZ-A-GESAMT     PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-G-DAT-VON    PIC X(10)                VALUE SPACE.
           10 WB-VSWZ-G-STZ-JAHR   PIC X(10)                VALUE SPACE.
           10 WB-VSWZ-G-VERS-SUM   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-G-STZ-ANZ    PIC S9(4)     COMP       VALUE ZERO.
           10 WB-VSWZ-G-VERS-NR    PIC S9(15)    COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-VERS-SUM   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-VERS-NR    PIC S9(15)    COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-DAT-VON    PIC X(10)                VALUE SPACE.
           10 WB-KTY-A-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 WB-KNR-A-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 WB-KTY-G-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 WB-KNR-G-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 WB-VSWZ-DBI-DAT      PIC X(10)                VALUE SPACE.
           10 WB-VSWZ-DBI-KTO-NR   PIC X(09)                VALUE SPACE.
           10 WB-VSWZ-G-RV-VERS-NR PIC S9(15)    COMP-3     VALUE ZERO.
           10 WB-VSWZ-G-RV-RENTE   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-G-RV-ABF     PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-G-RV-BURENTE PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-RV-VERS-NR PIC S9(15)    COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-RV-RENTE   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-RV-ABF     PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-RV-BURENTE PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-KTY-Z-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 WB-KNR-Z-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 29      *
      ******************************************************************
