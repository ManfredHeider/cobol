      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBE024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBE024.CPv  $
      *
      *   Rev 2.1   Feb 19 2009 12:49:16   t13ex43
      *ASL00326: PGM - Dialogänderung VB 32
      *
      *   Rev 2.0   Feb 10 2009 09:38:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBE024                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.02.2009                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBE024                          *
      *****************************************************************
        05  WB-NI-VIVBE024-TAB.
           10  WB-NI-VIVBE024-ELE  PIC S9(04)  COMP   OCCURS  35 TIMES.
        05  WB-NI-VIVBE024           REDEFINES   WB-NI-VIVBE024-TAB.
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
           10 WB-NI-VSWZ-G-VERSB-LV    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-VERSB-RV    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-VERSB-LV    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-VERSB-RV    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-AGI-KTO-NR    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-AGI-KTO-NR2   PIC S9(04)  COMP.
        05  WB-VIVBE024.
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
           10 WB-VSWZ-G-VERSB-LV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-G-VERSB-RV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-VERSB-LV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-Z-VERSB-RV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 WB-VSWZ-AGI-KTO-NR   PIC X(10)                VALUE SPACE.
           10 WB-VSWZ-AGI-KTO-NR2  PIC X(10)                VALUE SPACE.
      *****************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 35     *
      *****************************************************************
