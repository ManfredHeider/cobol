      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTVB0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTVB0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:10:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0024                           *
      ******************************************************************
        05  WB-NI-TBVB0024-TAB.
           10  WB-NI-TBVB0024-ELE  PIC S9(04)  COMP   OCCURS  19 TIMES.
        05  WB-NI-TBVB0024           REDEFINES   WB-NI-TBVB0024-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-VSWZ-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-VSWZ-ZUSAGE-DAT  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-A-IG-DAT    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-A-GESAMT    PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-DAT-VON   PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-STZ-JAHR  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-VERS-SUM  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-STZ-ANZ   PIC S9(04)  COMP.
           10 WB-NI-VSWZ-G-VERS-NR   PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-VERS-SUM  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-VERS-NR   PIC S9(04)  COMP.
           10 WB-NI-VSWZ-Z-DAT-VON   PIC S9(04)  COMP.
           10 WB-NI-KTY-A-BERECHTIG  PIC S9(04)  COMP.
           10 WB-NI-KNR-A-BERECHTIG  PIC S9(04)  COMP.
           10 WB-NI-KTY-G-BERECHTIG  PIC S9(04)  COMP.
           10 WB-NI-KNR-G-BERECHTIG  PIC S9(04)  COMP.
           10 WB-NI-VSWZ-DBI-DAT     PIC S9(04)  COMP.
           10 WB-NI-VSWZ-DBI-KTO-NR  PIC S9(04)  COMP.
        05  WB-TBVB0024.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
