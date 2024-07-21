      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0030.CPv  $
      *
      *   Rev 2.1   Apr 22 2009 10:56:38   t13ex13
      *ASL00328: Unterprogramme
      *
      *   Rev 2.0   Feb 27 1999 15:59:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:23:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: WVVB0030                                      * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 
      *    DATUM:    22.04.1993                                       * 
      *    STAND:    22.04.1993                                       * 
      *    --------------------------------------------------------   * 
      *    VERSION:  001                                              * 
      ***************************************************************** 
      *                                                                 
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0030                           *
      ******************************************************************
        05  WB-NI-VIVB0030-TAB.
           10  WB-NI-VIVB0030-ELE  PIC S9(04)  COMP   OCCURS  14  TIMES.
        05  WB-NI-VIVB0030           REDEFINES   WB-NI-VIVB0030-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-ZV-NR            PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-VON      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-BIS      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-VBG-AB       PIC S9(04)  COMP.
           10 WB-NI-ZVZ-VBG-VNR      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-ABSICHERG    PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DVAG-SUMME   PIC S9(04)  COMP.
           10 WB-NI-ZVZ-VBG-BEITRAG  PIC S9(04)  COMP.
           10 WB-NI-ZVZ-CEN-AB       PIC S9(04)  COMP.
           10 WB-NI-ZVZ-CEN-VNR      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-CEN-BETR     PIC S9(04)  COMP.
           10 WB-NI-ZVZ-CEN-BEITRAG  PIC S9(04)  COMP.
        05  WB-VIVB0030.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-ZV-NR             PIC X(1)                 VALUE SPACE.
           10 WB-ZVZ-DAT-VON       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-ZVZ-DAT-BIS       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-VBG-AB        PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-VBG-VNR       PIC X(15)                VALUE SPACE.
           10 WB-ZVZ-ABSICHERG     PIC S9(9)V9(2) COMP-3    VALUE ZERO.
           10 WB-ZVZ-DVAG-SUMME    PIC S9(9)V9(2) COMP-3    VALUE ZERO.
           10 WB-ZVZ-VBG-BEITRAG   PIC S9(9)V9(2) COMP-3    VALUE ZERO.
           10 WB-ZVZ-CEN-AB        PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-CEN-VNR       PIC X(15)                VALUE SPACE.
           10 WB-ZVZ-CEN-BETR      PIC S9(9)V9(2) COMP-3    VALUE ZERO.
           10 WB-ZVZ-CEN-BEITRAG   PIC S9(9)V9(2) COMP-3    VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
