      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wvvba040.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\wvvba040.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   18 Nov 1996 13:14:24   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA040                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 *
      *    DATUM:    05.08.1996                                       *
      *    STAND:    05.08.1996                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA040                           *
      ******************************************************************
        05  WB-NI-VIVBA040-TAB.
           10  WB-NI-VIVBA040-ELE  PIC S9(04)  COMP   OCCURS 12  TIMES.
        05  WB-NI-VIVBA040         REDEFINES   WB-NI-VIVBA040-TAB.
           10 WB-NI-KL-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-KL-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-KL-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-KL-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KL-PROV-STZ-AP PIC S9(04)  COMP.
           10 WB-NI-KL-PROV-STZ-GP PIC S9(04)  COMP.
           10 WB-NI-KL-PROZ-STZ    PIC S9(04)  COMP.
           10 WB-NI-KL-RCK-STZ-DAT PIC S9(04)  COMP.
           10 WB-NI-KL-RCK-STZ     PIC S9(04)  COMP.
           10 WB-NI-MA-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-KNR-KL-TYP     PIC S9(04)  COMP.
           10 WB-NI-KTY-KL-TYP     PIC S9(04)  COMP.
        05  WB-VIVBA040.
           10 WB-KL-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KL-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KL-DAT-VON        PIC X(10)                VALUE SPACE.
           10 WB-KL-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-KL-PROV-STZ-AP    PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-PROV-STZ-GP    PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-PROZ-STZ       PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-KL-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KNR-KL-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-KL-TYP        PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
