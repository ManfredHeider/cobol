      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wvvb0075.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\wvvb0075.cpv  $
      *
      *   Rev 2.1   Mar 06 2000 15:29:28   T13AE42
      *ASL00050 - D7 / OLB
      *
      *   Rev 2.0   Feb 18 2000 10:15:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0075                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.SCHMIDT                                        *
      *    DATUM:    01.03.2000                                       *
      *    STAND:    01.03.2000                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0075                           *
      ******************************************************************
        05  WB-NI-VIVB0075-TAB.
           10 WB-NI-VIVB0075-ELE   PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-VIVB0075             REDEFINES   WB-NI-VIVB0075-TAB.
           10 WB-NI-OLBB-BETR-NR       PIC S9(04)    COMP.
           10 WB-NI-OLBB-BETR-STUFE    PIC S9(04)    COMP.
           10 WB-NI-OLBB-BETR-UMS      PIC S9(04)    COMP.
           10 WB-NI-OLBB-BETR-EQF      PIC S9(04)    COMP.
           10 WB-NI-OLBB-ERM-DAT       PIC S9(04)    COMP.
           10 WB-NI-OLBB-LAST-UPDATE   PIC S9(04)    COMP.
           10 WB-NI-OLBB-PARTNER-NR    PIC S9(04)    COMP.
           10 WB-NI-OLBB-PARTNER-STUFE PIC S9(04)    COMP.
           10 WB-NI-OLBB-PARTNER-UMS   PIC S9(04)    COMP.

        05  WB-VIVB0075.
           10 WB-OLBB-BETR-NR         PIC S9(9)     USAGE COMP.
           10 WB-OLBB-BETR-STUFE      PIC S9(4)     USAGE COMP.
           10 WB-OLBB-BETR-UMS        PIC S9(09)V9  USAGE COMP-3.
           10 WB-OLBB-BETR-EQF        PIC S9(03)V99 USAGE COMP-3.
           10 WB-OLBB-ERM-DAT         PIC X(10).
           10 WB-OLBB-LAST-UPDATE     PIC X(26).
           10 WB-OLBB-PARTNER-NR      PIC S9(9)     USAGE COMP.
           10 WB-OLBB-PARTNER-STUFE   PIC S9(4)     USAGE COMP.
           10 WB-OLBB-PARTNER-UMS     PIC S9(09)V9  USAGE COMP-3.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS   9
      ******************************************************************
