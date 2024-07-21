      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVATA008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVATA008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:10:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVATA008                                      * 00022003
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: P. SCHMIDT                                       * 00060002
      *    DATUM:    14.11.1994                                       * 00070001
      *    STAND:    14.11.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    10.01.95 16:41:11
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA008                           *
      ******************************************************************
        05  WB-NI-VIATA008-TAB.
           10  WB-NI-VIATA008-ELE  PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  WB-NI-VIATA008         REDEFINES   WB-NI-VIATA008-TAB.
           10 WB-NI-KTY-NR          PIC S9(04)  COMP.
           10 WB-NI-KNR-NR          PIC S9(04)  COMP.
           10 WB-NI-KNR-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZ         PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZ-KURZ    PIC S9(04)  COMP.
           10 WB-NI-KNR-HINDERNIS   PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZ10       PIC S9(04)  COMP.
        05  WB-VIATA008.
           10 WB-KTY-NR            PIC S9(4)  COMP          VALUE ZERO.
           10 WB-KNR-NR            PIC S9(4)  COMP          VALUE ZERO.
           10 WB-KNR-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-KNR-BEZ           PIC X(80)                VALUE SPACE.
           10 WB-KNR-BEZ-KURZ      PIC X(4)                 VALUE SPACE.
           10 WB-KNR-HINDERNIS     PIC X(01)                VALUE SPACE.
           10 WB-KNR-BEZ10         PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
