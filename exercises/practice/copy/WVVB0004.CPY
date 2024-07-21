      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:17:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0004                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0004                           *
      ******************************************************************
        05  WB-NI-VIVB0004-TAB.
           10  WB-NI-VIVB0004-ELE  PIC S9(04)  COMP   OCCURS  24 TIMES.
        05  WB-NI-VIVB0004           REDEFINES   WB-NI-VIVB0004-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-MA-LAST-UPDATE   PIC S9(04)  COMP.
           10 WB-NI-KTY-MA-TYP       PIC S9(04)  COMP.
           10 WB-NI-KNR-MA-TYP       PIC S9(04)  COMP.
           10 WB-NI-MA-BDV-MITGLIED  PIC S9(04)  COMP.
           10 WB-NI-MA-STRAFE        PIC S9(04)  COMP.
           10 WB-NI-MA-VERSANDSTOP   PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-ZVII-ABL   PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ       PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ-DAT   PIC S9(04)  COMP.
           10 WB-NI-MA-FUEH-ZEUG-DAT PIC S9(04)  COMP.
           10 WB-NI-MA-SCHUFA-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-34C-DAT       PIC S9(04)  COMP.
           10 WB-NI-MA-AFO           PIC S9(04)  COMP.
           10 WB-NI-MA-DARLEHEN      PIC S9(04)  COMP.
           10 WB-NI-MA-AVAG-DVAG     PIC S9(04)  COMP.
           10 WB-NI-MA-KOMPASS-AVAG  PIC S9(04)  COMP.
           10 WB-NI-MA-AUTOTELEFON   PIC S9(04)  COMP.
           10 WB-NI-MA-TELETEXT      PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-NR         PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS-R  PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-VON    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-KUE-DAT    PIC S9(04)  COMP.
        05  WB-VIVB0004.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-MA-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KTY-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MA-BDV-MITGLIED   PIC X(1)                 VALUE SPACE.
           10 WB-MA-STRAFE         PIC X(1)                 VALUE SPACE.
           10 WB-MA-VERSANDSTOP    PIC X(1)                 VALUE SPACE.
           10 WB-MA-ZV-ZVII-ABL    PIC X(1)                 VALUE SPACE.
           10 WB-MA-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-MA-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-MA-FUEH-ZEUG-DAT  PIC X(10)                VALUE SPACE.
           10 WB-MA-SCHUFA-DAT     PIC X(10)                VALUE SPACE.
           10 WB-MA-34C-DAT        PIC X(10)                VALUE SPACE.
           10 WB-MA-AFO            PIC X(1)                 VALUE SPACE.
           10 WB-MA-DARLEHEN       PIC X(1)                 VALUE SPACE.
           10 WB-MA-AVAG-DVAG      PIC X(10)                VALUE SPACE.
           10 WB-MA-KOMPASS-AVAG   PIC X(10)                VALUE SPACE.
           10 WB-MA-AUTOTELEFON    PIC X(14)                VALUE SPACE.
           10 WB-MA-TELETEXT       PIC X(14)                VALUE SPACE.
           10 WB-MA-ZV-NR          PIC X(1)                 VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS-R   PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS     PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-DAT-VON     PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-KUE-DAT     PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************
