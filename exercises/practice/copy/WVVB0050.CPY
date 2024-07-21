      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0050.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0050.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:27:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: WVVB0050                                       *
      *    ----------------------------------------------------------  *
      *    ERSTELLT: BERGHOFER                                         *
      *    DATUM:    08.03.94                                          *
      *    STAND:    08.03.94                                          *
      *    ----------------------------------------------------------  *
      *    VERSION:  1    31.03.94 17:56:43
      ******************************************************************
      *                                                                *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0050                           *
      ******************************************************************
        05  WB-NI-VIVB0050-TAB.
           10  WB-NI-VIVB0050-ELE     PIC S9(04)  COMP OCCURS  7  TIMES.
        05  WB-NI-VIVB0050         REDEFINES   WB-NI-VIVB0050-TAB.
           10 WB-NI-SDNR-ST-NR        PIC S9(04)  COMP.
           10 WB-NI-SDNR-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-SDNR-BEZ          PIC S9(04)  COMP.
           10 WB-NI-KTY-SDNR-TYP      PIC S9(04)  COMP.
           10 WB-NI-KNR-SDNR-TYP      PIC S9(04)  COMP.
           10 WB-NI-SDNR-ANFANG       PIC S9(04)  COMP.
           10 WB-NI-SDNR-ENDE         PIC S9(04)  COMP.
        05  WB-VIVB0050.
           10 WB-SDNR-ST-NR           PIC S9(9)   COMP      VALUE ZERO.
           10 WB-SDNR-LAST-UPDATE     PIC X(26)             VALUE SPACE.
           10 WB-SDNR-BEZ             PIC X(40)             VALUE SPACE.
           10 WB-KTY-SDNR-TYP         PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-SDNR-TYP         PIC S9(4)   COMP      VALUE ZERO.
           10 WB-SDNR-ANFANG          PIC S9(9)   COMP      VALUE ZERO.
           10 WB-SDNR-ENDE            PIC S9(9)   COMP      VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
