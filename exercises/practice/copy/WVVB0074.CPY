      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wvvb0074.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\wvvb0074.cpv  $
      *
      *   Rev 2.1   Mar 06 2000 15:29:08   T13AE42
      *ASL00049 - D7 / Aufbau OLZ
      *
      *   Rev 2.0   Feb 18 2000 10:15:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0074                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.SCHMIDT                                        *
      *    DATUM:    01.03.2000                                       *
      *    STAND:    01.03.2000                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0074                           *
      ******************************************************************
        05  WB-NI-VIVB0074-TAB.
           10 WB-NI-VIVB0074-ELE   PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  WB-NI-VIVB0074            REDEFINES   WB-NI-VIVB0074-TAB.
           10 WB-NI-AOBV-BETR-NR      PIC S9(04)  COMP.
           10 WB-NI-AOBV-UNTST-NR     PIC S9(04)  COMP.
           10 WB-NI-AOBV-ERM-DAT      PIC S9(04)  COMP.
           10 WB-NI-AOBV-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-AOBV-VB-SEIT      PIC S9(04)  COMP.
           10 WB-NI-AOBV-VB-BIS       PIC S9(04)  COMP.

        05  WB-VIVB0074.
           10 WB-AOBV-BETR-NR         PIC S9(9) USAGE COMP.
           10 WB-AOBV-UNTST-NR        PIC S9(9) USAGE COMP.
           10 WB-AOBV-ERM-DAT         PIC X(10).
           10 WB-AOBV-LAST-UPDATE     PIC X(26).
           10 WB-AOBV-VB-SEIT         PIC X(26).
           10 WB-AOBV-VB-BIS          PIC X(10).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS   6
      ******************************************************************
