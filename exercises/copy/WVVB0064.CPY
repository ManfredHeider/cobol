      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0064.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0064.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:29:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0064                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: SCHMIDT                                          * 00060000
      *    DATUM:    12.09.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:16:19
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0226!12.09.95! 01 ! NEU                       ! SCHMIDT  *  00140000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0064                           *
      ******************************************************************
        05  WB-NI-VIVB0064-TAB.
           10 WB-NI-VIVB0064-ELE   PIC S9(04) COMP  OCCURS 8  TIMES.
        05  WB-NI-VIVB0064         REDEFINES  WB-NI-VIVB0064-TAB.
           10 WB-NI-ST-NR          PIC S9(4)    USAGE COMP.
           10 WB-NI-SLM-DAT-VON    PIC S9(4)    USAGE COMP.
           10 WB-NI-KNR-SLM-TYP    PIC S9(4)    USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG   PIC S9(4)    USAGE COMP.
           10 WB-NI-SLM-LAST-UPDATE PIC S9(4)   USAGE COMP.
           10 WB-NI-KTY-SLM-TYP    PIC S9(4)    USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG   PIC S9(4)    USAGE COMP.
           10 WB-NI-SLM-AUSZAHLUNG PIC S9(4)    USAGE COMP.
        05  WB-VIVB0064.
           10 WB-ST-NR             PIC S9(9)    USAGE COMP.
           10 WB-SLM-DAT-VON       PIC X(10).
           10 WB-KNR-SLM-TYP       PIC S9(4)    USAGE COMP.
           10 WB-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 WB-SLM-LAST-UPDATE   PIC X(26).
           10 WB-KTY-SLM-TYP       PIC S9(4)    USAGE COMP.
           10 WB-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 WB-SLM-AUSZAHLUNG    PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
