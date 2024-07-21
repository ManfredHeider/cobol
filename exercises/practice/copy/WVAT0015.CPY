      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:13:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0015                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GIESE                                            * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:41:42
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!18.11.94! 01 ! NEU                       ! GIESE    *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0015                           *
      ******************************************************************
        05  WB-NI-VIAT0015-TAB.
           10 WB-NI-VIAT0015-ELE   PIC S9(04) COMP  OCCURS 9 TIMES.
        05  WB-NI-VIAT0015         REDEFINES  WB-NI-VIAT0015-TAB.
           10 WB-NI-KNR-GRP-SPARTE    PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-PROV-TYP      PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-PROD-SPARTE   PIC S9(4) USAGE COMP.
           10 WB-NI-PSGS-DAT-VON      PIC S9(4) USAGE COMP.
           10 WB-NI-PSGS-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 WB-NI-PSGS-DAT-BIS      PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-GRP-SPARTE    PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROV-TYP      PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-SPARTE   PIC S9(4) USAGE COMP.
        05  WB-VIAT0015.
           10 WB-KNR-GRP-SPARTE       PIC S9(4) USAGE COMP.
           10 WB-KNR-PROV-TYP         PIC S9(4) USAGE COMP.
           10 WB-KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 WB-PSGS-DAT-VON         PIC  X(26).
           10 WB-PSGS-LAST-UPDATE     PIC  X(26).
           10 WB-PSGS-DAT-BIS         PIC  X(10).
           10 WB-KTY-GRP-SPARTE       PIC S9(4) USAGE COMP.
           10 WB-KTY-PROV-TYP         PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
