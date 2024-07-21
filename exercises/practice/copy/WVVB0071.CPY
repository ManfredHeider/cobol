      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0071.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0071.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Apr 30 1996 17:09:58   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0071                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    WB-BEREICH VON VIVB0071 (KTOB)                             * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: J. SCHINDLER                                     * 00060000
      *    DATUM:    14.12.1995                                       * 00071000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0226!11.12.95! 01 ! NEU                       !SCHINDLER *  00140000
      *         !18.03.96! 02 ! BUCHUNGSTEXT 40 STELLEN   ! AUERBACH *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0071                           *
      ******************************************************************
        05 WB-NI-VIVB0071-TAB.
           10 WB-NI-VIVB0071-ELE   PIC S9(04) COMP  OCCURS 9 TIMES.
        05 WB-NI-VIVB0071         REDEFINES  WB-NI-VIVB0071-TAB.
           10 WB-NI-ST-NR          PIC S9(4)    USAGE COMP.
           10 WB-NI-KNR-KTO-TYP    PIC S9(4)    USAGE COMP.
           10 WB-NI-KTOB-BUCH-DAT  PIC S9(4)    USAGE COMP.
           10 WB-NI-KTOB-LAST-UPDATE PIC S9(4)  USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG   PIC S9(4)    USAGE COMP.
           10 WB-NI-KTY-KTO-TYP    PIC S9(4)    USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG   PIC S9(4)    USAGE COMP.
           10 WB-NI-KTOB-BUCH-TEXT PIC S9(4)    USAGE COMP.
           10 WB-NI-KTOB-BUCH-BETR PIC S9(4)    USAGE COMP.
        05 WB-WB-VIVB0071.
           10 WB-ST-NR             PIC S9(9)    USAGE COMP.
           10 WB-KNR-KTO-TYP       PIC S9(4)    USAGE COMP.
           10 WB-KTOB-BUCH-DAT     PIC X(10).
           10 WB-KTOB-LAST-UPDATE  PIC X(26).
           10 WB-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 WB-KTY-KTO-TYP       PIC S9(4)    USAGE COMP.
           10 WB-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 WB-KTOB-BUCH-TEXT    PIC X(40).
           10 WB-KTOB-BUCH-BETR    PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
