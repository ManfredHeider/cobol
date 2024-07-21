      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA032.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA032.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:15:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVBA032                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    12.09.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.10.95 10:14:57
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0237!12.09.95!    ! NEU                       ! SCHÖLLH. *  00140000
      *         !14.10.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00136001
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA032                           *
      ****************************************************************** *
        05  WB-NI-VIVBA032-TAB.
           10 WB-NI-VIVBA032-ELE     PIC S9(04) COMP  OCCURS 9  TIMES.
        05  WB-NI-VIVBA032           REDEFINES  WB-NI-VIVBA032-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-MONAT        PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-KTO-STAND    PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-KTO-STAND    PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-STAND-DISK   PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-STAND-VORSCH PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-STAND-RUECK  PIC S9(4) USAGE COMP.
        05  WB-VIVBA032.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-KTO-MONAT           PIC X(10).
           10 WB-KNR-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KNR-KTO-STAND       PIC S9(4) USAGE COMP.
           10 WB-KTY-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KTY-KTO-STAND       PIC S9(4) USAGE COMP.
           10 WB-KTO-STAND-DISK      PIC S999999999V99 USAGE COMP-3.
           10 WB-KTO-STAND-VORSCH    PIC S999999999V99 USAGE COMP-3.
           10 WB-KTO-STAND-RUECK     PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
