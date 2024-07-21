      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0037.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0037.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:59:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0037                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0037 (GESE)                                 * 00040001
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    15.09.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    29.10.94 10:16:01
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!15.09.94! 01 ! NEU                       ! KUHN     *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0037)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0037 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      GESE_MONAT                     DATE NOT NULL,
CCC004*      GESE_EH_GS_ZU_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_EH_GS_ZU_GR               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_EH_GS_AB_EG               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_EH_GS_AB_GR               DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0037                           *
      ******************************************************************
       01  TBVB0037-GESAMT.
        05  NI-TBVB0037-TAB.
           10 NI-TBVB0037-ELE      PIC S9(04) COMP  OCCURS 6 TIMES.
        05  NI-TBVB0037            REDEFINES  NI-TBVB0037-TAB.
           10 NI-ST-NR             PIC S9(9) USAGE COMP.
           10 NI-GESE-MONAT        PIC S9(9) USAGE COMP.
           10 NI-GESE-EH-GS-ZU-EG  PIC S9(9) USAGE COMP.
           10 NI-GESE-EH-GS-ZU-GR  PIC S9(9) USAGE COMP.
           10 NI-GESE-EH-GS-AB-EG  PIC S9(9) USAGE COMP.
           10 NI-GESE-EH-GS-AB-GR  PIC S9(9) USAGE COMP.
        05  DCL-TBVB0037.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 GESE-MONAT           PIC X(10).
           10 GESE-EH-GS-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 GESE-EH-GS-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 GESE-EH-GS-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 GESE-EH-GS-AB-GR     PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
