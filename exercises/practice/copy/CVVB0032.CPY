      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0032.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0032.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:17:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVB0032                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVB0032 (KTO)                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: KUHN                                             * 00060000
      *    DATUM:    15.09.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    29.10.94 10:22:13
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
      * DCLGEN TABLE(VIVB0032)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0032 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      KTO_MONAT                      DATE NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTO_UEBWEIS                    DECIMAL(11, 2) NOT NULL,
CCC004*      KTO_STAND_DISK                 DECIMAL(11, 2) NOT NULL,
CCC004*      KTO_STAND_VORSCH               DECIMAL(11, 2) NOT NULL,
CCC004*      KTO_STAND_RUECK                DECIMAL(11, 2) NOT NULL,
CCC004*      KTO_RCK_STZ                    DECIMAL(5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0032                           *
      ******************************************************************
       01  VIVB0032-GESAMT.
        05  NI-VIVB0032-TAB.
           10 NI-VIVB0032-ELE      PIC S9(04) COMP  OCCURS 9  TIMES.
        05  NI-VIVB0032            REDEFINES  NI-VIVB0032-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-KTO-MONAT         PIC S9(4) USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KTO-UEBWEIS       PIC S9(4) USAGE COMP.
           10 NI-KTO-STAND-DISK    PIC S9(4) USAGE COMP.
           10 NI-KTO-STAND-VORSCH  PIC S9(4) USAGE COMP.
           10 NI-KTO-STAND-RUECK   PIC S9(4) USAGE COMP.
           10 NI-KTO-RCK-STZ       PIC S9(4) USAGE COMP.
        05  DCL-VIVB0032.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 KTO-MONAT            PIC X(10).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTO-UEBWEIS          PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-DISK       PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-VORSCH     PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-RUECK      PIC S999999999V99 USAGE COMP-3.
           10 KTO-RCK-STZ          PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
