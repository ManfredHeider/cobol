      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA032.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA032.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:10:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVVBA032                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIVBA032 (KTO)                                  * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    12.09.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.10.95 10:13:59
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0237!12.09.95!    ! NEU                       ! SCHÖLLH. *  00140000
      *         !14.10.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00144001
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN VIEW (VIVBA032)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA032 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      KTO_MONAT                      DATE NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KNR_KTO_STAND                  SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_KTO_STAND                  SMALLINT NOT NULL,
CCC004*      KTO_STAND_DISK                 DECIMAL(11, 2) NOT NULL,
CCC004*      KTO_STAND_VORSCH               DECIMAL(11, 2) NOT NULL,
CCC004*      KTO_STAND_RUECK                DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA032                           *
      ******************************************************************
       01  VIVBA032-GESAMT.
        05  NI-VIVBA032-TAB.
           10 NI-VIVBA032-ELE      PIC S9(04) COMP  OCCURS 9  TIMES.
        05  NI-VIVBA032            REDEFINES  NI-VIVBA032-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-KTO-MONAT         PIC S9(4) USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KNR-KTO-STAND     PIC S9(4) USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KTY-KTO-STAND     PIC S9(4) USAGE COMP.
           10 NI-KTO-STAND-DISK    PIC S9(4) USAGE COMP.
           10 NI-KTO-STAND-VORSCH  PIC S9(4) USAGE COMP.
           10 NI-KTO-STAND-RUECK   PIC S9(4) USAGE COMP.
        05  DCL-VIVBA032.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 KTO-MONAT            PIC X(10).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-KTO-STAND        PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-KTO-STAND        PIC S9(4) USAGE COMP.
           10 KTO-STAND-DISK       PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-VORSCH     PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-RUECK      PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
