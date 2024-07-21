      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0071.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0071.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Apr 30 1996 17:06:36   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0071                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0071 (KTOB)                                 * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: J. SCHINDLER                                     * 00060000
      *    DATUM:    11.12.1995                                       * 00071000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0266!11.12.95! 01 ! NEU                       !SCHINDLER *  00140000
      *         !18.03.96! 02 ! BUCHUNGSTEXT 40 STELLEN   ! AUERBACH *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0071)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0071 TABLE
CCC004*    ( ST_NR              SMALLINT       NOT NULL,
CCC004*      KNR_KTO_TYP        SMALLINT       NOT NULL,
CCC004*      KTOB_BUCH_DAT      DATE           NOT NULL,
CCC004*      KTOB_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KTY_KTO_TYP        SMALLINT       NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KTOB_BUCH_TEXT     CHAR(40)       NOT NULL,
CCC004*      KTOB_BUCH_BETR     DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0071                           *
      ******************************************************************
       01  TBVB0071-GESAMT.
        05  NI-TBVB0071-TAB.
           10 NI-TBVB0071-ELE      PIC S9(04) COMP  OCCURS 9 TIMES.
        05  NI-TBVB0071            REDEFINES  NI-TBVB0071-TAB.
           10 NI-ST-NR             PIC S9(4)    USAGE COMP.
           10 NI-KNR-KTO-TYP       PIC S9(4)    USAGE COMP.
           10 NI-KTOB-BUCH-DAT     PIC S9(4)    USAGE COMP.
           10 NI-KTOB-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KTY-KTO-TYP       PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KTOB-BUCH-TEXT    PIC S9(4)    USAGE COMP.
           10 NI-KTOB-BUCH-BETR    PIC S9(4)    USAGE COMP.
        05  DCL-TBVB0071.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 KNR-KTO-TYP          PIC S9(4)    USAGE COMP.
           10 KTOB-BUCH-DAT        PIC X(10).
           10 KTOB-LAST-UPDATE     PIC X(26).
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KTY-KTO-TYP          PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KTOB-BUCH-TEXT       PIC X(40).
           10 KTOB-BUCH-BETR       PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
