      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0050.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0050.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:01:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    SRCDCL-MEMBER FÜR DIE DB2-TABLE SDNR                       * 00002000
      *    --------------------------------------------------------   * 00002200
      *    ERSTELLT: KUHN                                             * 00002300
      *    DATUM:    31.03.1994                                       * 00002400
      ***************************************************************** 00002500
      *    VERSION   1    31.03.94 18:10:36
      ****************************************************************  00002700
      * AENDERUNGEN:                                                 *  00002800
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00002900
      * -------------------------------------------------------------*  00003000
      * AFVB0044!31.03.94!    ! NEU                       ! KUHN     *  00004001
      *         !        !    !                           !          *  00005000
      *         !        !    !                           !          *  00006000
      *         !        !    !                           !          *  00007000
      *         !        !    !                           !          *  00008000
      *         !        !    !                           !          *  00009000
      *         !        !    !                           !          *  00010000
      *         !        !    !                           !          *  00010100
      *                                                              *  00010200
      * ENDE-LOGBUCH                                                 *  00010300
      ****************************************************************  00010400
      ******************************************************************
      * DCLGEN TABLE(TBVB0050)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0050))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0050 TABLE
CCC004*    ( SDNR_ST_NR                     INTEGER NOT NULL,
CCC004*      SDNR_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      SDNR_BEZ                       CHAR(40) NOT NULL,
CCC004*      KTY_SDNR_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_SDNR_TYP                   SMALLINT NOT NULL,
CCC004*      SDNR_ANFANG                    INTEGER NOT NULL,
CCC004*      SDNR_ENDE                      INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0050                           *
      ******************************************************************
       01  TBVB0050-GESAMT.
        05  NI-TBVB0050-TAB.
           10  NI-TBVB0050-ELE     PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-TBVB0050            REDEFINES   NI-TBVB0050-TAB.
           10 NI-SDNR-ST-NR        PIC S9(04)  COMP.
           10 NI-SDNR-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-SDNR-BEZ          PIC S9(04)  COMP.
           10 NI-KTY-SDNR-TYP      PIC S9(04)  COMP.
           10 NI-KNR-SDNR-TYP      PIC S9(04)  COMP.
           10 NI-SDNR-ANFANG       PIC S9(04)  COMP.
           10 NI-SDNR-ENDE         PIC S9(04)  COMP.
        05  DCL-TBVB0050.
           10 SDNR-ST-NR           PIC S9(9) USAGE COMP.
           10 SDNR-LAST-UPDATE     PIC X(26).
           10 SDNR-BEZ             PIC X(40).
           10 KTY-SDNR-TYP         PIC S9(9) USAGE COMP.
           10 KNR-SDNR-TYP         PIC S9(9) USAGE COMP.
           10 SDNR-ANFANG          PIC S9(9) USAGE COMP.
           10 SDNR-ENDE            PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
