      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:08:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    SRCDCL-MEMBER FÜR DIE VIEW AUF DIE DB2-TABLE ST            * 00002000
      *    --------------------------------------------------------   * 00002200
      *    ERSTELLT: KUHN                                             * 00002300
      *    DATUM:    31.03.1994                                       * 00002400
      ***************************************************************** 00002500
      *    VERSION   1    31.03.94 18:08:16
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
      * DCLGEN TABLE(VIVBA001)                                         *
      *        LIBRARY(T130003.CE010000.SRCDCL(CVVBA001))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA001 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      ST_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      ST_PRUEFZIFFER                 CHAR(1) NOT NULL,
CCC004*      ST_FREI                        CHAR(1) NOT NULL,
CCC004*      ST_BEREINIGUNG                 CHAR(1) NOT NULL,
CCC004*      KTY_ST_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_ST_TYP                     SMALLINT NOT NULL,
CCC004*      KTY_GESELL_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_GESELL_TYP                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA001                           *
      ******************************************************************
       01  VIVBA001-GESAMT.
        05  NI-VIVBA001-TAB.
           10 NI-VIVBA001-ELE      PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-VIVBA001            REDEFINES   NI-VIVBA001-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-ST-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-ST-PRUEFZIFFER    PIC S9(04)  COMP.
           10 NI-ST-FREI           PIC S9(04)  COMP.
           10 NI-ST-BEREINIGUNG    PIC S9(04)  COMP.
           10 NI-KTY-ST-TYP        PIC S9(04)  COMP.
           10 NI-KNR-ST-TYP        PIC S9(04)  COMP.
           10 NI-KTY-GESELL-TYP    PIC S9(04)  COMP.
           10 NI-KNR-GESELL-TYP    PIC S9(04)  COMP.
        05  DCL-VIVBA001.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 ST-LAST-UPDATE       PIC X(26).
           10 ST-PRUEFZIFFER       PIC X(1).
           10 ST-FREI              PIC X(1).
           10 ST-BEREINIGUNG       PIC X(1).
           10 KTY-ST-TYP           PIC S9(4) USAGE COMP.
           10 KNR-ST-TYP           PIC S9(4) USAGE COMP.
           10 KTY-GESELL-TYP       PIC S9(4) USAGE COMP.
           10 KNR-GESELL-TYP       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
