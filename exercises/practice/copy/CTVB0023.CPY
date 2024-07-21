      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:56:12   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    DCLGEN TABLE(TBVB0023) CTVB0023                            * 00022006
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: KUHN                                             * 00060002
      *    DATUM:    ENDE 1993                                        * 00070001
      ***************************************************************** 00070000
      *    VERSION   1    18.07.94 17:53:16
      ****************************************************************  00090000
      * AENDERUNGEN:                                                 *  00100000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00101000
      * -------------------------------------------------------------*  00102000
      *         !ENDE '93! 01 ! NEU                       ! KUHN     *  00103000
      * AFVB0117!24.07.94!    ! NEU: KTY/KNR-EDD-URSACHE  ! SCHÖLLH. *
      *         !18.07.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *
      *         !        !    !                           !          *  00107304
      *                                                              *  00108000
      * ENDE-LOGBUCH                                                 *  00109000
      ****************************************************************  00110000
      *                                                                 00100000
      ******************************************************************
      * DCLGEN TABLE(TBVB0023)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0023))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0023 TABLE
CCC004*    ( ED_ST_NR                       INTEGER NOT NULL,
CCC004*      EDD_BEZUGS_NR                  INTEGER NOT NULL,
CCC004*      EDD_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      EDD_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      EDD_DAT_BIS                    DATE NOT NULL,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KTY_EDD_URSACHE                SMALLINT NOT NULL,
CCC004*      KNR_EDD_URSACHE                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0023                           *
      ******************************************************************
       01  TBVB0023-GESAMT.
        05  NI-TBVB0023-TAB.
           10  NI-TBVB0023-ELE     PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-TBVB0023            REDEFINES   NI-TBVB0023-TAB.
           10 NI-ED-ST-NR          PIC S9(04)  COMP.
           10 NI-EDD-BEZUGS-NR     PIC S9(04)  COMP.
           10 NI-EDD-DAT-VON       PIC S9(04)  COMP.
           10 NI-EDD-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-EDD-DAT-BIS       PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KTY-EDD-URSACHE   PIC S9(04)  COMP.
           10 NI-KNR-EDD-URSACHE   PIC S9(04)  COMP.
        05  DCL-TBVB0023.
           10 ED-ST-NR             PIC S9(9) USAGE COMP.
           10 EDD-BEZUGS-NR        PIC S9(9) USAGE COMP.
           10 EDD-DAT-VON          PIC X(26).
           10 EDD-LAST-UPDATE      PIC X(26).
           10 EDD-DAT-BIS          PIC X(10).
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KTY-EDD-URSACHE      PIC S9(4) USAGE COMP.
           10 KNR-EDD-URSACHE      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
