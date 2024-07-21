      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0014.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0014.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:49:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0014)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0014))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0014 TABLE
CCC004*    ( RSM_TIMESTAMP                  TIMESTAMP NOT NULL,
CCC004*      RSM_HPROG_ID                   CHAR(8) NOT NULL,
CCC004*      RSM_UPROG_ID                   CHAR(8) NOT NULL,
CCC004*      RSM_SQLCODE                    SMALLINT NOT NULL,
CCC004*      RSM_SQLERRMC                   CHAR(70) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0014                           *
      ******************************************************************
       01  TBAT0014-GESAMT.
        05  NI-TBAT0014-TAB.
           10 NI-TBAT0014-ELE      PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  NI-TBAT0014            REDEFINES   NI-TBAT0014-TAB.
           10 NI-RSM-TIMESTAMP     PIC S9(04)  COMP.
           10 NI-RSM-HPROG-ID      PIC S9(04)  COMP.
           10 NI-RSM-UPROG-ID      PIC S9(04)  COMP.
           10 NI-RSM-SQLCODE       PIC S9(04)  COMP.
           10 NI-RSM-SQLERRMC      PIC S9(04)  COMP.
        05 DCL-TBAT0014.
           10 RSM-TIMESTAMP        PIC X(26).
           10 RSM-HPROG-ID         PIC X(8).
           10 RSM-UPROG-ID         PIC X(8).
           10 RSM-SQLCODE          PIC S9(4) USAGE COMP.
           10 RSM-SQLERRMC         PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
