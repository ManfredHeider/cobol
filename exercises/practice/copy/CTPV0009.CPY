      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPV0009.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPV0009.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:49:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:51:12   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPV0009)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTPV0009))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPV0009 TABLE
CCC004*    ( PM_ID                          CHAR(8) NOT NULL,
CCC004*      MAP_ID                         CHAR(7) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPV0009                           *
      ******************************************************************
       01  DCLTBPV0009.
           10 PM-ID                PIC X(8).
           10 MAP-ID               PIC X(7).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
