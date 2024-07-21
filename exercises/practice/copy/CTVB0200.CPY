      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0200.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0200.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:04:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0200)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0200))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0200 TABLE
CCC004*    ( DUMMY_CHAR                     CHAR(6) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0200                           *
      ******************************************************************
       01  DCLTBVB0200.
           10 DUMMY-CHAR           PIC X(6).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 1       *
      ******************************************************************
