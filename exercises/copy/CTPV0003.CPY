      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPV0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPV0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:49:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:50:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPV0003)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTPV0003))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPV0003 TABLE
CCC004*    ( MAP_ID                         CHAR(7) NOT NULL,
CCC004*      MAP_BESCHR                     CHAR(150) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPV0003                           *
      ******************************************************************
       01  DCLTBPV0003.
           10 MAP-ID               PIC X(7).
           10 MAP-BESCHR           PIC X(150).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
