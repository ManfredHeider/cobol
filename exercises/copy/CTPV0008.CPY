      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPV0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPV0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:49:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:51:00   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPV0008)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTPV0008))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPV0008 TABLE
CCC004*    ( PMZ_H_PGM_ID                   CHAR(8) NOT NULL,
CCC004*      PMZ_U_PGM_ID                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPV0008                           *
      ******************************************************************
       01  DCLTBPV0008.
           10 PMZ-H-PGM-ID         PIC X(8).
           10 PMZ-U-PGM-ID         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
