      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPV0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPV0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:49:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Dec 1998 13:18:48   T13AE34
      *APA00035 PROGRAMMVERWALTUNG
      *
      *   Rev 1.0   Nov 04 1995 11:50:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPV0001)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTPV0001))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPV0001 TABLE
CCC004*    ( PM_ID                      CHAR(8) NOT NULL,
CCC004*      PM_BESCHR                  CHAR(150) NOT NULL,
CCC004*      KNR_PM_KENNZIFFER          SMALLINT NOT NULL WITH DEFAULT,
CCC004*      KTY_PM_KENNZIFFER          SMALLINT NOT NULL WITH DEFAULT,
CCC004*      PM_LAST_UPDATE             TIMESTAMP NOT NULL WITH DEFAULT,
CCC004*      PM_KENNUNG                 CHAR(8) NOT NULL
300597*      PM_STATUS_ENTW             CHAR(1) NOT NULL,
300597*      PM_STATUS_AACHEN           CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPV0001                           *
      ******************************************************************
       01  DCLTBPV0001.
           10 PM-ID                PIC X(8).
           10 PM-BESCHR            PIC X(150).
970908     10 KNR-PM-KENNZIFFER    PIC S9(4) COMP.
970908     10 KTY-PM-KENNZIFFER    PIC S9(4) COMP.
300597     10 PM-LAST-UPDATE       PIC X(26).
300597     10 PM-KENNUNG           PIC X(8).
300597*    10 PM-STATUS-ENTW       PIC X(1).
300597*    10 PM-STATUS-AACHEN     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
