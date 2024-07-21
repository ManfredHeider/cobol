      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPA0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 08:46:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   04 Jul 1997 08:01:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0024)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0024 TABLE
CCC004*    ( PLOG_TIMESTAMP                 TIMESTAMP NOT NULL,
CCC004*      KNR_OBJEKT_HKF_TAB             SMALLINT NOT NULL,
CCC004*      PLOG_VERS_HKF_TAB              SMALLINT NOT NULL,
CCC004*      PLOG_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PLOG_VERSION                   SMALLINT NOT NULL,
CCC004*      PLOG_V_GUELTIG_AB              TIMESTAMP NOT NULL,
CCC004*      PLOG_V_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PLOG_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PLOG_DIALOG_ID                 CHAR(4) NOT NULL,
CCC004*      PLOG_AEND_TYP                  SMALLINT NOT NULL,
CCC004*      PLOG_LOG_SATZ                  VARCHAR(3946) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0024                           *
      ******************************************************************
       01  TBPA0024-GESAMT.
         05  DCL-TBPA0024.
           10 PLOG-TIMESTAMP       PIC X(26).
           10 KNR-OBJEKT-HKF-TAB   PIC S9(4) USAGE COMP.
           10 PLOG-VERS-HKF-TAB    PIC S9(4) USAGE COMP.
           10 PLOG-ID              PIC S9(15)V USAGE COMP-3.
           10 PLOG-VERSION         PIC S9(4) USAGE COMP.
           10 PLOG-V-GUELTIG-AB    PIC X(26).
           10 PLOG-V-GUELTIG-BIS   PIC X(10).
           10 PLOG-KENNUNG         PIC X(8).
           10 PLOG-DIALOG-ID       PIC X(4).
           10 PLOG-AEND-TYP        PIC S9(4) USAGE COMP.
           10 PLOG-LOG-SATZ.
              49 PLOG-LOG-SATZ-LEN  PIC S9(4) USAGE COMP.
              49 PLOG-LOG-SATZ-TEXT  PIC X(3946).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
