      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:36   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   06 Feb 1998 10:54:00   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0024)                                         *
      * AAT00000 V001.000 19980205                                     *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0024 TABLE
CCC004*    ( RSTD_JOB_NAME                  CHAR(8) NOT NULL,
CCC004*      RSTD_PRG_NAME                  CHAR(8) NOT NULL,
CCC004*      RSTD_LFD_NR                    SMALLINT NOT NULL,
CCC004*      RSTD_RESTART_DATEN             VARCHAR(4000) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0024                           *
      ******************************************************************
       01  VIAT0024-GESAMT.
         05  DCL-VIAT0024.
           10 JOBNAME              PIC X(8).
           10 PROGRAMMNAME         PIC X(8).
           10 LFD-NR               PIC S9(4) USAGE COMP.
           10 RESTART-ANGABEN.
              49 RESTART-ANGABEN-LEN  PIC S9(4) USAGE COMP.
              49 RESTART-ANGABEN-TEXT  PIC X(4000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************

