      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0022.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0022.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:30:54   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   06 Feb 1998 10:53:48   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0022)                                         *
      * AAT0000 V001.000 19980205                                      *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0022 TABLE
CCC004*    ( COMF_JOB_NAME                  CHAR(8) NOT NULL,
CCC004*      COMF_PRG_NAME                  CHAR(8) NOT NULL,
CCC004*      COMF_COMMIT_FRQ                INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0022                           *
      ******************************************************************
       01  VIAT0022-GESAMT.
         05  DCL-VIAT0022.
           10 JOBNAME              PIC X(8).
           10 PROGRAMMNAME         PIC X(8).
           10 COMMITFREQUENZ       PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
