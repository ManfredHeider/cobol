      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:47:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:26:42   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:52:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BANKKONTOART  TABLE
CCC004*    ( BANKKTOARTID            SMALLINT NOT NULL,
CCC004*      BANKKTOARTTEXT          CHAR (20) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BANKKONTOART-GESAMT.
        05  NI-BANKKONTOART-TAB.
           10 NI-BANKKONTOART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BANKKONTOART  REDEFINES  NI-BANKKONTOART-TAB.
           10 NI-BANKKTOARTID             PIC S9(4) COMP.
           10 NI-BANKKTOARTTEXT           PIC S9(4) COMP.
        05  DCL-BANKKONTOART.
           10 BANKKTOARTID                PIC S9(4) COMP.
           10 BANKKTOARTTEXT              PIC X(20).
      ******************************************************************
