      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0069.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0069.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  FIRMENBEZART  TABLE
CCC004*    ( FIRMENBEZARTID          SMALLINT NOT NULL,
CCC004*      FIRMENBEZARTTEXT        CHAR (20) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  FIRMENBEZART-GESAMT.
        05  NI-FIRMENBEZART-TAB.
           10 NI-FIRMENBEZART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-FIRMENBEZART  REDEFINES  NI-FIRMENBEZART-TAB.
           10 NI-FIRMENBEZARTID           PIC S9(4) COMP.
           10 NI-FIRMENBEZARTTEXT         PIC S9(4) COMP.
        05  DCL-FIRMENBEZART.
           10 FIRMENBEZARTID              PIC S9(4) COMP.
           10 FIRMENBEZARTTEXT            PIC X(20).
      ******************************************************************
