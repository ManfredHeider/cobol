      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0049.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0049.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:32   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:54:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSREFNUMMERART  TABLE
CCC004*    ( PREFNUMARTID            SMALLINT NOT NULL,
CCC004*      PREFNUMARTTEXT          CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSREFNUMMERART-GESAMT.
        05  NI-PERSREFNUMMERART-TAB.
           10 NI-PERSREFNUMMERART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-PERSREFNUMMERART  REDEFINES  NI-PERSREFNUMMERART-TAB.
           10 NI-PREFNUMARTID             PIC S9(4) COMP.
           10 NI-PREFNUMARTTEXT           PIC S9(4) COMP.
        05  DCL-PERSREFNUMMERART.
           10 PREFNUMARTID                PIC S9(4) COMP.
           10 PREFNUMARTTEXT              PIC X(30).
      ******************************************************************
