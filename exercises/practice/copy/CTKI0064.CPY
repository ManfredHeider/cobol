      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0064.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0064.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:46   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VEREIN  TABLE
CCC004*    ( VEREINPERSONENID CHAR (13) NOT NULL,
CCC004*      VEREINLFDNUMMER INTEGER NOT NULL,
CCC004*      VEREINTEXT CHAR (30),
CCC004*      VEREINFUNKTION CHAR (30),
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VEREIN-GESAMT.
        05  NI-VEREIN-TAB.
           10 NI-VEREIN-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-VEREIN  REDEFINES  NI-VEREIN-TAB.
           10 NI-VEREINPERSONENID         PIC S9(4) COMP.
           10 NI-VEREINLFDNUMMER          PIC S9(4) COMP.
           10 NI-VEREINTEXT               PIC S9(4) COMP.
           10 NI-VEREINFUNKTION           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VEREIN.
           10 VEREINPERSONENID            PIC X(13).
           10 VEREINLFDNUMMER             PIC S9(9) COMP.
           10 VEREINTEXT                  PIC X(30).
           10 VEREINFUNKTION              PIC X(30).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
