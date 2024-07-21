      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0399.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0399.CPv  $
      *
      *   Rev 2.1   May 13 2004 14:13:54   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.0   15 May 2001 13:47:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE WOHNSPARSCHLUESSEL
      ******************************************************************
CCC005*    EXEC SQL DECLARE WOHNSPARSCHLUESSEL TABLE
CCC004*    ( WOHNSPARKEY                    SMALLINT NOT NULL,
CCC004*      WOHNSPARTEXT                   CHAR(25) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  WOHNSPARSCHLUESSEL
      ******************************************************************
       01  WOHNSPARSCHLUESSEL-GESAMT.
         05 NI-WOHNSPARSCHLUESSEL-TAB.
           10 NI-WOHNSPARSCHLUESSEL-ELE PIC S9(4) COMP OCCURS yy TIMES.
         05 NI-WOHNSPARSCHLUESSEL REDEFINES NI-WOHNSPARSCHLUESSEL-TAB.
           10 NI-WOHNSPARKEY        PIC S9(4) COMP.
           10 NI-WOHNSPARTEXT       PIC S9(4) COMP.
         05 DCL-WOHNSPARSCHLUESSEL.
           10 WOHNSPARKEY           PIC S9(4) COMP.
           10 WOHNSPARTEXT          PIC X(25).
      ******************************************************************
