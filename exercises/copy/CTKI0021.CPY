      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:19:52   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:53:00   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  FAMILIENSTAND  TABLE
CCC004*    ( FAMILIENSTANDID         SMALLINT NOT NULL,
CCC004*      FAMILIENSTANDTEXT       CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  FAMILIENSTAND-GESAMT.
        05  NI-FAMILIENSTAND-TAB.
           10 NI-FAMILIENSTAND-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-FAMILIENSTAND  REDEFINES  NI-FAMILIENSTAND-TAB.
           10 NI-FAMILIENSTANDID          PIC S9(4) COMP.
           10 NI-FAMILIENSTANDTEXT        PIC S9(4) COMP.
        05  DCL-FAMILIENSTAND.
           10 FAMILIENSTANDID             PIC S9(4) COMP.
           10 FAMILIENSTANDTEXT           PIC X(30).
      ******************************************************************
