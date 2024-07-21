      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0304.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0304.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:28   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BEITRAGSZUSTAND  TABLE
CCC004*    ( BEITRZUSTANDID          CHAR (4) NOT NULL,
CCC004*      BEITRZUSTANDTEXT        CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BEITRAGSZUSTAND-GESAMT.
        05  NI-BEITRAGSZUSTAND-TAB.
           10 NI-BEITRAGSZUSTAND-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BEITRAGSZUSTAND  REDEFINES  NI-BEITRAGSZUSTAND-TAB.
           10 NI-BEITRZUSTANDID           PIC S9(4) COMP.
           10 NI-BEITRZUSTANDTEXT         PIC S9(4) COMP.
        05  DCL-BEITRAGSZUSTAND.
           10 BEITRZUSTANDID              PIC X(4).
           10 BEITRZUSTANDTEXT            PIC X(30).
      ******************************************************************
