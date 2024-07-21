      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:47:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:26:40   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:52:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BANKBLZ  TABLE
CCC004*    ( BANKBLZID               INTEGER NOT NULL,
CCC004*      BANKBLZLEITZAHL         INTEGER NOT NULL,
CCC004*      BANKLEITZAHLTEXT        CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BANKBLZ-GESAMT.
        05  NI-BANKBLZ-TAB.
           10 NI-BANKBLZ-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-BANKBLZ  REDEFINES  NI-BANKBLZ-TAB.
           10 NI-BANKBLZID                PIC S9(4) COMP.
           10 NI-BANKBLZLEITZAHL          PIC S9(4) COMP.
           10 NI-BANKLEITZAHLTEXT         PIC S9(4) COMP.
        05  DCL-BANKBLZ.
           10 BANKBLZID                   PIC S9(9) COMP.
           10 BANKBLZLEITZAHL             PIC S9(9) COMP.
           10 BANKLEITZAHLTEXT            PIC X(30).
      ******************************************************************
