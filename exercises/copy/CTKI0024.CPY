      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:00   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:53:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  HOBBY  TABLE
CCC004*    ( HOBBYPERSONENID CHAR (13) NOT NULL,
CCC004*      HOBBYLFDNUMMER INTEGER NOT NULL,
CCC004*      HOBBYTEXT CHAR (30) NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  HOBBY-GESAMT.
        05  NI-HOBBY-TAB.
           10 NI-HOBBY-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-HOBBY  REDEFINES  NI-HOBBY-TAB.
           10 NI-HOBBYPERSONENID          PIC S9(4) COMP.
           10 NI-HOBBYLFDNUMMER           PIC S9(4) COMP.
           10 NI-HOBBYTEXT                PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-HOBBY.
           10 HOBBYPERSONENID             PIC X(13).
           10 HOBBYLFDNUMMER              PIC S9(9) COMP.
           10 HOBBYTEXT                   PIC X(30).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
