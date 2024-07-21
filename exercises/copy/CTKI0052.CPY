      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0052.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0052.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:18   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TITEL  TABLE
CCC004*    ( TITELID                 SMALLINT NOT NULL,
CCC004*      TITELTEXT               CHAR (15) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  TITEL-GESAMT.
        05  NI-TITEL-TAB.
           10 NI-TITEL-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-TITEL  REDEFINES  NI-TITEL-TAB.
           10 NI-TITELID                  PIC S9(4) COMP.
           10 NI-TITELTEXT                PIC S9(4) COMP.
        05  DCL-TITEL.
           10 TITELID                     PIC S9(4) COMP.
           10 TITELTEXT                   PIC X(15).
      ******************************************************************
