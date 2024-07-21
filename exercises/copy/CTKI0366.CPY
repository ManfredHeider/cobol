      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0366.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0366.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   29 Jan 1999 11:34:10   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   09 May 1998 13:30:14   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V006.100 19980909'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  WOHNGEBAEUDEART  TABLE
CCC004*    ( WGEBAEUDEARTID          SMALLINT NOT NULL,
CCC004*      WGEBAEUDEARTTEXT        CHAR (30) NOT NULL,
CCC004*      WGEBAEUDEARTHAURAT      CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  WOHNGEBAEUDEART-GESAMT.
        05  NI-WOHNGEBAEUDEART-TAB.
           10 NI-WOHNGEBAEUDEART-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-WOHNGEBAEUDEART  REDEFINES  NI-WOHNGEBAEUDEART-TAB.
           10 NI-WGEBAEUDEARTID           PIC S9(4) COMP.
           10 NI-WGEBAEUDEARTTEXT         PIC S9(4) COMP.
           10 NI-WGEBAEUDEARTHAURAT       PIC S9(4) COMP.
        05  DCL-WOHNGEBAEUDEART.
           10 WGEBAEUDEARTID              PIC S9(4) COMP.
           10 WGEBAEUDEARTTEXT            PIC X(30).
           10 WGEBAEUDEARTHAURAT          PIC X(30).
      ******************************************************************
