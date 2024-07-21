      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0914.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0914.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   19 Aug 1998 11:59:40   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   09 May 1998 13:37:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   09 May 1998 12:11:20   P13SP99
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.100 19980721'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PCSTRUKTUR  TABLE
CCC004*    ( PCSTRKVBNR INTEGER NOT NULL,
CCC004*      PCSTRKPCNR INTEGER NOT NULL,
CCC004*      PCSTRKERSTELLDAT DATE NOT NULL WITH DEFAULT,
CCC004*      PCSTRKBETR INTEGER  NOT NULL,
CCC004*      PCSTRKSTRKSTUFE SMALLINT NOT NULL,
CCC004*      PCSTRKVBKZ CHAR (01) NOT NULL,
CCC004*      PCSTRKBETRKZ CHAR (01) NOT NULL,
CCC004*      PCSTRKKIDATENPCKZ SMALLINT NOT NULL,
CCC004*      PCSTRKPCNRDT INTEGER NOT NULL,
CCC004*      PCSTRKHOMEPC INTEGER NOT NULL,
CCC004*      PCSTRKFLEXSTRVBNR INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PCSTRUKTUR-GESAMT.
        05  NI-PCSTRUKTUR-TAB.
           10 NI-PCSTRUKTUR-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  NI-PCSTRUKTUR  REDEFINES  NI-PCSTRUKTUR-TAB.
           10 NI-PCSTRKVBNR               PIC S9(4) COMP.
           10 NI-PCSTRKPCNR               PIC S9(4) COMP.
           10 NI-PCSTRKERSTELLDAT         PIC S9(4) COMP.
           10 NI-PCSTRKBETR               PIC S9(4) COMP.
           10 NI-PCSTRKSTRKSTUFE          PIC S9(4) COMP.
           10 NI-PCSTRKVBKZ               PIC S9(4) COMP.
           10 NI-PCSTRKBETRKZ             PIC S9(4) COMP.
           10 NI-PCSTRKKIDATENPCKZ        PIC S9(4) COMP.
           10 NI-PCSTRKPCNRDT             PIC S9(4) COMP.
           10 NI-PCSTRKHOMEPC             PIC S9(4) COMP.
           10 NI-PCSTRKFLEXSTRVBNR        PIC S9(4) COMP.
        05  DCL-PCSTRUKTUR.
           10 PCSTRKVBNR                  PIC S9(9) COMP.
           10 PCSTRKPCNR                  PIC S9(9) COMP.
           10 PCSTRKERSTELLDAT            PIC X(10).
           10 PCSTRKBETR                  PIC S9(9) COMP.
           10 PCSTRKSTRKSTUFE             PIC S9(4) COMP.
           10 PCSTRKVBKZ                  PIC X(01).
           10 PCSTRKBETRKZ                PIC X(01).
           10 PCSTRKKIDATENPCKZ           PIC S9(4) COMP.
           10 PCSTRKPCNRDT                PIC S9(9) COMP.
           10 PCSTRKHOMEPC                PIC S9(9) COMP.
           10 PCSTRKFLEXSTRVBNR           PIC S9(9) COMP.
      ******************************************************************
