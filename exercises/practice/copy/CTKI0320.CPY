      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0320.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0320.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KFZFAHRZEUGTYP  TABLE
CCC004*    ( KFZTYPID                SMALLINT NOT NULL,
CCC004*      KFZTYPTEXT              CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KFZFAHRZEUGTYP-GESAMT.
        05  NI-KFZFAHRZEUGTYP-TAB.
           10 NI-KFZFAHRZEUGTYP-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-KFZFAHRZEUGTYP  REDEFINES  NI-KFZFAHRZEUGTYP-TAB.
           10 NI-KFZTYPID                 PIC S9(4) COMP.
           10 NI-KFZTYPTEXT               PIC S9(4) COMP.
        05  DCL-KFZFAHRZEUGTYP.
           10 KFZTYPID                    PIC S9(4) COMP.
           10 KFZTYPTEXT                  PIC X(30).
      ******************************************************************
