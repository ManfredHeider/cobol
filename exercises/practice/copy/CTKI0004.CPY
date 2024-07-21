      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:47:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:19:44   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:51:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ANREDE  TABLE
CCC004*    ( ANREDEID                SMALLINT NOT NULL,
CCC004*      ANREDETEXT              CHAR (20) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ANREDE-GESAMT.
        05  NI-ANREDE-TAB.
           10 NI-ANREDE-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-ANREDE  REDEFINES  NI-ANREDE-TAB.
           10 NI-ANREDEID                 PIC S9(4) COMP.
           10 NI-ANREDETEXT               PIC S9(4) COMP.
        05  DCL-ANREDE.
           10 ANREDEID                    PIC S9(4) COMP.
           10 ANREDETEXT                  PIC X(20).
      ******************************************************************
