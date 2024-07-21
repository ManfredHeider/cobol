      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:26:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:52:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  EIGENTUMSNUTZART  TABLE
CCC004*    ( EIGENTUMSNARTID         CHAR (3) NOT NULL,
CCC004*      EIGENTUMSNARTTEXT       CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  EIGENTUMSNUTZART-GESAMT.
        05  NI-EIGENTUMSNUTZART-TAB.
           10 NI-EIGENTUMSNUTZART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-EIGENTUMSNUTZART  REDEFINES  NI-EIGENTUMSNUTZART-TAB.
           10 NI-EIGENTUMSNARTID          PIC S9(4) COMP.
           10 NI-EIGENTUMSNARTTEXT        PIC S9(4) COMP.
        05  DCL-EIGENTUMSNUTZART.
           10 EIGENTUMSNARTID             PIC X(3).
           10 EIGENTUMSNARTTEXT           PIC X(30).
      ******************************************************************
