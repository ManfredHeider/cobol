      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:26:52   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:52:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  EIGENTUMSART  TABLE
CCC004*    ( EIGENTUMSARTID          CHAR (3) NOT NULL,
CCC004*      EIGENTUMSARTTEXT        CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  EIGENTUMSART-GESAMT.
        05  NI-EIGENTUMSART-TAB.
           10 NI-EIGENTUMSART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-EIGENTUMSART  REDEFINES  NI-EIGENTUMSART-TAB.
           10 NI-EIGENTUMSARTID           PIC S9(4) COMP.
           10 NI-EIGENTUMSARTTEXT         PIC S9(4) COMP.
        05  DCL-EIGENTUMSART.
           10 EIGENTUMSARTID              PIC X(3).
           10 EIGENTUMSARTTEXT            PIC X(30).
      ******************************************************************
