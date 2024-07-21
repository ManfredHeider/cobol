      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0350.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0350.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSLEISTUNG  TABLE
CCC004*    ( VERSLEISTUNGID          SMALLINT NOT NULL,
CCC004*      VERSLEISTUNGTEXT        CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSLEISTUNG-GESAMT.
        05  NI-VERSLEISTUNG-TAB.
           10 NI-VERSLEISTUNG-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-VERSLEISTUNG  REDEFINES  NI-VERSLEISTUNG-TAB.
           10 NI-VERSLEISTUNGID           PIC S9(4) COMP.
           10 NI-VERSLEISTUNGTEXT         PIC S9(4) COMP.
        05  DCL-VERSLEISTUNG.
           10 VERSLEISTUNGID              PIC S9(4) COMP.
           10 VERSLEISTUNGTEXT            PIC X(30).
      ******************************************************************
