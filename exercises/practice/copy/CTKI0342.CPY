      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0342.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0342.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:56   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERFUEGUNG  TABLE
CCC004*    ( LVERFUEGUNGID           CHAR (2) NOT NULL,
CCC004*      LVERFUEGUNGTEXT         CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERFUEGUNG-GESAMT.
        05  NI-VERFUEGUNG-TAB.
           10 NI-VERFUEGUNG-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-VERFUEGUNG  REDEFINES  NI-VERFUEGUNG-TAB.
           10 NI-LVERFUEGUNGID            PIC S9(4) COMP.
           10 NI-LVERFUEGUNGTEXT          PIC S9(4) COMP.
        05  DCL-VERFUEGUNG.
           10 LVERFUEGUNGID               PIC X(2).
           10 LVERFUEGUNGTEXT             PIC X(30).
      ******************************************************************
