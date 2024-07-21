      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0306.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0306.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  DIREKTVERSICHERER  TABLE
CCC004*    ( DIREKTVERSID            CHAR NOT NULL,
CCC004*      DIREKTVERSTEXT          CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  DIREKTVERSICHERER-GESAMT.
        05  NI-DIREKTVERSICHERER-TAB.
           10 NI-DIREKTVERSICHERER-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-DIREKTVERSICHERER  REDEFINES  NI-DIREKTVERSICHERER-TAB.
           10 NI-DIREKTVERSID             PIC S9(4) COMP.
           10 NI-DIREKTVERSTEXT           PIC S9(4) COMP.
        05  DCL-DIREKTVERSICHERER.
           10 DIREKTVERSID                PIC X(01).
           10 DIREKTVERSTEXT              PIC X(30).
      ******************************************************************
