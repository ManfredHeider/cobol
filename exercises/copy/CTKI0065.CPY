      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0065.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0065.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:48   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ZEITRAUM  TABLE
CCC004*    ( ZEITRAUMID              SMALLINT NOT NULL,
CCC004*      ZEITRAUMTEXT            CHAR (15) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ZEITRAUM-GESAMT.
        05  NI-ZEITRAUM-TAB.
           10 NI-ZEITRAUM-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-ZEITRAUM  REDEFINES  NI-ZEITRAUM-TAB.
           10 NI-ZEITRAUMID               PIC S9(4) COMP.
           10 NI-ZEITRAUMTEXT             PIC S9(4) COMP.
        05  DCL-ZEITRAUM.
           10 ZEITRAUMID                  PIC S9(4) COMP.
           10 ZEITRAUMTEXT                PIC X(15).
      ******************************************************************
