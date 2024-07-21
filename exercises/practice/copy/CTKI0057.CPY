      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0057.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0057.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:40   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ZIELWUNSCHTEXT  TABLE
CCC004*    ( ZWTEXTID                SMALLINT NOT NULL,
CCC004*      ZWGUELTIGKZ             CHAR NOT NULL,
CCC004*      ZWTEXT                  VARCHAR (100)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ZIELWUNSCHTEXT-GESAMT.
        05  NI-ZIELWUNSCHTEXT-TAB.
           10 NI-ZIELWUNSCHTEXT-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-ZIELWUNSCHTEXT  REDEFINES  NI-ZIELWUNSCHTEXT-TAB.
           10 NI-ZWTEXTID                 PIC S9(4) COMP.
           10 NI-ZWGUELTIGKZ              PIC S9(4) COMP.
           10 NI-ZWTEXT                   PIC S9(4) COMP.
        05  DCL-ZIELWUNSCHTEXT.
           10 ZWTEXTID                    PIC S9(4) COMP.
           10 ZWGUELTIGKZ                 PIC X(01).
           10 ZWTEXT.
              49 ZWTEXT-LEN               PIC S9(4) COMP.
              49 ZWTEXT-TEXT              PIC X(100).
      ******************************************************************
