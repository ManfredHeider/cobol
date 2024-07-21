      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0387.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0387.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:50   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   31 Mar 1998 13:12:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  FINDATTABELLE  TABLE
CCC004*    ( FDTATABELLEID   CHAR(13) NOT NULL,
CCC004*      FDTABREITE      SMALLINT NOT NULL,
CCC004*      FDTABEZEICHNUNG VARCHAR(100),
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  FINDATTABELLE-GESAMT.
        05  NI-FINDATTABELLE-TAB.
           10 NI-FINDATTABELLE-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-FINDATTABELLE  REDEFINES  NI-FINDATTABELLE-TAB.
           10 NI-FDTATABELLEID            PIC S9(4) COMP.
           10 NI-FDTABREITE               PIC S9(4) COMP.
           10 NI-FDTABEZEICHNUNG          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-FINDATTABELLE.
           10 FDTATABELLEID               PIC X(13).
           10 FDTABREITE                  PIC S9(4) COMP.
           10 FDTABEZEICHNUNG.
              49 FDTABEZEICHNUNG-LEN      PIC S9(4) COMP.
              49 FDTABEZEICHNUNG-TEXT     PIC X(100).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
