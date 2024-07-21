      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0388.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0388.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:50   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   31 Mar 1998 13:12:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  FINDATWERT  TABLE
CCC004*    ( FDWEZELLEID     CHAR(13) NOT NULL,
CCC004*      FDWESPALTEPOSITION      SMALLINT NOT NULL,
CCC004*      FDWEWERT        VARCHAR(30) NOT NULL,
CCC004*      FDWESPALTE      CHAR(13) NOT NULL,
CCC004*      FDWEEDITIERBAR  CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  FINDATWERT-GESAMT.
        05  NI-FINDATWERT-TAB.
           10 NI-FINDATWERT-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05  NI-FINDATWERT  REDEFINES  NI-FINDATWERT-TAB.
           10 NI-FDWEZELLEID              PIC S9(4) COMP.
           10 NI-FDWESPALTEPOSITION       PIC S9(4) COMP.
           10 NI-FDWEWERT                 PIC S9(4) COMP.
           10 NI-FDWESPALTE               PIC S9(4) COMP.
           10 NI-FDWEEDITIERBAR           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-FINDATWERT.
           10 FDWEZELLEID                 PIC X(13).
           10 FDWESPALTEPOSITION          PIC S9(4) COMP.
           10 FDWEWERT.
              49 FDWEWERT-LEN             PIC S9(4) COMP.
              49 FDWEWERT-TEXT            PIC X(30).
           10 FDWESPALTE                  PIC X(13).
           10 FDWEEDITIERBAR              PIC X(1).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
