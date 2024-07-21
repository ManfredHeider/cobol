      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0386.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0386.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:48   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   31 Mar 1998 13:12:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  FINDATSPALTE  TABLE
CCC004*    ( FDSPSPALTEID    CHAR(13) NOT NULL,
CCC004*      FDSPAUSRICHTUNG SMALLINT NOT NULL,
CCC004*      FDSPAUTOBREITE  CHAR(1) NOT NULL,
CCC004*      FDSPTABELLE     CHAR(13) NOT NULL,
CCC004*      FDSPBREITE      SMALLINT NOT NULL,
CCC004*      FDSPTABELLENPOS SMALLINT NOT NULL,
CCC004*      FDSPHEADER      VARCHAR(80),
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  FINDATSPALTE-GESAMT.
        05  NI-FINDATSPALTE-TAB.
           10 NI-FINDATSPALTE-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-FINDATSPALTE  REDEFINES  NI-FINDATSPALTE-TAB.
           10 NI-FDSPSPALTEID             PIC S9(4) COMP.
           10 NI-FDSPAUSRICHTUNG          PIC S9(4) COMP.
           10 NI-FDSPAUTOBREITE           PIC S9(4) COMP.
           10 NI-FDSPTABELLE              PIC S9(4) COMP.
           10 NI-FDSPBREITE               PIC S9(4) COMP.
           10 NI-FDSPTABELLENPOS          PIC S9(4) COMP.
           10 NI-FDSPHEADER               PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-FINDATSPALTE.
           10 FDSPSPALTEID                PIC X(13).
           10 FDSPAUSRICHTUNG             PIC S9(4) COMP.
           10 FDSPAUTOBREITE              PIC X(1).
           10 FDSPTABELLE                 PIC X(13).
           10 FDSPBREITE                  PIC S9(4) COMP.
           10 FDSPTABELLENPOS             PIC S9(4) COMP.
           10 FDSPHEADER.
              49 FDSPHEADER-LEN           PIC S9(4) COMP.
              49 FDSPHEADER-TEXT          PIC X(80).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
