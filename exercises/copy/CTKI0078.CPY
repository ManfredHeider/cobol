      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0078.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0078.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:08   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   04 Feb 1998 14:57:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ADRESSAENDGRUND  TABLE
CCC004*    ( AAGID SMALLINT NOT NULL,
CCC004*      AAGTEXT CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ADRESSAENDGRUND-GESAMT.
        05  NI-ADRESSAENDGRUND-TAB.
           10 NI-ADRESSAENDGRUND-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-ADRESSAENDGRUND  REDEFINES  NI-ADRESSAENDGRUND-TAB.
           10 NI-AAGID                    PIC S9(4) COMP.
           10 NI-AAGTEXT                  PIC S9(4) COMP.
        05  DCL-ADRESSAENDGRUND.
           10 AAGID                       PIC S9(4) COMP.
           10 AAGTEXT                     PIC X(30).
      ******************************************************************
