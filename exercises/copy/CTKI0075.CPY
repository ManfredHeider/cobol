      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0075.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0075.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:04   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   16 Jan 1998 14:16:40   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSNAMAENDGRUND  TABLE
CCC004*    ( PNAGRUNDID              SMALLINT NOT NULL,
CCC004*      PNAGRUNDTEXT            CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSNAMAENDGRUND-GESAMT.
        05  NI-PERSNAMAENDGRUND-TAB.
           10 NI-PERSNAMAENDGRUND-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-PERSNAMAENDGRUND  REDEFINES  NI-PERSNAMAENDGRUND-TAB.
           10 NI-PNAGRUNDID               PIC S9(4) COMP.
           10 NI-PNAGRUNDTEXT             PIC S9(4) COMP.
        05  DCL-PERSNAMAENDGRUND.
           10 PNAGRUNDID                  PIC S9(4) COMP.
           10 PNAGRUNDTEXT                PIC X(30).
      ******************************************************************
