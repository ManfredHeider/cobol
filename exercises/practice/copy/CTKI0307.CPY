      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0307.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0307.CPv  $
      *
      *   Rev 2.3   Feb 18 2004 16:10:40   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.2   Oct 15 2002 15:35:34   t13ex27
      *AKI00308 DB-Änderung für Fixpack 27
      *
      *   Rev 2.1   12 Sep 2000 16:21:44   T13EX27
      *AKI00149 DB-Änderung für Fixpack 16
      *
      *   Rev 2.0   Feb 27 1999 12:50:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:32   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  FOND  TABLE
CCC004*    ( FONDID SMALLINT NOT NULL,
CCC004*      FONDDVAGPRODSCHL SMALLINT NOT NULL,
CCC004*      FONDGESNR SMALLINT NOT NULL,
CCC004*      FONDWERTPAPKNR CHAR (6) NOT NULL,
CCC004*      FONDWERTPAPKZ CHAR NOT NULL,
CCC004*      FONDBEZEICHNUNG CHAR (40) NOT NULL,
CCC004*      FONDART CHAR (20),
CCC004*      FONDISIN CHAR (12)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  FOND-GESAMT.
        05  NI-FOND-TAB.
120900*    10 NI-FOND-ELE PIC S9(04) COMP OCCURS 6 TIMES.
300902*    10 NI-FOND-ELE PIC S9(04) COMP OCCURS 7 TIMES.
300902     10 NI-FOND-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-FOND  REDEFINES  NI-FOND-TAB.
           10 NI-FONDID                   PIC S9(4) COMP.
           10 NI-FONDDVAGPRODSCHL         PIC S9(4) COMP.
           10 NI-FONDGESNR                PIC S9(4) COMP.
           10 NI-FONDWERTPAPKNR           PIC S9(4) COMP.
           10 NI-FONDWERTPAPKZ            PIC S9(4) COMP.
           10 NI-FONDBEZEICHNUNG          PIC S9(4) COMP.
120900     10 NI-FONDART                  PIC S9(4) COMP.
300902     10 NI-FONDISIN                 PIC S9(4) COMP.
        05  DCL-FOND.
           10 FONDID                      PIC S9(4) COMP.
           10 FONDDVAGPRODSCHL            PIC S9(4) COMP.
           10 FONDGESNR                   PIC S9(4) COMP.
           10 FONDWERTPAPKNR              PIC X(6).
           10 FONDWERTPAPKZ               PIC X(01).
           10 FONDBEZEICHNUNG             PIC X(40).
120900     10 FONDART                     PIC X(20).
300902     10 FONDISIN                    PIC X(12).
      ******************************************************************
