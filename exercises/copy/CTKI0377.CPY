      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0377.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0377.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:26   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:08:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KFZSFKLASSEN  TABLE
CCC004*    ( KFZSFKLID               SMALLINT NOT NULL,
CCC004*      KFZSFKLTEXT             CHAR (5) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KFZSFKLASSEN-GESAMT.
        05  NI-KFZSFKLASSEN-TAB.
           10 NI-KFZSFKLASSEN-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-KFZSFKLASSEN  REDEFINES  NI-KFZSFKLASSEN-TAB.
           10 NI-KFZSFKLID                PIC S9(4) COMP.
           10 NI-KFZSFKLTEXT              PIC S9(4) COMP.
        05  DCL-KFZSFKLASSEN.
           10 KFZSFKLID                   PIC S9(4) COMP.
           10 KFZSFKLTEXT                 PIC X(5).
      ******************************************************************
