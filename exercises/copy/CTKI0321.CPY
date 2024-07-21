      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0321.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0321.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:56   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KFZLEISTUNGSART  TABLE
CCC004*    ( KFZLEISTUNGSARTID       SMALLINT NOT NULL,
CCC004*      KFZLEISTARTTEXT         CHAR (10) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KFZLEISTUNGSART-GESAMT.
        05  NI-KFZLEISTUNGSART-TAB.
           10 NI-KFZLEISTUNGSART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-KFZLEISTUNGSART  REDEFINES  NI-KFZLEISTUNGSART-TAB.
           10 NI-KFZLEISTUNGSARTID        PIC S9(4) COMP.
           10 NI-KFZLEISTARTTEXT          PIC S9(4) COMP.
        05  DCL-KFZLEISTUNGSART.
           10 KFZLEISTUNGSARTID           PIC S9(4) COMP.
           10 KFZLEISTARTTEXT             PIC X(10).
      ******************************************************************
