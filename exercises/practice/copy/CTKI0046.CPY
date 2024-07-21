      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0046.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0046.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:28   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:54:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSBEZIEHUNGSART  TABLE
CCC004*    ( PBEZARTID               SMALLINT NOT NULL,
CCC004*      PBEZARTKATEGORIE        CHAR NOT NULL,
CCC004*      PBEZARTTEXT             CHAR (15) NOT NULL,
CCC004*      PBEZARTINVBEZARTID      SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSBEZIEHUNGSART-GESAMT.
        05  NI-PERSBEZIEHUNGSART-TAB.
           10 NI-PERSBEZIEHUNGSART-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-PERSBEZIEHUNGSART  REDEFINES  NI-PERSBEZIEHUNGSART-TAB.
           10 NI-PBEZARTID                PIC S9(4) COMP.
           10 NI-PBEZARTKATEGORIE         PIC S9(4) COMP.
           10 NI-PBEZARTTEXT              PIC S9(4) COMP.
           10 NI-PBEZARTINVBEZARTID       PIC S9(4) COMP.
        05  DCL-PERSBEZIEHUNGSART.
           10 PBEZARTID                   PIC S9(4) COMP.
           10 PBEZARTKATEGORIE            PIC X(01).
           10 PBEZARTTEXT                 PIC X(15).
           10 PBEZARTINVBEZARTID          PIC S9(4) COMP.
      ******************************************************************
