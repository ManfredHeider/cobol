      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0063.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0063.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:26   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:44   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  STRUKTURSTUFE  TABLE
CCC004*    ( STRUKTURSTUFEID         INTEGER NOT NULL,
CCC004*      STRSTUFEKURZTEXT        CHAR (5) NOT NULL,
CCC004*      STRSTUFELANGTEXT        CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  STRUKTURSTUFE-GESAMT.
        05  NI-STRUKTURSTUFE-TAB.
           10 NI-STRUKTURSTUFE-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-STRUKTURSTUFE  REDEFINES  NI-STRUKTURSTUFE-TAB.
           10 NI-STRUKTURSTUFEID          PIC S9(4) COMP.
           10 NI-STRSTUFEKURZTEXT         PIC S9(4) COMP.
           10 NI-STRSTUFELANGTEXT         PIC S9(4) COMP.
        05  DCL-STRUKTURSTUFE.
           10 STRUKTURSTUFEID             PIC S9(9) COMP.
           10 STRSTUFEKURZTEXT            PIC X(5).
           10 STRSTUFELANGTEXT            PIC X(30).
      ******************************************************************
