      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0335.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CTKI0335.cpv  $
      *
      *   Rev 2.1   Mar 17 1999 15:24:26   P13SP13
      *AKI00020 Erweiterung Tabelle SPARTE
      *um Kurztext
      *
      *   Rev 2.0   Feb 27 1999 12:50:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:20   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.200 19990205'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  SPARTE  TABLE
CCC004*    ( SPARTEID               SMALLINT NOT NULL,
CCC004*      SPARTETEXT             CHAR (30) NOT NULL,
CCC004*      SPARTEKURZTEXT         CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  SPARTE-GESAMT.
        05  NI-SPARTE-TAB.
           10 NI-SPARTE-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-SPARTE  REDEFINES  NI-SPARTE-TAB.
           10 NI-SPARTEID                 PIC S9(4) COMP.
           10 NI-SPARTETEXT               PIC S9(4) COMP.
           10 NI-SPARTEKURZTEXT           PIC S9(4) COMP.
        05  DCL-SPARTE.
           10 SPARTEID                    PIC S9(4) COMP.
           10 SPARTETEXT                  PIC X(30).
           10 SPARTEKURZTEXT              PIC X(04).
      ******************************************************************
