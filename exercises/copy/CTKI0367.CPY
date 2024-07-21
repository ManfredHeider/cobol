      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0367.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0367.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:16   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  WOHNGEBBAUARTKL  TABLE
CCC004*    ( WGEBBAUARTKLID          SMALLINT NOT NULL,
CCC004*      WGEBBAUARTKLTEXT        CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  WOHNGEBBAUARTKL-GESAMT.
        05  NI-WOHNGEBBAUARTKL-TAB.
           10 NI-WOHNGEBBAUARTKL-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-WOHNGEBBAUARTKL  REDEFINES  NI-WOHNGEBBAUARTKL-TAB.
           10 NI-WGEBBAUARTKLID           PIC S9(4) COMP.
           10 NI-WGEBBAUARTKLTEXT         PIC S9(4) COMP.
        05  DCL-WOHNGEBBAUARTKL.
           10 WGEBBAUARTKLID              PIC S9(4) COMP.
           10 WGEBBAUARTKLTEXT            PIC X(30).
      ******************************************************************
