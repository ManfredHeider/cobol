      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0071.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0071.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:36   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   26 Aug 1997 10:33:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  RECHTSFORM  TABLE
CCC004*    ( RECHTSFORMID            SMALLINT NOT NULL,
CCC004*      RECHTSFORMTEXT          CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  RECHTSFORM-GESAMT.
        05  NI-RECHTSFORM-TAB.
           10 NI-RECHTSFORM-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-RECHTSFORM  REDEFINES  NI-RECHTSFORM-TAB.
           10 NI-RECHTSFORMID             PIC S9(4) COMP.
           10 NI-RECHTSFORMTEXT           PIC S9(4) COMP.
        05  DCL-RECHTSFORM.
           10 RECHTSFORMID                PIC S9(4) COMP.
           10 RECHTSFORMTEXT              PIC X(30).
      ******************************************************************
