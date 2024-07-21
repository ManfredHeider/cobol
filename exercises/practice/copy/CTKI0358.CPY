      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0358.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0358.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:00   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERTRAGBANKVBDG  TABLE
CCC004*    ( VBVVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VBVPERSONENID CHAR (13) NOT NULL,
CCC004*      VBVLAUFENDENUMMER INTEGER NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERTRAGBANKVBDG-GESAMT.
        05  NI-VERTRAGBANKVBDG-TAB.
           10 NI-VERTRAGBANKVBDG-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-VERTRAGBANKVBDG  REDEFINES  NI-VERTRAGBANKVBDG-TAB.
           10 NI-VBVVERTRAGSID            PIC S9(4) COMP.
           10 NI-VBVPERSONENID            PIC S9(4) COMP.
           10 NI-VBVLAUFENDENUMMER        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VERTRAGBANKVBDG.
           10 VBVVERTRAGSID               PIC X(13).
           10 VBVPERSONENID               PIC X(13).
           10 VBVLAUFENDENUMMER           PIC S9(9) COMP.
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
