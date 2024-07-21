      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0363.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0363.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:54   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERTRAGSZUSTAND  TABLE
CCC004*    ( VERTRAGSZUSTANDID       SMALLINT NOT NULL,
CCC004*      VERTRAGSZUSTANDTXT      CHAR (35) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERTRAGSZUSTAND-GESAMT.
        05  NI-VERTRAGSZUSTAND-TAB.
           10 NI-VERTRAGSZUSTAND-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-VERTRAGSZUSTAND  REDEFINES  NI-VERTRAGSZUSTAND-TAB.
           10 NI-VERTRAGSZUSTANDID        PIC S9(4) COMP.
           10 NI-VERTRAGSZUSTANDTXT       PIC S9(4) COMP.
        05  DCL-VERTRAGSZUSTAND.
           10 VERTRAGSZUSTANDID           PIC S9(4) COMP.
           10 VERTRAGSZUSTANDTXT          PIC X(35).
      ******************************************************************
