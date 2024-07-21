      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0369.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0369.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:21:00   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:01:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ZAHLUNGSWEISE  TABLE
CCC004*    ( ZAHLUNGSWEISEID         SMALLINT NOT NULL,
CCC004*      ZAHLUNGSWEISETEXT       CHAR (25) NOT NULL,
CCC004*      ZAHLUNGSWEISEKURZ       CHAR(4)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ZAHLUNGSWEISE-GESAMT.
        05  NI-ZAHLUNGSWEISE-TAB.
           10 NI-ZAHLUNGSWEISE-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-ZAHLUNGSWEISE  REDEFINES  NI-ZAHLUNGSWEISE-TAB.
           10 NI-ZAHLUNGSWEISEID          PIC S9(4) COMP.
           10 NI-ZAHLUNGSWEISETEXT        PIC S9(4) COMP.
           10 NI-ZAHLUNGSWEISEKURZ        PIC S9(4) COMP.
        05  DCL-ZAHLUNGSWEISE.
           10 ZAHLUNGSWEISEID             PIC S9(4) COMP.
           10 ZAHLUNGSWEISETEXT           PIC X(25).
           10 ZAHLUNGSWEISEKURZ           PIC X(4).
      ******************************************************************
