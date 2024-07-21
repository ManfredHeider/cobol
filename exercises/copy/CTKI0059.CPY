      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0059.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0059.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ZUSPERSNAMENART  TABLE
CCC004*    ( ZUSPERSNAMARTID         SMALLINT NOT NULL,
CCC004*      ZUSPERSNAMEARTTEXT      CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ZUSPERSNAMENART-GESAMT.
        05  NI-ZUSPERSNAMENART-TAB.
           10 NI-ZUSPERSNAMENART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-ZUSPERSNAMENART  REDEFINES  NI-ZUSPERSNAMENART-TAB.
           10 NI-ZUSPERSNAMARTID          PIC S9(4) COMP.
           10 NI-ZUSPERSNAMEARTTEXT       PIC S9(4) COMP.
        05  DCL-ZUSPERSNAMENART.
           10 ZUSPERSNAMARTID             PIC S9(4) COMP.
           10 ZUSPERSNAMEARTTEXT          PIC X(30).
      ******************************************************************
