      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0390.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0390.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Jan 1999 11:34:18   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   07 Jul 1998 16:49:12   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.100 19980703'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSSTEUER  TABLE
CCC004*    ( VSTVERSSTEUERID         SMALLINT NOT NULL,
CCC004*      VSTVERSSTEUERPROZ       DECIMAL (5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSSTEUER-GESAMT.
        05  NI-VERSSTEUER-TAB.
           10 NI-VERSSTEUER-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-VERSSTEUER  REDEFINES  NI-VERSSTEUER-TAB.
           10 NI-VSTVERSSTEUERID          PIC S9(4) COMP.
           10 NI-VSTVERSSTEUERPROZ        PIC S9(4) COMP.
        05  DCL-VERSSTEUER.
           10 VSTVERSSTEUERID             PIC S9(4) COMP.
           10 VSTVERSSTEUERPROZ           PIC S9(3)V9(2) COMP-3.
      ******************************************************************
