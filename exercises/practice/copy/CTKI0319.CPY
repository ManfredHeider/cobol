      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0319.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0319.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:52   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KFZDECKSCHLUESSEL  TABLE
CCC004*    ( KDSDECKSCHLHPFLID       CHAR (2) NOT NULL,
CCC004*      KDSDECKSCHLTEXT         CHAR (40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KFZDECKSCHLUESSEL-GESAMT.
        05  NI-KFZDECKSCHLUESSEL-TAB.
           10 NI-KFZDECKSCHLUESSEL-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-KFZDECKSCHLUESSEL  REDEFINES  NI-KFZDECKSCHLUESSEL-TAB.
           10 NI-KDSDECKSCHLHPFLID        PIC S9(4) COMP.
           10 NI-KDSDECKSCHLTEXT          PIC S9(4) COMP.
        05  DCL-KFZDECKSCHLUESSEL.
           10 KDSDECKSCHLHPFLID           PIC X(2).
           10 KDSDECKSCHLTEXT             PIC X(40).
      ******************************************************************
