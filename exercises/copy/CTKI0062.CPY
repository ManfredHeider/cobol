      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0062.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0062.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:24   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:55:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KUNDENSTATUS  TABLE
CCC004*    ( KUNDENSTATUSID          CHAR NOT NULL,
CCC004*      KUNDENSTATUSTEXT        CHAR (15) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KUNDENSTATUS-GESAMT.
        05  NI-KUNDENSTATUS-TAB.
           10 NI-KUNDENSTATUS-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-KUNDENSTATUS  REDEFINES  NI-KUNDENSTATUS-TAB.
           10 NI-KUNDENSTATUSID           PIC S9(4) COMP.
           10 NI-KUNDENSTATUSTEXT         PIC S9(4) COMP.
        05  DCL-KUNDENSTATUS.
           10 KUNDENSTATUSID              PIC X(01).
           10 KUNDENSTATUSTEXT            PIC X(15).
      ******************************************************************
