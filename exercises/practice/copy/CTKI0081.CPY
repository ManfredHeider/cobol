      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0081.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0081.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Jan 1999 11:33:36   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   21 Jul 1998 16:14:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.100 19980721'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  DIREKTIONAENDERUNG   TABLE
CCC004*    ( DIRAENDID CHAR(13) NOT NULL,
CCC004*      DIRAENDPERSONENID CHAR(13) NOT NULL,
CCC004*      DIRAENDPERSROLLE CHAR(1) NOT NULL,
CCC004*      DIRAENDDTAERLEDIGT CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  DIREKTIONAENDERUNG-GESAMT.
        05  NI-DIREKTIONAENDERUNG-TAB.
           10 NI-DIREKTIONAENDERUNG-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-DIREKTIONAENDERUNG  REDEFINES  NI-DIREKTIONAENDERUNG-TAB.
           10 NI-DIRAENDID                PIC S9(4) COMP.
           10 NI-DIRAENDPERSONENID        PIC S9(4) COMP.
           10 NI-DIRAENDPERSROLLE         PIC S9(4) COMP.
           10 NI-DIRAENDDTAERLEDIGT       PIC S9(4) COMP.
        05  DCL-DIREKTIONAENDERUNG.
           10 DIRAENDID                   PIC X(13).
           10 DIRAENDPERSONENID           PIC X(13).
           10 DIRAENDPERSROLLE            PIC X(1).
           10 DIRAENDDTAERLEDIGT          PIC X(1).
      ******************************************************************
