      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0076.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0076.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:06   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   16 Jan 1998 14:16:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSNAMENAENDERUNG  TABLE
CCC004*    ( PNAMITARBEITERID CHAR (13) NOT NULL,
CCC004*      PNAMITARBPERSROLLE CHAR NOT NULL,
CCC004*      PNAPERSONENID CHAR (13) NOT NULL,
CCC004*      PNAAENDERUNGSGRUND SMALLINT NOT NULL,
CCC004*      PNALAUFENDENUM SMALLINT NOT NULL,
CCC004*      PNAGEAENDERTAM DATE,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSNAMENAENDERUNG-GESAMT.
        05  NI-PERSNAMENAENDERUNG-TAB.
           10 NI-PERSNAMENAENDERUNG-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05  NI-PERSNAMENAENDERUNG  REDEFINES  NI-PERSNAMENAENDERUNG-TAB.
           10 NI-PNAMITARBEITERID         PIC S9(4) COMP.
           10 NI-PNAMITARBPERSROLLE       PIC S9(4) COMP.
           10 NI-PNAPERSONENID            PIC S9(4) COMP.
           10 NI-PNAAENDERUNGSGRUND       PIC S9(4) COMP.
           10 NI-PNALAUFENDENUM           PIC S9(4) COMP.
           10 NI-PNAGEAENDERTAM           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-PERSNAMENAENDERUNG.
           10 PNAMITARBEITERID            PIC X(13).
           10 PNAMITARBPERSROLLE          PIC X(01).
           10 PNAPERSONENID               PIC X(13).
           10 PNAAENDERUNGSGRUND          PIC S9(4) COMP.
           10 PNALAUFENDENUM              PIC S9(4) COMP.
           10 PNAGEAENDERTAM              PIC X(10).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
