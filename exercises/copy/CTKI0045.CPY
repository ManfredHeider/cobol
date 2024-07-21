      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0045.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CTKI0045.CPv  $
      *
      *   Rev 2.1   04 Sep 2001 14:15:58   t13ex27
      *AKI00219
      *
      *   Rev 2.0   Feb 27 1999 12:49:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:26   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:54:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSPERSBEZIEHUNG  TABLE
CCC004*    ( PPBEZPERSONENID1 CHAR (13) NOT NULL,
CCC004*      PPBEZPERSONENID2 CHAR (13) NOT NULL,
CCC004*      PPBEZBEZART SMALLINT NOT NULL,
CCC004*      PPBEZINVBEZART SMALLINT NOT NULL,
CCC004*      PPBEZGUELTIGVON DATE,
CCC004*      PPBEZGUELTIGBIS DATE,
CCC004*      LETZTEAENDERUNG TIMESTAMP,
CCC004*      PPBEZWERT DECIMAL (8, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSPERSBEZIEHUNG-GESAMT.
        05  NI-PERSPERSBEZIEHUNG-TAB.
           10 NI-PERSPERSBEZIEHUNG-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-PERSPERSBEZIEHUNG  REDEFINES  NI-PERSPERSBEZIEHUNG-TAB.
           10 NI-PPBEZPERSONENID1         PIC S9(4) COMP.
           10 NI-PPBEZPERSONENID2         PIC S9(4) COMP.
           10 NI-PPBEZBEZART              PIC S9(4) COMP.
           10 NI-PPBEZINVBEZART           PIC S9(4) COMP.
           10 NI-PPBEZGUELTIGVON          PIC S9(4) COMP.
           10 NI-PPBEZGUELTIGBIS          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
           10 NI-PPBEZWERT                PIC S9(4) COMP.
        05  DCL-PERSPERSBEZIEHUNG.
           10 PPBEZPERSONENID1            PIC X(13).
           10 PPBEZPERSONENID2            PIC X(13).
           10 PPBEZBEZART                 PIC S9(4) COMP.
           10 PPBEZINVBEZART              PIC S9(4) COMP.
           10 PPBEZGUELTIGVON             PIC X(10).
           10 PPBEZGUELTIGBIS             PIC X(10).
           10 LETZTEAENDERUNG             PIC X(26).
           10 PPBEZWERT                   PIC S9(6)V9(2) COMP-3.
      ******************************************************************
