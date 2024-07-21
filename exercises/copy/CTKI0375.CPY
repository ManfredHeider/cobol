      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0375.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0375.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:21:06   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:08:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BEIHILFESTATUS  TABLE
CCC004*    ( BEIHILFESTATUSID        SMALLINT NOT NULL,
CCC004*      BEIHILFESTATUSTEXT      CHAR (50) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BEIHILFESTATUS-GESAMT.
        05  NI-BEIHILFESTATUS-TAB.
           10 NI-BEIHILFESTATUS-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BEIHILFESTATUS  REDEFINES  NI-BEIHILFESTATUS-TAB.
           10 NI-BEIHILFESTATUSID         PIC S9(4) COMP.
           10 NI-BEIHILFESTATUSTEXT       PIC S9(4) COMP.
        05  DCL-BEIHILFESTATUS.
           10 BEIHILFESTATUSID            PIC S9(4) COMP.
           10 BEIHILFESTATUSTEXT          PIC X(50).
      ******************************************************************
