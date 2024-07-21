      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0374.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0374.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:30:24   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:08:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BEIHILFEBEZ  TABLE
CCC004*    ( BEIHILFESTATUSID SMALLINT NOT NULL,
CCC004*      BEIHILFEVORSCHRID SMALLINT NOT NULL,
CCC004*      BHBEZSATZAMBULANT DECIMAL(5, 2),
CCC004*      BHBEZSATZSTATION DECIMAL(5, 2),
CCC004*      BHBEZSATZZAHN DECIMAL(5, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BEIHILFEBEZ-GESAMT.
        05  NI-BEIHILFEBEZ-TAB.
           10 NI-BEIHILFEBEZ-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-BEIHILFEBEZ  REDEFINES  NI-BEIHILFEBEZ-TAB.
           10 NI-BEIHILFESTATUSID         PIC S9(4) COMP.
           10 NI-BEIHILFEVORSCHRID        PIC S9(4) COMP.
           10 NI-BHBEZSATZAMBULANT        PIC S9(4) COMP.
           10 NI-BHBEZSATZSTATION         PIC S9(4) COMP.
           10 NI-BHBEZSATZZAHN            PIC S9(4) COMP.
        05  DCL-BEIHILFEBEZ.
           10 BEIHILFESTATUSID            PIC S9(4) COMP.
           10 BEIHILFEVORSCHRID           PIC S9(4) COMP.
           10 BHBEZSATZAMBULANT           PIC S9(3)V9(2) COMP-3.
           10 BHBEZSATZSTATION            PIC S9(3)V9(2) COMP-3.
           10 BHBEZSATZZAHN               PIC S9(3)V9(2) COMP-3.
      ******************************************************************
