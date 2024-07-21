      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0334.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0334.CPv  $
      *
      *   Rev 2.2   Feb 27 2012 13:24:16   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 2.1   Jun 10 2002 09:50:26   T13EX27
      *AKI00289 -  DB-Änderun für neue 'VP-Analyse'
      *
      *   Rev 2.0   Feb 27 1999 12:50:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:29:18   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE RUECKNAHMEPREIS TABLE
CCC004*    ( RNPFONDID                      SMALLINT NOT NULL,
CCC004*      RNPAUSGABEBETRAG               DECIMAL(9, 2),
CCC004*      RNPDATUMFONDSTAND              DATE,
CCC004*      RNPRUECKNAHMEBETR              DECIMAL(9, 2),
CCC004*      RNPDATUMWERTENTW               DATE,
CCC004*      RNPWERTENTWICKLUNG             DECIMAL(7, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE RUECKNAHMEPREIS             *
      ******************************************************************
       01  RUECKNAHMEPREIS-GESAMT.
        05 NI-RUECKNAHMEPREIS-TAB.
           10 NI-RUECKNAHMEPREIS-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05 NI-RUECKNAHMEPREIS REDEFINES NI-RUECKNAHMEPREIS-TAB.
           10 NI-RNPFONDID          PIC S9(4) COMP.
           10 NI-RNPAUSGABEBETRAG   PIC S9(4) COMP.
           10 NI-RNPDATUMFONDSTAND  PIC S9(4) COMP.
           10 NI-RNPRUECKNAHMEBETR  PIC S9(4) COMP.
           10 NI-RNPDATUMWERTENTW   PIC S9(4) COMP.
           10 NI-RNPWERTENTWICKLUNG PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-RUECKNAHMEPREIS.
           10 RNPFONDID            PIC S9(4) USAGE COMP.
           10 RNPAUSGABEBETRAG     PIC S9(7)V9(2) USAGE COMP-3.
           10 RNPDATUMFONDSTAND    PIC X(10).
           10 RNPRUECKNAHMEBETR    PIC S9(7)V9(2) USAGE COMP-3.
           10 RNPDATUMWERTENTW     PIC X(10).
           10 RNPWERTENTWICKLUNG   PIC S9(5)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
