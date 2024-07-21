      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0338.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0338.CPv  $
      *
      *   Rev 2.1   Jun 02 2010 09:48:18   t13ex14
      *AKI00970: DB-Änderungen für Rel. 5.12: Badenia
      *
      *   Rev 2.0   Feb 27 1999 12:50:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:24   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  UMSATZ  TABLE
CCC004*    ( UMSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      UMSGUTHABPRODLFDNR             SMALLINT NOT NULL,
CCC004*      UMSLAUFENDENR                  SMALLINT NOT NULL,
CCC004*      UMSBUCHUNGSDATUM               DATE,
CCC004*      UMSUMSATZBETRAG                DECIMAL(11, 2),
CCC004*      UMSUMSBETRWKZ                  CHAR(1) NOT NULL,
CCC004*      UMSTEXTID                      SMALLINT NOT NULL,
CCC004*      UMSZAHLUNGSWEISEID             SMALLINT,
CCC004*      UMSTEXT                        VARCHAR(20),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      UMSVWLUMSATZKZ                 CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  UMSATZ-GESAMT.
        05  NI-UMSATZ-TAB.
           10 NI-UMSATZ-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  NI-UMSATZ  REDEFINES  NI-UMSATZ-TAB.
           10 NI-UMSVERTRAGSID            PIC S9(4) COMP.
           10 NI-UMSGUTHABPRODLFDNR       PIC S9(4) COMP.
           10 NI-UMSLAUFENDENR            PIC S9(4) COMP.
           10 NI-UMSBUCHUNGSDATUM         PIC S9(4) COMP.
           10 NI-UMSUMSATZBETRAG          PIC S9(4) COMP.
           10 NI-UMSUMSBETRWKZ            PIC S9(4) COMP.
           10 NI-UMSTEXTID                PIC S9(4) COMP.
           10 NI-UMSZAHLUNGSWEISEID       PIC S9(4) COMP.
           10 NI-UMSTEXT                  PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
           10 NI-UMSVWLUMSATZKZ           PIC S9(4) COMP.
        05  DCL-UMSATZ.
           10 UMSVERTRAGSID               PIC X(13).
           10 UMSGUTHABPRODLFDNR          PIC S9(4) COMP.
           10 UMSLAUFENDENR               PIC S9(4) COMP.
           10 UMSBUCHUNGSDATUM            PIC X(10).
           10 UMSUMSATZBETRAG             PIC S9(9)V9(2) COMP-3.
           10 UMSUMSBETRWKZ               PIC X(01).
           10 UMSTEXTID                   PIC S9(4) COMP.
           10 UMSZAHLUNGSWEISEID          PIC S9(4) COMP.
           10 UMSTEXT.
              49 UMSTEXT-LEN              PIC S9(4) COMP.
              49 UMSTEXT-TEXT             PIC X(20).
           10 LETZTEAENDERUNG             PIC X(26).
           10 UMSVWLUMSATZKZ              PIC X(1).
      ******************************************************************
