      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0392.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0392.CPv  $
      *
      *   Rev 2.2   Feb 03 2012 15:16:42   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.1   Oct 15 2002 15:35:42   t13ex27
      *AKI00308 DB-Änderung für Fixpack 27
      *
      *   Rev 2.0   15 May 2001 13:47:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE RISIKOKLASSE                                      *
      ******************************************************************
CCC005*    EXEC SQL DECLARE UNFBEITRAGRUECK TABLE
CCC004*    ( UBRVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      UBRVETLFDNR                    SMALLINT NOT NULL,
CCC004*      UBRRGID                        SMALLINT NOT NULL,
CCC004*      UBRJAHR                        SMALLINT NOT NULL,
CCC004*      UBRMONAT                       SMALLINT NOT NULL,
CCC004*      UBRMONNR                       SMALLINT,
CCC004*      UBRLFDMON                      SMALLINT,
CCC004*      UBRSTATUS                      CHAR(1),
CCC004*      UBRERSTDATUM                   DATE,
CCC004*      UBRGARRUEWERT                  DECIMAL(15, 4),
CCC004*      UBRGARTODLEIST                 DECIMAL(15, 4),
CCC004*      UBRRUEWERTUEB                  DECIMAL(15, 4),
CCC004*      UBRTODLEIUEB                   DECIMAL(15, 4),
CCC004*      UBRSTILLERES                   DECIMAL(15, 4),
CCC004*      UBRRECHZINS                    DECIMAL(4, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE UNFBEITRAGRUECK             *
      ******************************************************************
       01  UNFBEITRAGRUECK-GESAMT.
        05 NI-UNFBEITRAGRUECK-TAB.
           10 NI-UNFBEITRAGRUECK-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-UNFBEITRAGRUECK REDEFINES NI-UNFBEITRAGRUECK-TAB.
           10 NI-UBRVERTRAGSID      PIC S9(4) COMP.
           10 NI-UBRVETLFDNR        PIC S9(4) COMP.
           10 NI-UBRRGID            PIC S9(4) COMP.
           10 NI-UBRJAHR            PIC S9(4) COMP.
           10 NI-UBRMONAT           PIC S9(4) COMP.
           10 NI-UBRMONNR           PIC S9(4) COMP.
           10 NI-UBRLFDMON          PIC S9(4) COMP.
           10 NI-UBRSTATUS          PIC S9(4) COMP.
           10 NI-UBRERSTDATUM       PIC S9(4) COMP.
           10 NI-UBRGARRUEWERT      PIC S9(4) COMP.
           10 NI-UBRGARTODLEIST     PIC S9(4) COMP.
           10 NI-UBRRUEWERTUEB      PIC S9(4) COMP.
           10 NI-UBRTODLEIUEB       PIC S9(4) COMP.
           10 NI-UBRSTILLERES       PIC S9(4) COMP.
           10 NI-UBRRECHZINS        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-UNFBEITRAGRUECK.
           10 UBRVERTRAGSID        PIC X(13).
           10 UBRVETLFDNR          PIC S9(4) USAGE COMP.
           10 UBRRGID              PIC S9(4) USAGE COMP.
           10 UBRJAHR              PIC S9(4) USAGE COMP.
           10 UBRMONAT             PIC S9(4) USAGE COMP.
           10 UBRMONNR             PIC S9(4) USAGE COMP.
           10 UBRLFDMON            PIC S9(4) USAGE COMP.
           10 UBRSTATUS            PIC X(1).
           10 UBRERSTDATUM         PIC X(10).
           10 UBRGARRUEWERT        PIC S9(11)V9(4) USAGE COMP-3.
           10 UBRGARTODLEIST       PIC S9(11)V9(4) USAGE COMP-3.
           10 UBRRUEWERTUEB        PIC S9(11)V9(4) USAGE COMP-3.
           10 UBRTODLEIUEB         PIC S9(11)V9(4) USAGE COMP-3.
           10 UBRSTILLERES         PIC S9(11)V9(4) USAGE COMP-3.
           10 UBRRECHZINS          PIC S9(2)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
