      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWKI2357.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DWKI2357.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 09:57:14   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 14:29:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
       03  ACTION-CODE                    PIC X(1).
       03  FUNCTION-CODE                  PIC X(9).
       03  VERTRAG-GESAMT.
        05  NI-VERTRAG-TAB.
           10 NI-VERTRAG-ELE PIC S9(04) COMP OCCURS 29 TIMES.
        05  NI-VERTRAG  REDEFINES  NI-VERTRAG-TAB.
           10 NI-VERVERTRAGSID            PIC S9(4) COMP.
           10 NI-VERFRAPERSONENID         PIC S9(4) COMP.
           10 NI-VERFRALFDNR              PIC S9(4) COMP.
           10 NI-VERPARTGESELLSCHID       PIC S9(4) COMP.
           10 NI-VERFREMDVERTRAGKZ        PIC S9(4) COMP.
           10 NI-VERVERTRAGSNUMMER        PIC S9(4) COMP.
           10 NI-VERZAHLUNGSARTID         PIC S9(4) COMP.
           10 NI-VERZAHLUNGSWEISEID       PIC S9(4) COMP.
           10 NI-VERVERTRZUSTANDID        PIC S9(4) COMP.
           10 NI-VERVERTRZUSTANDDAT       PIC S9(4) COMP.
           10 NI-VERABSCHLUSSGRUND        PIC S9(4) COMP.
           10 NI-VERDATVERTRAEND          PIC S9(4) COMP.
           10 NI-VERREMDVERTRGESNAM       PIC S9(4) COMP.
           10 NI-VERGESAMTBEITRAG         PIC S9(4) COMP.
           10 NI-VERGESBEITRWKZ           PIC S9(4) COMP.
           10 NI-VERFREISTAUFTRAG         PIC S9(4) COMP.
           10 NI-VERINTERNEVERTRNR        PIC S9(4) COMP.
           10 NI-VERPRUEFZIFFVNR          PIC S9(4) COMP.
           10 NI-VERMONATDERHAUPTF        PIC S9(4) COMP.
           10 NI-VERNACHLASSKZ            PIC S9(4) COMP.
           10 NI-VERGEBUENDELTKZ          PIC S9(4) COMP.
           10 NI-VERSTORNODATUM           PIC S9(4) COMP.
           10 NI-VERAENDERUNGSDATUM       PIC S9(4) COMP.
           10 NI-VERVERTRAGSABLAUF        PIC S9(4) COMP.
           10 NI-VERVERTRAGSBEGINN        PIC S9(4) COMP.
           10 NI-VERNOTIZID               PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
           10 NI-VERABSCHLUSSVBNR         PIC S9(4) COMP.
           10 NI-VEREIGENVERMKZ           PIC S9(4) COMP.
        05  DCL-VERTRAG.
           10 VERVERTRAGSID               PIC X(13).
           10 VERFRAPERSONENID            PIC X(13).
           10 VERFRALFDNR                 PIC S9(4) COMP.
           10 VERPARTGESELLSCHID          PIC S9(4) COMP.
           10 VERFREMDVERTRAGKZ           PIC X(01).
           10 VERVERTRAGSNUMMER           PIC X(15).
           10 VERZAHLUNGSARTID            PIC S9(4) COMP.
           10 VERZAHLUNGSWEISEID          PIC S9(4) COMP.
           10 VERVERTRZUSTANDID           PIC S9(4) COMP.
           10 VERVERTRZUSTANDDAT          PIC X(10).
           10 VERABSCHLUSSGRUND.
              49 VERABSCHLUSSGRUND-LEN    PIC S9(4) COMP.
              49 VERABSCHLUSSGRUND-TEXT   PIC X(30).
           10 VERDATVERTRAEND             PIC X(10).
           10 VERREMDVERTRGESNAM.
              49 VERREMDVERTRGESNAM-LEN   PIC S9(4) COMP.
              49 VERREMDVERTRGESNAM-TEXT  PIC X(25).
           10 VERGESAMTBEITRAG            PIC S9(9)V9(2) COMP-3.
           10 VERGESBEITRWKZ              PIC X(01).
           10 VERFREISTAUFTRAG            PIC X(01).
           10 VERINTERNEVERTRNR           PIC S9(9) COMP.
           10 VERPRUEFZIFFVNR             PIC S9(4) COMP.
           10 VERMONATDERHAUPTF           PIC S9(4) COMP.
           10 VERNACHLASSKZ               PIC X(01).
           10 VERGEBUENDELTKZ             PIC X(01).
           10 VERSTORNODATUM              PIC X(10).
           10 VERAENDERUNGSDATUM          PIC X(26).
           10 VERVERTRAGSABLAUF           PIC X(10).
           10 VERVERTRAGSBEGINN           PIC X(10).
           10 VERNOTIZID                  PIC X(13).
           10 LETZTEAENDERUNG             PIC X(26).
           10 VERABSCHLUSSVBNR            PIC S9(9) COMP.
           10 VEREIGENVERMKZ              PIC X(01).
      ******************************************************************
