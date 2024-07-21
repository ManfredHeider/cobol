      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0357.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WDKI0357.CPv  $
      *
      *   Rev 2.4   Apr 04 2005 08:39:20   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.3   Oct 18 2004 09:43:56   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.2   May 24 2004 10:54:08   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.1   Apr 16 1999 10:10:58   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:46   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAG-GESAMT.
         10 EA-NI-VERTRAG-TAB.
           20 EA-NI-VERTRAG-ELE PIC X OCCURS 32 TIMES.
         10 EA-NI-VERTRAG
                 REDEFINES EA-NI-VERTRAG-TAB.
           20 EA-NI-VERVERTRAGSID            PIC X.
           20 EA-NI-VERFRAPERSONENID         PIC X.
           20 EA-NI-VERFRALFDNR              PIC X.
           20 EA-NI-VERPARTGESELLSCHID       PIC X.
           20 EA-NI-VERFREMDVERTRAGKZ        PIC X.
           20 EA-NI-VERVERTRAGSNUMMER        PIC X.
           20 EA-NI-VERZAHLUNGSARTID         PIC X.
           20 EA-NI-VERZAHLUNGSWEISEID       PIC X.
           20 EA-NI-VERVERTRZUSTANDID        PIC X.
           20 EA-NI-VERVERTRZUSTANDDAT       PIC X.
           20 EA-NI-VERABSCHLUSSGRUND        PIC X.
           20 EA-NI-VERDATVERTRAEND          PIC X.
           20 EA-NI-VERREMDVERTRGESNAM       PIC X.
           20 EA-NI-VERGESAMTBEITRAG         PIC X.
           20 EA-NI-VERGESBEITRWKZ           PIC X.
           20 EA-NI-VERFREISTAUFTRAG         PIC X.
           20 EA-NI-VERINTERNEVERTRNR        PIC X.
           20 EA-NI-VERPRUEFZIFFVNR          PIC X.
           20 EA-NI-VERMONATDERHAUPTF        PIC X.
           20 EA-NI-VERNACHLASSKZ            PIC X.
           20 EA-NI-VERGEBUENDELTKZ          PIC X.
           20 EA-NI-VERSTORNODATUM           PIC X.
           20 EA-NI-VERAENDERUNGSDATUM       PIC X.
           20 EA-NI-VERVERTRAGSABLAUF        PIC X.
           20 EA-NI-VERVERTRAGSBEGINN        PIC X.
           20 EA-NI-VERNOTIZID               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-VERABSCHLUSSVBNR         PIC X.
           20 EA-NI-VEREIGENVERMKZ           PIC X.
           20 EA-NI-VERVSNRZUSATZ            PIC X.
           20 EA-NI-VERDATPRODLOESCH         PIC X.
           20 EA-NI-VEREHELEUTEKZ            PIC X.
         10 EA-DCL-VERTRAG.
           20 EA-VERVERTRAGSID               PIC X(13).
           20 EA-VERFRAPERSONENID            PIC X(13).
           20 EA-VERFRALFDNR                 PIC -----9.
           20 EA-VERPARTGESELLSCHID          PIC -----9.
           20 EA-VERFREMDVERTRAGKZ           PIC X(1).
           20 EA-VERVERTRAGSNUMMER           PIC X(15).
           20 EA-VERZAHLUNGSARTID            PIC -----9.
           20 EA-VERZAHLUNGSWEISEID          PIC -----9.
           20 EA-VERVERTRZUSTANDID           PIC -----9.
           20 EA-VERVERTRZUSTANDDAT          PIC X(010).
           20 EA-VERABSCHLUSSGRUND.
              49 EA-VERABSCHLUSSGRUND-LEN    PIC -----9.
              49 EA-VERABSCHLUSSGRUND-TEXT   PIC X(30).
           20 EA-VERDATVERTRAEND             PIC X(010).
           20 EA-VERREMDVERTRGESNAM.
              49 EA-VERREMDVERTRGESNAM-LEN   PIC -----9.
              49 EA-VERREMDVERTRGESNAM-TEXT  PIC X(25).
           20 EA-VERGESAMTBEITRAG            PIC ---------9.99.
           20 EA-VERGESBEITRWKZ              PIC X(1).
           20 EA-VERFREISTAUFTRAG            PIC X(1).
           20 EA-VERINTERNEVERTRNR           PIC ----------9.
           20 EA-VERPRUEFZIFFVNR             PIC -----9.
           20 EA-VERMONATDERHAUPTF           PIC -----9.
           20 EA-VERNACHLASSKZ               PIC X(1).
           20 EA-VERGEBUENDELTKZ             PIC X(1).
           20 EA-VERSTORNODATUM              PIC X(010).
           20 EA-VERAENDERUNGSDATUM          PIC X(026).
           20 EA-VERVERTRAGSABLAUF           PIC X(010).
           20 EA-VERVERTRAGSBEGINN           PIC X(010).
           20 EA-VERNOTIZID                  PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-VERABSCHLUSSVBNR            PIC ----------9.
           20 EA-VEREIGENVERMKZ              PIC X(1).
           20 EA-VERVSNRZUSATZ               PIC X(10).
           20 EA-VERDATPRODLOESCH            PIC X(010).
           20 EA-VEREHELEUTEKZ               PIC X(1).
      *****************************************************************
