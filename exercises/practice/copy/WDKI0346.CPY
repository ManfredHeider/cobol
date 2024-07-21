      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0346.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0346.CPv  $
      *
      *   Rev 2.2   Apr 04 2005 08:39:14   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.1   Apr 16 1999 10:10:34   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSLEBENTARIF-GESAMT.
         10 EA-NI-VERSLEBENTARIF-TAB.
           20 EA-NI-VERSLEBENTARIF-ELE PIC X OCCURS 11 TIMES.
         10 EA-NI-VERSLEBENTARIF
                 REDEFINES EA-NI-VERSLEBENTARIF-TAB.
           20 EA-NI-LTAVERTRAGSID            PIC X.
           20 EA-NI-LTAVERSTARLFDNR          PIC X.
           20 EA-NI-LTARUMSGRUPPE            PIC X.
           20 EA-NI-LTARENTENZAHLWID         PIC X.
           20 EA-NI-LTARENTENGARZEIT         PIC X.
           20 EA-NI-LTADYNLTGRV              PIC X.
           20 EA-NI-LTADIREKTVERSID          PIC X.
           20 EA-NI-LTALVART                 PIC X.
           20 EA-NI-LTAKZKAPWRECHT           PIC X.
           20 EA-NI-LTASUBTYPE               PIC X.
           20 EA-NI-LTAVERFUEGUNGID          PIC X.
         10 EA-DCL-VERSLEBENTARIF.
           20 EA-LTAVERTRAGSID               PIC X(13).
           20 EA-LTAVERSTARLFDNR             PIC -----9.
           20 EA-LTARUMSGRUPPE               PIC X(6).
           20 EA-LTARENTENZAHLWID            PIC -----9.
           20 EA-LTARENTENGARZEIT            PIC X(010).
           20 EA-LTADYNLTGRV                 PIC X(1).
           20 EA-LTADIREKTVERSID             PIC X(1).
           20 EA-LTALVART                    PIC -----9.
           20 EA-LTAKZKAPWRECHT              PIC X(1).
           20 EA-LTASUBTYPE                  PIC X(1).
           20 EA-LTAVERFUEGUNGID             PIC X(3).
      *****************************************************************
