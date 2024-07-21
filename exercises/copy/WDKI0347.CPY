      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0347.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0347.CPv  $
      *
      *   Rev 2.2   Oct 17 2005 08:57:48   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.1   Apr 16 1999 10:10:38   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:42   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSGEFAHRBEZ-GESAMT.
         10 EA-NI-VERSGEFAHRBEZ-TAB.
           20 EA-NI-VERSGEFAHRBEZ-ELE PIC X OCCURS 12 TIMES.
         10 EA-NI-VERSGEFAHRBEZ
                 REDEFINES EA-NI-VERSGEFAHRBEZ-TAB.
           20 EA-NI-VEGVERTRAGSID            PIC X.
           20 EA-NI-VEGVERSTARLFDNR          PIC X.
           20 EA-NI-VEGGEFAHRCODE            PIC X.
           20 EA-NI-VEGSELBSTBETEIL          PIC X.
           20 EA-NI-VEGSELBSTBETWKZ          PIC X.
           20 EA-NI-VEGTARIFZONE             PIC X.
           20 EA-NI-VEGSBVSUMME              PIC X.
           20 EA-NI-VEGSBSCHADEN             PIC X.
           20 EA-NI-VEGSBMIN                 PIC X.
           20 EA-NI-VEGSBMAX                 PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-VEGKZSBNL                PIC X.
         10 EA-DCL-VERSGEFAHRBEZ.
           20 EA-VEGVERTRAGSID               PIC X(13).
           20 EA-VEGVERSTARLFDNR             PIC -----9.
           20 EA-VEGGEFAHRCODE               PIC -----9.
           20 EA-VEGSELBSTBETEIL             PIC ---------9.99.
           20 EA-VEGSELBSTBETWKZ             PIC X(1).
           20 EA-VEGTARIFZONE                PIC -----9.
           20 EA-VEGSBVSUMME                 PIC --9.
           20 EA-VEGSBSCHADEN                PIC --9.
           20 EA-VEGSBMIN                    PIC -----9.
           20 EA-VEGSBMAX                    PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-VEGKZSBNL                   PIC X(1).
      *****************************************************************
