      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0025.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0025.CPv  $
      *
      *   Rev 2.2   Apr 07 2003 10:12:34   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.1   Apr 16 1999 10:04:32   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HYPOTHEK-GESAMT.
         10 EA-NI-HYPOTHEK-TAB.
           20 EA-NI-HYPOTHEK-ELE PIC X
                  OCCURS 21 TIMES.
         10 EA-NI-HYPOTHEK
                 REDEFINES EA-NI-HYPOTHEK-TAB.
           20 EA-NI-HYPPERSONENID            PIC X.
           20 EA-NI-HYPPERSONENROLLE         PIC X.
           20 EA-NI-HYPHWELFDNUMMER          PIC X.
           20 EA-NI-HYPLAUFENDENUMMER        PIC X.
           20 EA-NI-HYPBANKBLZ               PIC X.
           20 EA-NI-HYPBANKBLZTEXT           PIC X.
           20 EA-NI-HYPDARLEHENSSUMME        PIC X.
           20 EA-NI-HYPDARLSUMMEWKZ          PIC X.
           20 EA-NI-HYPFESTGELEGTBIS         PIC X.
           20 EA-NI-HYPMONBELASTUNG          PIC X.
           20 EA-NI-HYPMONBELASTWKZ          PIC X.
           20 EA-NI-HYPRESTSCHULD            PIC X.
           20 EA-NI-HYPRESTSCHULDWKZ         PIC X.
           20 EA-NI-HYPTILGUNG               PIC X.
           20 EA-NI-HYPZINSEN                PIC X.
           20 EA-NI-HYPANLAGEZIEL            PIC X.
           20 EA-NI-HYPNOTIZID               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-HYPDARLEHENSART          PIC X.
           20 EA-NI-HYPTILGUNGSBEITRAG       PIC X.
           20 EA-NI-HYPTILGUNGSBEITWKZ       PIC X.
         10 EA-DCL-HYPOTHEK.
           20 EA-HYPPERSONENID               PIC X(13).
           20 EA-HYPPERSONENROLLE            PIC X(1).
           20 EA-HYPHWELFDNUMMER             PIC ----------9.
           20 EA-HYPLAUFENDENUMMER           PIC ----------9.
           20 EA-HYPBANKBLZ                  PIC ----------9.
           20 EA-HYPBANKBLZTEXT              PIC X(30).
           20 EA-HYPDARLEHENSSUMME           PIC ---------9.99.
           20 EA-HYPDARLSUMMEWKZ             PIC X(1).
           20 EA-HYPFESTGELEGTBIS            PIC X(010).
           20 EA-HYPMONBELASTUNG             PIC ---------9.99.
           20 EA-HYPMONBELASTWKZ             PIC X(1).
           20 EA-HYPRESTSCHULD               PIC ---------9.99.
           20 EA-HYPRESTSCHULDWKZ            PIC X(1).
           20 EA-HYPTILGUNG                  PIC ---9.99.
           20 EA-HYPZINSEN                   PIC ---9.99.
           20 EA-HYPANLAGEZIEL               PIC X(50).
           20 EA-HYPNOTIZID                  PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-HYPDARLEHENSART             PIC -----9.
           20 EA-HYPTILGUNGSBEITRAG          PIC ---------9.99.
           20 EA-HYPTILGUNGSBEITWKZ          PIC X(1).
      *****************************************************************
