      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0028.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0028.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:36   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KAUFBAUWUNSCH-GESAMT.
         10 EA-NI-KAUFBAUWUNSCH-TAB.
           20 EA-NI-KAUFBAUWUNSCH-ELE PIC X
                  OCCURS 18 TIMES.
         10 EA-NI-KAUFBAUWUNSCH
                 REDEFINES EA-NI-KAUFBAUWUNSCH-TAB.
           20 EA-NI-KBWPERSONENID            PIC X.
           20 EA-NI-KBWPERSONENROLLE         PIC X.
           20 EA-NI-KBWLAUFENDENUMMER        PIC X.
           20 EA-NI-KBWEIGENTUMSARTID        PIC X.
           20 EA-NI-KBWERWMITTEL             PIC X.
           20 EA-NI-KBWERWMITTELWKZ          PIC X.
           20 EA-NI-KBWERWMITTELIMJ          PIC X.
           20 EA-NI-KBWGRUNDSTWERT           PIC X.
           20 EA-NI-KBWGRUNDSTWWKZ           PIC X.
           20 EA-NI-KBWGRUNDSTVORH           PIC X.
           20 EA-NI-KBWKAUFBAUDATUM          PIC X.
           20 EA-NI-KBWMONBELGRUNDST         PIC X.
           20 EA-NI-KBWMONBELGRSTWKZ         PIC X.
           20 EA-NI-KBWMONBELOBJECK          PIC X.
           20 EA-NI-KBWMONBELOBJWKZ          PIC X.
           20 EA-NI-KBWOBJEKTWERT            PIC X.
           20 EA-NI-KBWOBJEKTWERTWKZ         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KAUFBAUWUNSCH.
           20 EA-KBWPERSONENID               PIC X(13).
           20 EA-KBWPERSONENROLLE            PIC X(01).
           20 EA-KBWLAUFENDENUMMER           PIC --------9 .
           20 EA-KBWEIGENTUMSARTID           PIC X(3).
           20 EA-KBWERWMITTEL                PIC ---------9.99    .
           20 EA-KBWERWMITTELWKZ             PIC X(01).
           20 EA-KBWERWMITTELIMJ             PIC X(10).
           20 EA-KBWGRUNDSTWERT              PIC ---------9.99    .
           20 EA-KBWGRUNDSTWWKZ              PIC X(01).
           20 EA-KBWGRUNDSTVORH              PIC X(01).
           20 EA-KBWKAUFBAUDATUM             PIC X(10).
           20 EA-KBWMONBELGRUNDST            PIC ---------9.99    .
           20 EA-KBWMONBELGRSTWKZ            PIC X(01).
           20 EA-KBWMONBELOBJECK             PIC ---------9.99    .
           20 EA-KBWMONBELOBJWKZ             PIC X(01).
           20 EA-KBWOBJEKTWERT               PIC ---------9.99    .
           20 EA-KBWOBJEKTWERTWKZ            PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
