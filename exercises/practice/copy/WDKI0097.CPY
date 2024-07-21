      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  24.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0097.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0097.CPv  $
      *
      *   Rev 2.1   Apr 07 2003 10:12:42   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.0   Mar 24 2003 11:15:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-DARLEHENSART-GESAMT.
         10 EA-NI-DARLEHENSART-TAB.
           20 EA-NI-DARLEHENSART-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-DARLEHENSART
                 REDEFINES EA-NI-DARLEHENSART-TAB.
           20 EA-NI-DARLEHENSARTID           PIC X.
           20 EA-NI-DARLEHENSARTTEXT         PIC X.
         10 EA-DCL-DARLEHENSART.
           20 EA-DARLEHENSARTID              PIC -----9.
           20 EA-DARLEHENSARTTEXT            PIC X(25).
      *****************************************************************
