      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  13.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0397.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0397.CPv  $
      *
      *   Rev 2.1   Apr 07 2003 10:12:50   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.0   Mar 13 2003 08:25:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-FILIALNRBLZ-GESAMT.
         10 EA-NI-FILIALNRBLZ-TAB.
           20 EA-NI-FILIALNRBLZ-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-FILIALNRBLZ
                 REDEFINES EA-NI-FILIALNRBLZ-TAB.
           20 EA-NI-FILIALNUMMER             PIC X.
           20 EA-NI-FILIALBLZ                PIC X.
           20 EA-NI-FILIALNAME               PIC X.
         10 EA-DCL-FILIALNRBLZ.
           20 EA-FILIALNUMMER                PIC -----9.
           20 EA-FILIALBLZ                   PIC --------9.
           20 EA-FILIALNAME                  PIC X(30).
      *****************************************************************
