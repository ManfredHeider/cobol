      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  13.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0396.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0396.CPv  $
      *
      *   Rev 2.1   Apr 07 2003 10:12:48   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.0   Mar 13 2003 08:25:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-BILANZSCHLUESSEL-GESAMT.
         10 EA-NI-BILANZSCHLUESSEL-TAB.
           20 EA-NI-BILANZSCHLUESSEL-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-BILANZSCHLUESSEL
                 REDEFINES EA-NI-BILANZSCHLUESSEL-TAB.
           20 EA-NI-BILANZSCHL               PIC X.
           20 EA-NI-BILANZSCHLTEXT           PIC X.
         10 EA-DCL-BILANZSCHLUESSEL.
           20 EA-BILANZSCHL                  PIC -----9.
           20 EA-BILANZSCHLTEXT              PIC X(30).
      *****************************************************************
