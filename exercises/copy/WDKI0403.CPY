      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0403.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WDKI0403.CPv  $
      *
      *   Rev 2.1   Jul 21 2005 10:28:10   t13ex27
      *AKI00505: DB-Änderung für Fix 77 EA-Programme
      *
      *   Rev 2.0   Jun 27 2005 16:22:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-RIESTERZULAGE-GESAMT.
         10 EA-NI-RIESTERZULAGE-TAB.
           20 EA-NI-RIESTERZULAGE-ELE PIC X OCCURS 6 TIMES.
         10 EA-NI-RIESTERZULAGE
                 REDEFINES EA-NI-RIESTERZULAGE-TAB.
           20 EA-NI-RZLVERTRAGSID            PIC X.
           20 EA-NI-RZLZULAGENJAHR           PIC X.
           20 EA-NI-RZLDATUMZULAGE           PIC X.
           20 EA-NI-RZLZULAGEBETRAG          PIC X.
           20 EA-NI-RZLZULAGEBETRAGWKZ       PIC X.
           20 EA-NI-RZLZULAGENART            PIC X.
         10 EA-DCL-RIESTERZULAGE.
           20 EA-RZLVERTRAGSID               PIC X(13).
           20 EA-RZLZULAGENJAHR              PIC -----9.
           20 EA-RZLDATUMZULAGE              PIC X(010).
           20 EA-RZLZULAGEBETRAG             PIC ---------9.99.
           20 EA-RZLZULAGEBETRAGWKZ          PIC X(1).
           20 EA-RZLZULAGENART               PIC -----9.
      *****************************************************************
