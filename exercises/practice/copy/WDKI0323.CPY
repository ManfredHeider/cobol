      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0323.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0323.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:07:38   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:32   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KFZMOPEDVERS-GESAMT.
         10 EA-NI-KFZMOPEDVERS-TAB.
           20 EA-NI-KFZMOPEDVERS-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-KFZMOPEDVERS
                 REDEFINES EA-NI-KFZMOPEDVERS-TAB.
           20 EA-NI-KFMVERTRAGSID            PIC X.
           20 EA-NI-KFMVERSTARLFDNR          PIC X.
           20 EA-NI-KFMBEITRSATZHAFTPF       PIC X.
           20 EA-NI-KFMDECKSCHLHPFLID        PIC X.
           20 EA-NI-KFMDECKUMFANG            PIC X.
           20 EA-NI-KFMHAFTPFLBEITRAG        PIC X.
           20 EA-NI-KFMHPFBEITRAGWKZ         PIC X.
           20 EA-NI-KFMHAFTPFLREGIOKL        PIC X.
           20 EA-NI-KFMHAFTPFLSCHADFID       PIC X.
           20 EA-NI-KFMTARBEZEICHNUNG        PIC X.
         10 EA-DCL-KFZMOPEDVERS.
           20 EA-KFMVERTRAGSID               PIC X(13).
           20 EA-KFMVERSTARLFDNR             PIC -----9    .
           20 EA-KFMBEITRSATZHAFTPF          PIC ---9.99          .
           20 EA-KFMDECKSCHLHPFLID           PIC X(2).
           20 EA-KFMDECKUMFANG.
              49 EA-KFMDECKUMFANG-LEN        PIC -----9    .
              49 EA-KFMDECKUMFANG-TEXT       PIC X(30).
           20 EA-KFMHAFTPFLBEITRAG           PIC ---------9.99    .
           20 EA-KFMHPFBEITRAGWKZ            PIC X(01).
           20 EA-KFMHAFTPFLREGIOKL           PIC X(3).
           20 EA-KFMHAFTPFLSCHADFID          PIC -----9    .
           20 EA-KFMTARBEZEICHNUNG           PIC X(15).
      ******************************************************************
