      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0380.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0380.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:18   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:54   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERMOEGLEIST-GESAMT.
         10 EA-NI-VERMOEGLEIST-TAB.
           20 EA-NI-VERMOEGLEIST-ELE PIC X
                  OCCURS 11 TIMES.
         10 EA-NI-VERMOEGLEIST
                 REDEFINES EA-NI-VERMOEGLEIST-TAB.
           20 EA-NI-VMLVERTRAGSID            PIC X.
           20 EA-NI-VMLARBEITNEHMSPAR        PIC X.
           20 EA-NI-VMLARBSPARZULSUM         PIC X.
           20 EA-NI-VMLARBSPZULSUMWKZ        PIC X.
           20 EA-NI-VMLARBSPARZULDAT         PIC X.
           20 EA-NI-VMLVLBEITRAGGES          PIC X.
           20 EA-NI-VMLBEITRGESWKZ           PIC X.
           20 EA-NI-VMLARBEITGEBZUSCH        PIC X.
           20 EA-NI-VMLARBGEBZUSCHWKZ        PIC X.
           20 EA-NI-VMLENDESTEUERLFZ         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERMOEGLEIST.
           20 EA-VMLVERTRAGSID               PIC X(13).
           20 EA-VMLARBEITNEHMSPAR           PIC X(01).
           20 EA-VMLARBSPARZULSUM            PIC ---------9.99    .
           20 EA-VMLARBSPZULSUMWKZ           PIC X(01).
           20 EA-VMLARBSPARZULDAT            PIC X(10).
           20 EA-VMLVLBEITRAGGES             PIC ---------9.99    .
           20 EA-VMLBEITRGESWKZ              PIC X(01).
           20 EA-VMLARBEITGEBZUSCH           PIC ---------9.99    .
           20 EA-VMLARBGEBZUSCHWKZ           PIC X(01).
           20 EA-VMLENDESTEUERLFZ            PIC X(10).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
