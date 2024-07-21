      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0093.CPv  $
      *
      *   Rev 2.1   Sep 13 2002 08:33:42   t13ex27
      *AKI00300 Neue Tabelle ARBEITGEBERADRESSE
      *
      *   Rev 2.0   Aug 30 2002 12:10:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-ARBEITGEBERADRESSE-GESAMT.
         10 EA-NI-ARBEITGEBERADRESSE-TAB.
           20 EA-NI-ARBEITGEBERADRESSE-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-ARBEITGEBERADRESSE
                 REDEFINES EA-NI-ARBEITGEBERADRESSE-TAB.
           20 EA-NI-AGAPERSONENID,           PIC X.
           20 EA-NI-AGAADRESSEBDSLDID,       PIC X.
           20 EA-NI-AGAADRESSELANDID,        PIC X.
           20 EA-NI-AGAADRESSEORT,           PIC X.
           20 EA-NI-AGAADRESSEPLZ,           PIC X.
           20 EA-NI-AGAADRESSESTHAUSNR,      PIC X.
           20 EA-NI-AGAADRESSEISTVTADR,      PIC X.
           20 EA-NI-LETZTEAENDERUNG,         PIC X.
           20 EA-NI-AGAADRESSESUCHSTR,       PIC X.
           20 EA-NI-AGAADRESSEGEPRUEFT,      PIC X.
         10 EA-DCL-ARBEITGEBERADRESSE.
           20 EA-AGAPERSONENID               PIC X(13).
           20 EA-AGAADRESSEBDSLDID           PIC X(3).
           20 EA-AGAADRESSELANDID            PIC X(3).
           20 EA-AGAADRESSEORT               PIC X(30).
           20 EA-AGAADRESSEPLZ               PIC X(15).
           20 EA-AGAADRESSESTHAUSNR          PIC X(45).
           20 EA-AGAADRESSEISTVTADR          PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-AGAADRESSESUCHSTR           PIC X(45).
           20 EA-AGAADRESSEGEPRUEFT          PIC X(1).
      *****************************************************************
