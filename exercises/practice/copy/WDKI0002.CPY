      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0002.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0002.CPv  $
      *
      *   Rev 2.3   Feb 16 2004 09:48:06   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.2   Aug 11 2003 11:12:16   t13ex27
      *AKI00360 - DB-Änderung für Fix 46 und
      *           neues Adressverfahren
      *
      *   Rev 2.1   Apr 16 1999 10:04:12   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:31:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ADRESSE-GESAMT.
         10 EA-NI-ADRESSE-TAB.
           20 EA-NI-ADRESSE-ELE PIC X OCCURS 13 TIMES.
         10 EA-NI-ADRESSE
                 REDEFINES EA-NI-ADRESSE-TAB.
           20 EA-NI-ADRESSEID                PIC X.
           20 EA-NI-ADRESSEBDSLDID           PIC X.
           20 EA-NI-ADRESSELANDID            PIC X.
           20 EA-NI-ADRESSEORT               PIC X.
           20 EA-NI-ADRESSEPLZ               PIC X.
           20 EA-NI-ADRESSESTRHAUSNUM        PIC X.
           20 EA-NI-ADRESSEISTVTADR          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-ADRESSESUCHSTRASSE       PIC X.
           20 EA-NI-ADRESSEGEPRUEFT          PIC X.
           20 EA-NI-ADRESSEORTSTEIL          PIC X.
           20 EA-NI-ADRESSEPOSTPRUEF         PIC X.
           20 EA-NI-ADRESSEPOSTPRUEFID       PIC X.
         10 EA-DCL-ADRESSE.
           20 EA-ADRESSEID                   PIC X(13).
           20 EA-ADRESSEBDSLDID              PIC X(3).
           20 EA-ADRESSELANDID               PIC X(3).
           20 EA-ADRESSEORT                  PIC X(30).
           20 EA-ADRESSEPLZ                  PIC X(15).
           20 EA-ADRESSESTRHAUSNUM           PIC X(45).
           20 EA-ADRESSEISTVTADR             PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-ADRESSESUCHSTRASSE          PIC X(45).
           20 EA-ADRESSEGEPRUEFT             PIC X(1).
           20 EA-ADRESSEORTSTEIL             PIC X(30).
           20 EA-ADRESSEPOSTPRUEF            PIC X(25).
           20 EA-ADRESSEPOSTPRUEFID          PIC -----9.
      ******************************************************************
