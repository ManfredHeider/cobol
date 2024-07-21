      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0044.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0044.CPv  $
      *
      *   Rev 2.2   Aug 11 2003 11:13:06   t13ex27
      *AKI00360 - DB-Änderung für Fix 46 und
      *           neues Adressverfahren
      *
      *   Rev 2.1   Apr 16 1999 10:04:54   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSONADRESSBEZ-GESAMT.
         10 EA-NI-PERSONADRESSBEZ-TAB.
           20 EA-NI-PERSONADRESSBEZ-ELE PIC X OCCURS 5 TIMES.
         10 EA-NI-PERSONADRESSBEZ
                 REDEFINES EA-NI-PERSONADRESSBEZ-TAB.
           20 EA-NI-PERSABPERSONENID         PIC X.
           20 EA-NI-PERSABADRESSEID          PIC X.
           20 EA-NI-PERSABADRESSARTID        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-PERSABVERTRADRKZ         PIC X.
         10 EA-DCL-PERSONADRESSBEZ.
           20 EA-PERSABPERSONENID            PIC X(13).
           20 EA-PERSABADRESSEID             PIC X(13).
           20 EA-PERSABADRESSARTID           PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-PERSABVERTRADRKZ            PIC X(1).
      ******************************************************************
