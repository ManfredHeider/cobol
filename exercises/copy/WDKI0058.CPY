      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0058.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:36   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ZUSPERSONENNAMEN-GESAMT.
         10 EA-NI-ZUSPERSONENNAMEN-TAB.
           20 EA-NI-ZUSPERSONENNAMEN-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-ZUSPERSONENNAMEN
                 REDEFINES EA-NI-ZUSPERSONENNAMEN-TAB.
           20 EA-NI-ZPNPERSONENID            PIC X.
           20 EA-NI-ZPNPERSNAMENARTID        PIC X.
           20 EA-NI-ZPNNAME                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-ZUSPERSONENNAMEN.
           20 EA-ZPNPERSONENID               PIC X(13).
           20 EA-ZPNPERSNAMENARTID           PIC -----9    .
           20 EA-ZPNNAME                     PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
