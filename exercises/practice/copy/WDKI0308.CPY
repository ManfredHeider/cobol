      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0308.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/WDKI0308.CPv  $
      *
      *   Rev 2.3   Apr 07 2003 10:12:44   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.2   Oct 15 2002 08:15:48   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   Apr 16 1999 10:07:16   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:28   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FREISTELLUNGSAUFTR-GESAMT.
         10 EA-NI-FREISTELLUNGSAUFTR-TAB.
           20 EA-NI-FREISTELLUNGSAUFTR-ELE PIC X
                  OCCURS 14 TIMES.
         10 EA-NI-FREISTELLUNGSAUFTR
                 REDEFINES EA-NI-FREISTELLUNGSAUFTR-TAB.
           20 EA-NI-FRAPERSONENID            PIC X.
           20 EA-NI-FRALFDNR                 PIC X.
           20 EA-NI-FRAPARTNERGESID          PIC X.
           20 EA-NI-FRAFREMDVERTGESNAM       PIC X.
           20 EA-NI-FRAGEMFREIBETRAG         PIC X.
           20 EA-NI-FRAGEMFRBETRAGWKZ        PIC X.
           20 EA-NI-FRAFREIBETRAGVERBR       PIC X.
           20 EA-NI-FRAFRBETRVERBRWKZ        PIC X.
           20 EA-NI-FRAANZDEPFREISTBET       PIC X.
           20 EA-NI-FRAZINSABSCHSTEUER       PIC X.
           20 EA-NI-FRAFREIBETRAGABL         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-FRASTUECKZINS            PIC X.
           20 EA-NI-FRAGUELTIGAB             PIC X.
         10 EA-DCL-FREISTELLUNGSAUFTR.
           20 EA-FRAPERSONENID               PIC X(13).
           20 EA-FRALFDNR                    PIC -----9.
           20 EA-FRAPARTNERGESID             PIC -----9.
           20 EA-FRAFREMDVERTGESNAM.
              49 EA-FRAFREMDVERTGESNAM-LEN   PIC -----9.
              49 EA-FRAFREMDVERTGESNAM-TEXT  PIC X(25).
           20 EA-FRAGEMFREIBETRAG            PIC ---------9.99.
           20 EA-FRAGEMFRBETRAGWKZ           PIC X(1).
           20 EA-FRAFREIBETRAGVERBR          PIC ---------9.99.
           20 EA-FRAFRBETRVERBRWKZ           PIC X(1).
           20 EA-FRAANZDEPFREISTBET          PIC -----9.
           20 EA-FRAZINSABSCHSTEUER          PIC ---------9.99.
           20 EA-FRAFREIBETRAGABL            PIC X(010).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-FRASTUECKZINS               PIC ----------9.99.
           20 EA-FRAGUELTIGAB                PIC X(010).
      *****************************************************************
