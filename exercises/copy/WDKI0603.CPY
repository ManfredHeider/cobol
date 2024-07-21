      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0603.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0603.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:28   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUTHORISIERUNG-GESAMT.
         10 EA-NI-AUTHORISIERUNG-TAB.
           20 EA-NI-AUTHORISIERUNG-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-AUTHORISIERUNG
                 REDEFINES EA-NI-AUTHORISIERUNG-TAB.
           20 EA-NI-AUTID                    PIC X.
           20 EA-NI-AUTNAME                  PIC X.
           20 EA-NI-AUTART                   PIC X.
           20 EA-NI-AUTKATEGORIENAME         PIC X.
           20 EA-NI-AUTREGELNAME             PIC X.
           20 EA-NI-AUTKLASSEALSSYMBOL       PIC X.
           20 EA-NI-AUTASPEKTALSSYMBOL       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AUTHORISIERUNG.
           20 EA-AUTID                       PIC X(13).
           20 EA-AUTNAME                     PIC X(30).
           20 EA-AUTART                      PIC X(01).
           20 EA-AUTKATEGORIENAME            PIC X(13).
           20 EA-AUTREGELNAME                PIC X(30).
           20 EA-AUTKLASSEALSSYMBOL          PIC X(50).
           20 EA-AUTASPEKTALSSYMBOL          PIC X(50).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
