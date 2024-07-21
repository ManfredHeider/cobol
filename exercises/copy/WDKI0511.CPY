      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0511.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0511.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:22   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-STATISTIKGESAMT-GESAMT.
         10 EA-NI-STATISTIKGESAMT-TAB.
           20 EA-NI-STATISTIKGESAMT-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-STATISTIKGESAMT
                 REDEFINES EA-NI-STATISTIKGESAMT-TAB.
           20 EA-NI-WORKSTATIONNUMBER        PIC X.
           20 EA-NI-ZEITLETPROGUPDATE        PIC X.
           20 EA-NI-LETERFPROGUPDATE         PIC X.
           20 EA-NI-LETZTEREPLIKZEIT         PIC X.
           20 EA-NI-LETZTEREPLIKDATUM        PIC X.
           20 EA-NI-DATUMLETPROGUPDATE       PIC X.
           20 EA-NI-FESTPLATTENPLATZC        PIC X.
           20 EA-NI-FESTPLATTENPLATZD        PIC X.
           20 EA-NI-FESTPLATTENPLATZE        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-STATISTIKGESAMT.
           20 EA-WORKSTATIONNUMBER           PIC --------9 .
           20 EA-ZEITLETPROGUPDATE           PIC X(08).
           20 EA-LETERFPROGUPDATE            PIC X(10).
           20 EA-LETZTEREPLIKZEIT            PIC X(08).
           20 EA-LETZTEREPLIKDATUM           PIC X(10).
           20 EA-DATUMLETPROGUPDATE          PIC X(10).
           20 EA-FESTPLATTENPLATZC           PIC --------9 .
           20 EA-FESTPLATTENPLATZD           PIC --------9 .
           20 EA-FESTPLATTENPLATZE           PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
