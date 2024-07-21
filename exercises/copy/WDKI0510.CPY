      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0510.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\WDKI0510.CPv  $
      *
      *   Rev 2.2   Dec 16 1999 16:05:32   I13PC19
      * 
      *
      *   Rev 2.1   Apr 16 1999 10:13:14   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*

        05 EA-STATISTIK-GESAMT.
         10 EA-NI-STATISTIK-TAB.
           20 EA-NI-STATISTIK-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-STATISTIK
                 REDEFINES EA-NI-STATISTIK-TAB.
           20 EA-NI-STATISTIKID              PIC X.
           20 EA-NI-ADMAPERSONENID           PIC X.
           20 EA-NI-ADMAPERSONENROLLE        PIC X.
           20 EA-NI-WORKSTATIONNUMBER        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-MITARBEITERNR            PIC X.
         10 EA-DCL-STATISTIK.
           20 EA-STATISTIKID                 PIC X(13).
           20 EA-ADMAPERSONENID              PIC X(13).
           20 EA-ADMAPERSONENROLLE           PIC X(1).
           20 EA-WORKSTATIONNUMBER           PIC --------9 .
           20 EA-LETZTEAENDERUNG             PIC X(26).
           20 EA-MITARBEITERNR               PIC --------9 .
      ******************************************************************
