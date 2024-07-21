      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0301.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:07:12   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:26   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ARBNEHMSPARZULJAHR-GESAMT.
         10 EA-NI-ARBNEHMSPARZULJAHR-TAB.
           20 EA-NI-ARBNEHMSPARZULJAHR-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-ARBNEHMSPARZULJAHR
                 REDEFINES EA-NI-ARBNEHMSPARZULJAHR-TAB.
           20 EA-NI-ANSVERTRAGSID            PIC X.
           20 EA-NI-ANSGUTHPRODLFDNR         PIC X.
           20 EA-NI-ANSLAUFENDENUMMER        PIC X.
           20 EA-NI-ANSJAHR                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-ARBNEHMSPARZULJAHR.
           20 EA-ANSVERTRAGSID               PIC X(13).
           20 EA-ANSGUTHPRODLFDNR            PIC -----9    .
           20 EA-ANSLAUFENDENUMMER           PIC -----9    .
           20 EA-ANSJAHR                     PIC X(4).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
