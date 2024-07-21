      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0722.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0722.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:31:06   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TMPSELFILTERBETR-GESAMT.
         10 EA-NI-TMPSELFILTERBETR-TAB.
           20 EA-NI-TMPSELFILTERBETR-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-TMPSELFILTERBETR
                 REDEFINES EA-NI-TMPSELFILTERBETR-TAB.
           20 EA-NI-BETREUERPERSONENID       PIC X.
           20 EA-NI-MITARBEITERNUMMER        PIC X.
           20 EA-NI-STRUKTURSTUFENNR         PIC X.
         10 EA-DCL-TMPSELFILTERBETR.
           20 EA-BETREUERPERSONENID          PIC X(13).
           20 EA-MITARBEITERNUMMER           PIC --------9 .
           20 EA-STRUKTURSTUFENNR            PIC -----9    .
      ******************************************************************
