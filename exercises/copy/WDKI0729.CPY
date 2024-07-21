      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0729.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0729.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:31:58   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELOPERATOR-GESAMT.
         10 EA-NI-SELOPERATOR-TAB.
           20 EA-NI-SELOPERATOR-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-SELOPERATOR
                 REDEFINES EA-NI-SELOPERATOR-TAB.
           20 EA-NI-SOPID                    PIC X.
           20 EA-NI-SOPBEZEICHNUNGTEXT       PIC X.
           20 EA-NI-SOPANZAHLWERTE           PIC X.
           20 EA-NI-SOPGUELTIGBISDATUM       PIC X.
           20 EA-NI-SOPSYMBOL                PIC X.
           20 EA-NI-SOPDATENTYPENRELEV       PIC X.
         10 EA-DCL-SELOPERATOR.
           20 EA-SOPID                       PIC -----9    .
           20 EA-SOPBEZEICHNUNGTEXT          PIC X(40).
           20 EA-SOPANZAHLWERTE              PIC --------9 .
           20 EA-SOPGUELTIGBISDATUM          PIC X(10).
           20 EA-SOPSYMBOL                   PIC X(15).
           20 EA-SOPDATENTYPENRELEV.
              49 EA-SOPDATENTYPENRELEV-LEN   PIC -----9    .
              49 EA-SOPDATENTYPENRELEV-TEXT  PIC X(150).
      ******************************************************************
