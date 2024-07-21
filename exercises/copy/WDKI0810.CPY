      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0810.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0810.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:33:10   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:22   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PTRBINARY-GESAMT.
         10 EA-NI-PTRBINARY-TAB.
           20 EA-NI-PTRBINARY-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-PTRBINARY
                 REDEFINES EA-NI-PTRBINARY-TAB.
           20 EA-NI-PTRID                    PIC X.
           20 EA-NI-PTRCREATORID             PIC X.
           20 EA-NI-PTRSUBTYPE               PIC X.
           20 EA-NI-PTRPART                  PIC X.
           20 EA-NI-PTRITEMSUBTYPE           PIC X.
           20 EA-NI-PTRCONTENTS              PIC X.
         10 EA-DCL-PTRBINARY.
           20 EA-PTRID                       PIC --------9 .
           20 EA-PTRCREATORID                PIC --------9 .
           20 EA-PTRSUBTYPE                  PIC X(02).
           20 EA-PTRPART                     PIC --------9 .
           20 EA-PTRITEMSUBTYPE              PIC X(02).
           20 EA-PTRCONTENTS.
              49 EA-PTRCONTENTS-LEN          PIC -----9    .
              49 EA-PTRCONTENTS-TEXT         PIC X(32688).
      ******************************************************************
