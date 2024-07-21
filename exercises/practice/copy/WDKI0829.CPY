      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wdki0829.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\wdki0829.cpv  $
      *
      *   Rev 2.1   Dec 16 1999 16:05:10   I13PC19
      *Erstanlage
      *
      *   Rev 2.0   16 Dec 1999 15:59:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*

        05 EA-ITEMBT-GESAMT.
         10 EA-NI-ITEMBT-TAB.
           20 EA-NI-ITEMBT-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-ITEMBT
                 REDEFINES EA-NI-ITEMBT-TAB.
           20 EA-NI-IBTID                    PIC X.
           20 EA-NI-IBTVBNR                  PIC X.
           20 EA-NI-IBTPCVON                 PIC X.
           20 EA-NI-IBTPCNACH                PIC X.
           20 EA-NI-TIMESTAMP                PIC X.
         10 EA-DCL-ITEMBT.
           20 EA-IBTID                       PIC X(13).
           20 EA-IBTVBNR                     PIC --------9 .
           20 EA-IBTPCVON                    PIC --------9 .
           20 EA-IBTPCNACH                   PIC --------9 .
           20 EA-TIMESTAMP                   PIC X(26).
