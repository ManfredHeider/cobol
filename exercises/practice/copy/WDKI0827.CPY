      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wdki0827.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\wdki0827.cpv  $
      *
      *   Rev 2.1   Dec 16 1999 16:05:08   I13PC19
      *Erstanlage
      *
      *   Rev 2.0   16 Dec 1999 15:59:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*

        05 EA-ITEMCONTR-GESAMT.
         10 EA-NI-ITEMCONTR-TAB.
           20 EA-NI-ITEMCONTR-ELE PIC X
                  OCCURS 6 TIMES.
         10 EA-NI-ITEMCONTR
                 REDEFINES EA-NI-ITEMCONTR-TAB.
           20 EA-NI-ITEMCONTRID              PIC X.
           20 EA-NI-ICRPCNRVON               PIC X.
           20 EA-NI-ICRPCNRNACH              PIC X.
           20 EA-NI-ICRABSCHLUSSVER          PIC X.
           20 EA-NI-ICREVENTID               PIC X.
           20 EA-NI-TIMESTAMP                PIC X.
         10 EA-DCL-ITEMCONTR.
           20 EA-ITEMCONTRID                 PIC X(13).
           20 EA-ICRPCNRVON                  PIC --------9 .
           20 EA-ICRPCNRNACH                 PIC --------9 .
           20 EA-ICRABSCHLUSSVER             PIC X(01).
           20 EA-ICREVENTID                  PIC X(12).
           20 EA-TIMESTAMP                   PIC X(26).
