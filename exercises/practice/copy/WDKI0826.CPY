      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wdki0826.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\wdki0826.cpv  $
      *
      *   Rev 2.1   Dec 16 1999 16:05:06   I13PC19
      *Erstanlage
      *
      *   Rev 2.0   16 Dec 1999 15:59:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*

        05 EA-ITEM-GESAMT.
         10 EA-NI-ITEM-TAB.
           20 EA-NI-ITEM-ELE PIC X
                  OCCURS 15 TIMES.
         10 EA-NI-ITEM
                 REDEFINES EA-NI-ITEM-TAB.
           20 EA-NI-ITMID                    PIC X.
           20 EA-NI-ITMPCNR                  PIC X.
           20 EA-NI-ITMSTATUS                PIC X.
           20 EA-NI-ITMANGELEGT              PIC X.
           20 EA-NI-ITMREPLIZIERT            PIC X.
           20 EA-NI-ITMERSTELLT              PIC X.
           20 EA-NI-ITMGELOESCHT             PIC X.
           20 EA-NI-ITMSRVABGEHOLT           PIC X.
           20 EA-NI-ITMEINGEARBEITET         PIC X.
           20 EA-NI-ITMVBNRTEA               PIC X.
           20 EA-NI-ITMGRUND-TEA             PIC X.
           20 EA-NI-ITMGRUND-EA              PIC X.
           20 EA-NI-ITMWICHTIGKENNZ          PIC X.
           20 EA-NI-ITMGENERIERT             PIC X.
           20 EA-NI-TIMESTAMP                PIC X.
         10 EA-DCL-ITEM.
           20 EA-ITMID                       PIC X(13).
           20 EA-ITMPCNR                     PIC --------9 .
           20 EA-ITMSTATUS                   PIC -----9    .
           20 EA-ITMANGELEGT                 PIC X(10).
           20 EA-ITMREPLIZIERT               PIC X(10).
           20 EA-ITMERSTELLT                 PIC X(10).
           20 EA-ITMGELOESCHT                PIC X(10).
           20 EA-ITMSRVABGEHOLT              PIC X(10).
           20 EA-ITMEINGEARBEITET            PIC X(10).
           20 EA-ITMVBNRTEA                  PIC --------9 .
           20 EA-ITMGRUND-TEA                PIC --------9 .
           20 EA-ITMGRUND-EA                 PIC X(40).
           20 EA-ITMWICHTIGKENNZ             PIC X(01).
           20 EA-ITMGENERIERT                PIC X(01).
           20 EA-TIMESTAMP                   PIC X(26).
