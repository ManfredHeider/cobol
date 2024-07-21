      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0826.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/MF/CPY/Wuki0826.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 17:07:46   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-ITEM.
           10 ITMID                       PIC X(13).
           10 ITMPCNR                     PIC S9(9) COMP.
           10 ITMSTATUS                   PIC S9(4) COMP.
           10 ITMANGELEGT                 PIC X(010).
           10 NIL-ITMANGELEGT             PIC X(01).
           10 ITMREPLIZIERT               PIC X(010).
           10 NIL-ITMREPLIZIERT           PIC X(01).
           10 ITMERSTELLT                 PIC X(010).
           10 NIL-ITMERSTELLT             PIC X(01).
           10 ITMGELOESCHT                PIC X(010).
           10 NIL-ITMGELOESCHT            PIC X(01).
           10 ITMSRVABGEHOLT              PIC X(010).
           10 NIL-ITMSRVABGEHOLT          PIC X(01).
           10 ITMEINGEARBEITET            PIC X(010).
           10 NIL-ITMEINGEARBEITET        PIC X(01).
           10 ITMVBNRTEA                  PIC S9(9) COMP.
           10 NIL-ITMVBNRTEA              PIC X(01).
           10 ITMGRUND-TEA                PIC S9(9) COMP.
           10 NIL-ITMGRUND-TEA            PIC X(01).
           10 ITMGRUND-EA                 PIC X(40).
           10 NIL-ITMGRUND-EA             PIC X(01).
           10 ITMWICHTIGKENNZ             PIC X(1).
           10 ITMGENERIERT                PIC X(1).
           10 NIL-ITMGENERIERT            PIC X(01).
           10 TIMESTAMP                   PIC X(026).
           10 NIL-TIMESTAMP               PIC X(01).
      *****************************************************************
