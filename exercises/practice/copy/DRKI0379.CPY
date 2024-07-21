      * T13EX47   Stand: 17/11/98    16:21:46
      * T13EX39   Stand: 21/07/98    21:34:50
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0379.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0379.CPv  $
      *
      *   Rev 2.1   Jul 12 2007 08:50:58   t13ex48
      *AKI00670: Reaktivierung ausgelagerter KI-Sourcen
      *
      *   Rev 2.0   Feb 27 1999 12:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 Feb 1999 12:09:34   T13EX40
      *AKI00016 - Funktionserweiterung
      *
      *   Rev 1.1   28 Jan 1999 10:10:44   T13EX39
      *afki0001 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 17:03:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0379.
           05 PI-DRKI0379-UFKT                  PIC 9(02).
              88 PI-DRKI0379-GETMAXTR              VALUE 1.
              88 PI-DRKI0379-INCTRID               VALUE 2.
           05 PI-DRKI0379-GESNR                 PIC S9(4).
           05 PI-DRKI0379-PGMNAME               PIC  X(8).

       01  PO-DRKI0379.
           05 PO-DRKI0379-RC                    PIC X(03).
              88  PO-DRKI0379-RC-OK                 VALUE 'OK '.

           05 PO-DRKI0379-MAXTR                 PIC S9(4) COMP.
