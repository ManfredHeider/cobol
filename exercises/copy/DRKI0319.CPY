      * T13EX39   Stand: 28/07/98    17:05:13
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0319.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0319.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   28 Jan 1999 10:09:50   T13EX39
      *afki0001 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 16:59:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0319.
           05  PI-DRKI0319-DATEN.
               10  PI-DRKI0319-VERTRAGSID      PIC  X(13).
               10  PI-DRKI0319-ROLLE-VERTRAG   PIC  X(01).

      *
       01  PO-DRKI0319.
           05  PO-DRKI0319-RC                  PIC  X(003).
               88 PO-DRKI0319-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0319-RC-NOT-FOUND                 VALUE 'NFD'.

           05  PO-DRKI0319-PERSONID-CNT         PIC  9(02) COMP.
           05  PO-DRKI0319-PERSONID-TAB OCCURS 10.
               10  PO-DRKI0319-PERSONID        PIC  X(13).
      *
      ******* ENDE COPY-MEMBER **************************************
