      * T13EX39   Stand: 22/07/98    21:09:15
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0383.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0383.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:39:46   T13EX39
      *FUNKTION UPDATE ZTKI0315
      *
      *   Rev 1.0   14 Jul 1998 17:03:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0383.
           05  PI-DRKI0383-KEY.


             10   PI-DRKI0383-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10   PI-DRKI0383-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI0383-DATEN.


             10   PI-DRKI0383-GHPPRODUKTID   PIC S9(4) COMP.
             10   PI-DRKI0383-GHPVERFUEGUNGID   PIC X(2).


      *
       01  PO-DRKI0383.
           05  PO-DRKI0383-RC                 PIC  X(003).
               88 PO-DRKI0383-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0383-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
