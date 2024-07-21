      * T13EX39   Stand: 22/07/98    21:09:19
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0384.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0384.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:40:52   T13EX39
      *FUNKTION UPDATE ZTKI0315
      *
      *   Rev 1.0   14 Jul 1998 17:03:50   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0384.
           05  PI-DRKI0384-KEY.


             10   PI-DRKI0384-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10   PI-DRKI0384-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI0384-DATEN.


             10   PI-DRKI0384-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10   PI-DRKI0384-GHPPRODUKTID   PIC S9(4) COMP.


      *
       01  PO-DRKI0384.
           05  PO-DRKI0384-RC                 PIC  X(003).
               88 PO-DRKI0384-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0384-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
