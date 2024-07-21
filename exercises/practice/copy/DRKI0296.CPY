      * T13EX39   Stand: 22/07/98    21:08:45
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0296.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0296.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:54:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 14:13:02   T13EX39
      *FUNKTION SELECT FUER ZTKI0314
      *
      *   Rev 1.0   07 Jul 1998 11:50:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0296.
           05  PI-DRKI0296-KEY.


             10   PI-DRKI0296-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10   PI-DRKI0296-GHPVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0296.
           05  PO-DRKI0296-RC                 PIC  X(003).
               88 PO-DRKI0296-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0296-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0296-DATEN.


             10   PO-DRKI0296-GHPANLAGENAME.
                15   PO-DRKI0296-GHPANLAGENAME-LEN   PIC S9(4) COMP.
                15   PO-DRKI0296-GHPANLAGENAME-TEXT   PIC X(15).

             10   PO-DRKI0296-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10   PO-DRKI0296-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10   PO-DRKI0296-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10   PO-DRKI0296-GHPGUTHABENWKZ   PIC X(01).
             10   PO-DRKI0296-GHPGUTHWERTWKZ   PIC X(01).
             10   PO-DRKI0296-GHPINFOSPERREDATUM   PIC X(10).
             10   PO-DRKI0296-GHPINFOWPERREKZ   PIC X(01).
             10   PO-DRKI0296-GHPPRODUKTID   PIC S9(4) COMP.
             10   PO-DRKI0296-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10   PO-DRKI0296-GHPSCHLAUSZGWKZ   PIC X(01).
             10   PO-DRKI0296-GHPSUBTYPE   PIC X(01).
             10   PO-DRKI0296-GHPVERFUEGUNGID   PIC X(2).
             10   PO-DRKI0296-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10   PO-DRKI0296-LETZTEAENDERUNG   PIC X(26).


      *
      ******* ENDE COPY-MEMBER **************************************
