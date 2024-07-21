      * T13EX39   Stand: 22/07/98    21:08:36
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0294.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0294.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:54:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 14:11:46   T13EX39
      *FUNKTION UPDATE FUER ZTKI0315
      *
      *   Rev 1.0   07 Jul 1998 11:50:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0294.
           05  PI-DRKI0294-KEY.


             10   PI-DRKI0294-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10   PI-DRKI0294-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI0294-DATEN.


             10   PI-DRKI0294-GHPANLAGENAME.
                15   PI-DRKI0294-GHPANLAGENAME-LEN   PIC S9(4) COMP.
                15   PI-DRKI0294-GHPANLAGENAME-TEXT   PIC X(15).

             10   PI-DRKI0294-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10   PI-DRKI0294-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10   PI-DRKI0294-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10   PI-DRKI0294-GHPGUTHABENWKZ   PIC X(01).
             10   PI-DRKI0294-GHPGUTHWERTWKZ   PIC X(01).
             10   PI-DRKI0294-GHPINFOSPERREDATUM   PIC X(10).
             10   PI-DRKI0294-GHPINFOWPERREKZ   PIC X(01).
             10   PI-DRKI0294-GHPPRODUKTID   PIC S9(4) COMP.
             10   PI-DRKI0294-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10   PI-DRKI0294-GHPSCHLAUSZGWKZ   PIC X(01).
             10   PI-DRKI0294-GHPSUBTYPE   PIC X(01).
             10   PI-DRKI0294-GHPVERFUEGUNGID   PIC X(2).
             10   PI-DRKI0294-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.


      *
       01  PO-DRKI0294.
           05  PO-DRKI0294-RC                 PIC  X(003).
               88 PO-DRKI0294-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0294-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
