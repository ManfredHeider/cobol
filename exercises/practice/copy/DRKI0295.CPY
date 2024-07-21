      * T13EX39   Stand: 22/07/98    21:08:40
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0295.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0295.CPv  $
      *
      *   Rev 2.2   Jul 26 2004 15:08:46   T13EX39
      *AKI00249: DeuBA Depot u. Banking-Produkte
      *
      *   Rev 2.1   Jul 26 2004 12:49:44   T13EX39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank KONTOKORRENT
      *
      *   Rev 1.1   08 Feb 1999 14:12:28   T13EX39
      *FUNKTION DELETE FUER ZTKI0315
      *
      *   Rev 1.0   07 Jul 1998 11:50:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0295.
           05  PI-DRKI0295-KEY.


             10   PI-DRKI0295-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10   PI-DRKI0295-GHPVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0295.
           05  PO-DRKI0295-RC                 PIC  X(003).
               88 PO-DRKI0295-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0295-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
