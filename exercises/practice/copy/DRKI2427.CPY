      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2427.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2427.CPv  $
      *
      *   Rev 2.2   Sep 29 2010 07:46:56   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.1   Sep 23 2010 12:22:00   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.0   Sep 22 2010 08:22:00   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2427                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2427                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.09.2010 / 11:35:51 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2427
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2427.
           05  PI-DRKI2427-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2427-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2427-GHPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2427.
           05  PO-DRKI2427-RC                     PIC  X(003).
               88 PO-DRKI2427-RC-OK               VALUE 'OK '.
               88 PO-DRKI2427-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2427-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2427-GHPDETAILNAM-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2427-GHPDETAILNAM-KTY   PIC S9(4) COMP.
             10 PO-DRKI2427-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PO-DRKI2427-GHPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2427-GHPSCHLIESSDATUM   PIC  X(010).
             10 PO-DRKI2427-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
