      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2547.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2547.CPv  $
      *
      *   Rev 2.1   Feb 03 2012 15:16:14   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.0   Dec 5 2011 15:12:28 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2547                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2547                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.12.2011 / 15:12:28 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2547
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2547.
           05  PI-DRKI2547-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2547-UBRJAHR   PIC S9(4) COMP.
             10 PI-DRKI2547-UBRMONAT   PIC S9(4) COMP.
             10 PI-DRKI2547-UBRRGID   PIC S9(4) COMP.
             10 PI-DRKI2547-UBRVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2547-UBRVETLFDNR   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2547.
           05  PO-DRKI2547-RC                     PIC  X(003).
               88 PO-DRKI2547-RC-OK               VALUE 'OK '.
               88 PO-DRKI2547-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
