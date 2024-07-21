      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2834.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2834.CPv  $
      *
      *   Rev 2.1   Jan 23 2014 10:48:08   t13ex27
      *AKI01063: Wartungsauftrag-Ticketverarbeitung-Kundenlöschung
      *
      *   Rev 2.0   Jan 6 2014 15:17:30 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2834                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2834                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.01.2014 / 15:17:30 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2834
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2834.
           05  PI-DRKI2834-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2834-VRAVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2834.
           05  PO-DRKI2834-RC                     PIC  X(003).
               88 PO-DRKI2834-RC-OK               VALUE 'OK '.
               88 PO-DRKI2834-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2834-DATEN.
             10   PO-DRKI2834-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
