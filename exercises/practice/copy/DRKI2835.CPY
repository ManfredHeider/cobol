      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2835.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2835.CPv  $
      *
      *   Rev 2.5   Jan 06 2017 09:48:14   t13ex78
      *AKI01481: Änderungen UPKI0253
      *
      *   Rev 2.4   Dec 15 2016 12:13:16   t13ex78
      *AKI01481: OZPs und Funktionen
      *
      *   Rev 2.3   Oct 28 2015 19:31:34   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - An
      *passung ZTKI0077
      *
      *   Rev 2.2   Oct 28 2015 09:11:54   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - An
      *passung ZTKI0077
      *
      *   Rev 2.1   Jan 23 2014 10:48:14   t13ex27
      *AKI01063: Wartungsauftrag-Ticketverarbeitung-Kundenlöschung
      *
      *   Rev 2.0   Jan 6 2014 18:14:29 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2835                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2835                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.01.2017 / 09:37:55 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2835
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2835.
           05  PI-DRKI2835-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2835-VERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2835.
           05  PO-DRKI2835-RC                     PIC  X(003).
               88 PO-DRKI2835-RC-OK               VALUE 'OK '.
               88 PO-DRKI2835-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2835-DATEN.
             10   PO-DRKI2835-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

