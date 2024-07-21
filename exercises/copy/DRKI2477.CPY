      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2477.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2477.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:17:32   t13ex27
      *AKI01135: Kundendaten physisch löschen-Ticket-HP
      *
      *   Rev 2.0   Oct 17 2011 18:32:38 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2477                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2477                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.10.2011 / 18:32:38 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2477
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2477.
           05  PI-DRKI2477-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2477-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2477-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2477-KNDANZBETREUER   PIC S9(4) COMP.
             10 PI-DRKI2477-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2477-KNDGEANDERTVON   PIC  X(0004).
END   *


      *
       01  PO-DRKI2477.
           05  PO-DRKI2477-RC                     PIC  X(003).
               88 PO-DRKI2477-RC-OK               VALUE 'OK '.
               88 PO-DRKI2477-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
