      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2474.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2474.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:17:24   t13ex27
      *AKI01135: Kundendaten physisch löschen-Ticket-HP
      *
      *   Rev 2.0   Dec 21 2011 09:24:11 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2474                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2474                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.12.2011 / 09:24:11 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2474
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2474.
           05  PI-DRKI2474-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2474-KNDPERSONENID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2474.
           05  PO-DRKI2474-RC                     PIC  X(003).
               88 PO-DRKI2474-RC-OK               VALUE 'OK '.
               88 PO-DRKI2474-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2474-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2474-KNDALTKUNDE   PIC  X(010).
             10 PO-DRKI2474-KNDANREDEID   PIC S9(4) COMP.
             10 PO-DRKI2474-KNDANZBETREUER   PIC S9(4) COMP.
             10 PO-DRKI2474-KNDANZNAME   PIC  X(0030).
             10 PO-DRKI2474-NI-KNDANZNAME   PIC S9(4) COMP.
             10 PO-DRKI2474-KNDANZVORNAME   PIC  X(0030).
             10 PO-DRKI2474-NI-KNDANZVORNAME   PIC S9(4) COMP.
             10 PO-DRKI2474-KNDBDSGSPERRDAT   PIC  X(010).
             10 PO-DRKI2474-KNDGEANDERTPGM   PIC  X(0008).
             10 PO-DRKI2474-KNDGEANDERTVON   PIC  X(0004).
             10 PO-DRKI2474-KNDGEBGRUENDDAT   PIC  X(010).
             10 PO-DRKI2474-KNDGESCHLECHT   PIC  X(0001).
             10 PO-DRKI2474-KNDMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2474-KNDNEUKUNDE   PIC  X(010).
             10 PO-DRKI2474-KNDNUMMER   PIC S9(9) COMP.
             10 PO-DRKI2474-NI-KNDNUMMER   PIC S9(4) COMP.
             10 PO-DRKI2474-KNDSTATUSID   PIC  X(0001).
             10 PO-DRKI2474-KNDSUCHNAME   PIC  X(0030).
             10 PO-DRKI2474-KNDSUCHVORNAME   PIC  X(0030).
             10 PO-DRKI2474-KNDWIK   PIC  X(010).
             10 PO-DRKI2474-LETZTEAENDERUNG   PIC  X(026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
