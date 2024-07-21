      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2472.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2472.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:58:14   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Oct 17 2011 18:32:27 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2472                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2472                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.12.2011 / 15:21:43 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2472
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2472.
           05  PI-DRKI2472-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2472-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2472-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2472-KNDALTKUNDE   PIC  X(010).
             10 PI-DRKI2472-KNDANREDEID   PIC S9(4) COMP.
             10 PI-DRKI2472-KNDANZBETREUER   PIC S9(4) COMP.
             10 PI-DRKI2472-KNDANZNAME   PIC  X(0030).
             10 PI-DRKI2472-NI-KNDANZNAME      PIC S9(4) COMP.
             10 PI-DRKI2472-KNDANZVORNAME   PIC  X(0030).
             10 PI-DRKI2472-NI-KNDANZVORNAME   PIC S9(4) COMP.
             10 PI-DRKI2472-KNDBDSGSPERRDAT   PIC  X(010).
             10 PI-DRKI2472-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2472-KNDGEANDERTVON   PIC  X(0004).
             10 PI-DRKI2472-KNDGEBGRUENDDAT   PIC  X(010).
             10 PI-DRKI2472-KNDGESCHLECHT   PIC  X(0001).
             10 PI-DRKI2472-KNDMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI2472-KNDNEUKUNDE   PIC  X(010).
             10 PI-DRKI2472-KNDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI2472-NI-KNDNUMMER       PIC S9(4) COMP.
             10 PI-DRKI2472-KNDSTATUSID   PIC  X(0001).
             10 PI-DRKI2472-KNDSUCHNAME   PIC  X(0030).
             10 PI-DRKI2472-KNDSUCHVORNAME   PIC  X(0030).
             10 PI-DRKI2472-KNDWIK   PIC  X(010).
END   *


      *
       01  PO-DRKI2472.
           05  PO-DRKI2472-RC                     PIC  X(003).
               88 PO-DRKI2472-RC-OK               VALUE 'OK '.
               88 PO-DRKI2472-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
