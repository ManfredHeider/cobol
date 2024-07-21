      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2475.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2475.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:58:24   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Oct 17 2011 18:32:34 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2475                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2475                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.10.2011 / 18:32:34 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2475
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2475.
           05  PI-DRKI2475-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2475-KNDNUMMER   PIC S9(9) COMP.
END   *

      *
       01  PO-DRKI2475.
           05  PO-DRKI2475-RC                     PIC  X(003).
               88 PO-DRKI2475-RC-OK               VALUE 'OK '.
               88 PO-DRKI2475-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2475-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2475-KNDALTKUNDE   PIC  X(010).
             10 PO-DRKI2475-KNDANREDEID   PIC S9(4) COMP.
             10 PO-DRKI2475-KNDANZBETREUER   PIC S9(4) COMP.
             10 PO-DRKI2475-KNDANZNAME   PIC  X(0030).
             10 PO-DRKI2475-KNDANZVORNAME   PIC  X(0030).
             10 PO-DRKI2475-KNDBDSGSPERRDAT   PIC  X(010).
             10 PO-DRKI2475-KNDGEANDERTPGM   PIC  X(0008).
             10 PO-DRKI2475-KNDGEANDERTVON   PIC  X(0004).
             10 PO-DRKI2475-KNDGEBGRUENDDAT   PIC  X(010).
             10 PO-DRKI2475-KNDGESCHLECHT   PIC  X(0001).
             10 PO-DRKI2475-KNDMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2475-KNDNEUKUNDE   PIC  X(010).
             10 PO-DRKI2475-KNDPERSONENID   PIC  X(0013).
             10 PO-DRKI2475-KNDSTATUSID   PIC  X(0001).
             10 PO-DRKI2475-KNDSUCHNAME   PIC  X(0030).
             10 PO-DRKI2475-KNDSUCHVORNAME   PIC  X(0030).
             10 PO-DRKI2475-KNDWIK   PIC  X(010).
             10 PO-DRKI2475-LETZTEAENDERUNG   PIC  X(026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
