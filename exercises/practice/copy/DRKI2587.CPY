      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2587.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2587.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:59:38   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Mar 29 2012 17:38:19 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2587                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2587                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2012 / 16:08:21 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2587
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2587.
           05  PI-DRKI2587-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2587-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2587-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2587-KNDSUCHNAME   PIC  X(0030).
             10 PI-DRKI2587-KNDSUCHVORNAME   PIC  X(0030).
END   *


      *
       01  PO-DRKI2587.
           05  PO-DRKI2587-RC                     PIC  X(003).
               88 PO-DRKI2587-RC-OK               VALUE 'OK '.
               88 PO-DRKI2587-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
