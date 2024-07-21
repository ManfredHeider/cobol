      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2476.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2476.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:58:32   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Oct 17 2011 18:32:36 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2476                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2476                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.10.2011 / 18:32:36 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2476
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2476.
           05  PI-DRKI2476-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2476-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2476-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2476-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2476-KNDGEANDERTVON   PIC  X(0004).
             10 PI-DRKI2476-KNDNUMMER   PIC S9(9) COMP.
END   *


      *
       01  PO-DRKI2476.
           05  PO-DRKI2476-RC                     PIC  X(003).
               88 PO-DRKI2476-RC-OK               VALUE 'OK '.
               88 PO-DRKI2476-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
