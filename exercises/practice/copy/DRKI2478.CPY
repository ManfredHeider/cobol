      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2478.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2478.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:58:42   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Oct 17 2011 18:32:41 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2478                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2478                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.10.2011 / 18:32:41 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2478
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2478.
           05  PI-DRKI2478-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2478-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2478-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2478-KNDBDSGSPERRDAT   PIC  X(010).
             10 PI-DRKI2478-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2478-KNDGEANDERTVON   PIC  X(0004).
END   *


      *
       01  PO-DRKI2478.
           05  PO-DRKI2478-RC                     PIC  X(003).
               88 PO-DRKI2478-RC-OK               VALUE 'OK '.
               88 PO-DRKI2478-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
