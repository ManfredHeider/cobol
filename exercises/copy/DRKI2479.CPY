      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2479.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2479.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:58:50   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Oct 17 2011 18:32:42 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2479                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2479                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.10.2011 / 18:32:42 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2479
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2479.
           05  PI-DRKI2479-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2479-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2479-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2479-KNDALTKUNDE   PIC  X(010).
             10 PI-DRKI2479-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2479-KNDGEANDERTVON   PIC  X(0004).
             10 PI-DRKI2479-KNDSTATUSID   PIC  X(0001).
END   *


      *
       01  PO-DRKI2479.
           05  PO-DRKI2479-RC                     PIC  X(003).
               88 PO-DRKI2479-RC-OK               VALUE 'OK '.
               88 PO-DRKI2479-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
