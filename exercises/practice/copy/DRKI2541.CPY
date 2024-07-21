      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2541.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2541.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:59:10   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Nov 22 2011 10:12:59 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2541                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2541                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.11.2011 / 10:12:59 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2541
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2541.
           05  PI-DRKI2541-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2541-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2541-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2541-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2541-KNDGEANDERTVON   PIC  X(0004).
             10 PI-DRKI2541-KNDNEUKUNDE   PIC  X(010).
             10 PI-DRKI2541-KNDSTATUSID   PIC  X(0001).
END   *


      *
       01  PO-DRKI2541.
           05  PO-DRKI2541-RC                     PIC  X(003).
               88 PO-DRKI2541-RC-OK               VALUE 'OK '.
               88 PO-DRKI2541-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
