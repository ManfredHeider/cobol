      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2565.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2565.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:59:28   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Feb 21 2012 16:02:48 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2565                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2565                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.02.2012 / 16:02:48 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2565
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2565.
           05  PI-DRKI2565-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2565-KNDPERSONENID   PIC  X(0013).
END   *


           05  PI-DRKI2565-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2565-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2565-KNDGEANDERTVON   PIC  X(0004).
             10 PI-DRKI2565-KNDGEBGRUENDDAT   PIC  X(010).
END   *


      *
       01  PO-DRKI2565.
           05  PO-DRKI2565-RC                     PIC  X(003).
               88 PO-DRKI2565-RC-OK               VALUE 'OK '.
               88 PO-DRKI2565-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
