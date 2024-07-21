      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2620.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2620.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 16:16:38   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Adressen
      *
      *   Rev 2.0   Jun 08 2012 11:23:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2620                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2620                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.06.2012 / 11:20:17 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!08.06.12!2.5 ! Neue Kundenaktualisierung !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2620
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2620.
           05  PI-DRKI2620-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2620-ADRESSEID   PIC  X(0013).
END   *


           05  PI-DRKI2620-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2620-ADRESSEGEPRUEFT   PIC  X(0001).
             10 PI-DRKI2620-ADRESSELANDID   PIC  X(0003).
END   *


      *
       01  PO-DRKI2620.
           05  PO-DRKI2620-RC                     PIC  X(003).
               88 PO-DRKI2620-RC-OK               VALUE 'OK '.
               88 PO-DRKI2620-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER ***************************************
