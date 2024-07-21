      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3046.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3046.CPv  $
      *
      *   Rev 2.3   Jan 11 2016 16:55:52   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Er
      *weiterung ZTKI0084 um KUBERLEDIGTKZ
      *
      *   Rev 2.2   Nov 09 2015 17:05:14   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.1   Nov 09 2015 16:50:56   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.0   Nov 9 2015 16:47:33 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3046                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3046                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.01.2016 / 10:55:32 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3046
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  COUNT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3046.
           05  PI-DRKI3046-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI3046-KUBPERSONENID   PIC  X(0013).
END   *
      *
       01  PO-DRKI3046.
           05  PO-DRKI3046-RC                     PIC  X(003).
               88 PO-DRKI3046-RC-OK               VALUE 'OK '.
               88 PO-DRKI3046-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3046-DATEN.
             10   PO-DRKI3046-COUNTER           PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

