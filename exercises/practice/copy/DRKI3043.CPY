      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3043.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3043.CPv  $
      *
      *   Rev 2.4   Jan 11 2016 16:55:46   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Er
      *weiterung ZTKI0084 um KUBERLEDIGTKZ
      *
      *   Rev 2.3   Nov 09 2015 17:04:48   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.2   Nov 09 2015 16:50:40   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.1   Oct 28 2015 09:18:50   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.0   Oct 5 2015 13:24:34 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3043                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3043                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.01.2016 / 10:55:30 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3043
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3043.
           05  PI-DRKI3043-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3043-KUBPERSONENID   PIC  X(0013).
             10 PI-DRKI3043-KUBVBID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3043.
           05  PO-DRKI3043-RC                     PIC  X(003).
               88 PO-DRKI3043-RC-OK               VALUE 'OK '.
               88 PO-DRKI3043-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

