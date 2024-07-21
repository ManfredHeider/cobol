      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3042.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3042.CPv  $
      *
      *   Rev 2.4   Jan 11 2016 16:55:40   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Er
      *weiterung ZTKI0084 um KUBERLEDIGTKZ
      *
      *   Rev 2.3   Nov 09 2015 17:04:42   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.2   Nov 09 2015 16:50:36   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.1   Oct 28 2015 09:18:44   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.0   Oct 5 2015 13:24:33 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3042                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3042                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.01.2016 / 10:55:28 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3042
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3042.
           05  PI-DRKI3042-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3042-KUBPERSONENID   PIC  X(0013).
             10 PI-DRKI3042-KUBVBID   PIC  X(0013).
END   *
 
 
           05  PI-DRKI3042-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3042-KUBERLEDIGTKZ   PIC  X(0001).
             10 PI-DRKI3042-KUBTICKETID   PIC S9(18)V COMP-3.
END   *
 
 
      *
       01  PO-DRKI3042.
           05  PO-DRKI3042-RC                     PIC  X(003).
               88 PO-DRKI3042-RC-OK               VALUE 'OK '.
               88 PO-DRKI3042-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

