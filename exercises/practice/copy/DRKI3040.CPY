      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3040.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3040.CPv  $
      *
      *   Rev 2.4   Jan 11 2016 16:55:30   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Er
      *weiterung ZTKI0084 um KUBERLEDIGTKZ
      *
      *   Rev 2.3   Nov 09 2015 17:04:30   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.2   Nov 09 2015 16:50:24   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.1   Oct 28 2015 09:18:32   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
      *andard-OZP für TCKI0084
      *
      *   Rev 2.0   Oct 5 2015 13:24:29 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3040                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3040                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.01.2016 / 10:55:24 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3040
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3040.
           05  PI-DRKI3040-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3040-KUBPERSONENID   PIC  X(0013).
             10 PI-DRKI3040-KUBVBID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3040.
           05  PO-DRKI3040-RC                     PIC  X(003).
               88 PO-DRKI3040-RC-OK               VALUE 'OK '.
               88 PO-DRKI3040-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3040-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3040-KUBERLEDIGTKZ   PIC  X(0001).
             10 PO-DRKI3040-KUBTICKETID   PIC S9(18)V COMP-3.
             10 PO-DRKI3040-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

