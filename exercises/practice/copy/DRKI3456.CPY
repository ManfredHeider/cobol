      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3456.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3456.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:42:52   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.2   Oct 27 2018 15:25:54   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.1   Sep 19 2018 11:48:06   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.0   Sep 12 2018 07:56:56 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3456                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3456                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 10:00:08 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3456
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3456.
           05  PI-DRKI3456-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3456-G42FVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3456.
           05  PO-DRKI3456-RC                     PIC  X(003).
               88 PO-DRKI3456-RC-OK               VALUE 'OK '.
               88 PO-DRKI3456-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

