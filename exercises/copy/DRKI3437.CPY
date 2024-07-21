      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3437.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3437.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 10:08:50   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.2   Oct 27 2018 15:20:58   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.1   Oct 25 2018 14:41:48   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.0   Sep 7 2018 11:16:27 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3437                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3437                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 09:51:39 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3437
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3437.
           05  PI-DRKI3437-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3437-G42BVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI3437-G42BWKN   PIC  X(0006).
END   *
 
      *
       01  PO-DRKI3437.
           05  PO-DRKI3437-RC                     PIC  X(003).
               88 PO-DRKI3437-RC-OK               VALUE 'OK '.
               88 PO-DRKI3437-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

