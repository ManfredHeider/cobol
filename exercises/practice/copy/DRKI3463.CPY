      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3463.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3463.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:51:26   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.2   Oct 27 2018 15:29:16   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.1   Sep 21 2018 10:14:34   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.0   Sep 12 2018 08:06:08 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3463                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3463                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 10:20:27 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3463
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3463.
           05  PI-DRKI3463-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3463-G42PVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3463.
           05  PO-DRKI3463-RC                     PIC  X(003).
               88 PO-DRKI3463-RC-OK               VALUE 'OK '.
               88 PO-DRKI3463-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

