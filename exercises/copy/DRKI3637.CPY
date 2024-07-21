      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3637.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3637.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 08:21:12   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.1   Mar 19 2019 12:43:12   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.0   Mar 18 2019 11:12:22 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3637                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3637                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.03.2019 / 11:12:22 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3637
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3637.
           05  PI-DRKI3637-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3637-G67TLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3637-G67TVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3637.
           05  PO-DRKI3637-RC                     PIC  X(003).
               88 PO-DRKI3637-RC-OK               VALUE 'OK '.
               88 PO-DRKI3637-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************


