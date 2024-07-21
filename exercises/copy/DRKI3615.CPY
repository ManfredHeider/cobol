      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.01.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3615.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3615.CPv  $
      *
      *   Rev 2.1   Jan 22 2019 14:37:50   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Jan 22 2019 14:23:17 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3615                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3615                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2019 / 14:23:17 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3615
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3615.
           05  PI-DRKI3615-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3615-G11KDSPARTE   PIC  X(0003).
             10 PI-DRKI3615-G11KDVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3615.
           05  PO-DRKI3615-RC                     PIC  X(003).
               88 PO-DRKI3615-RC-OK               VALUE 'OK '.
               88 PO-DRKI3615-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3615-DATEN.
             10   PO-DRKI3615-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

