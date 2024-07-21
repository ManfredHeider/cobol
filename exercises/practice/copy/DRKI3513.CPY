      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3513.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3513.CPv  $
      *
      *   Rev 2.2   Jan 22 2019 14:37:08   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.1   Dec 19 2018 09:40:24   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Nov 05 2018 08:53:58   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3513                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3513                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2019 / 14:23:15 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3513
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3513.
           05  PI-DRKI3513-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3513-G11KDROLLE   PIC S9(4) COMP.
             10 PI-DRKI3513-G11KDSPARTE   PIC  X(0003).
             10 PI-DRKI3513-G11KDVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3513.
           05  PO-DRKI3513-RC                     PIC  X(003).
               88 PO-DRKI3513-RC-OK               VALUE 'OK '.
               88 PO-DRKI3513-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

