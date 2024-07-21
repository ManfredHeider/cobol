      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3510.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3510.CPv  $
      *
      *   Rev 2.2   Jan 22 2019 14:36:38   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.1   Dec 19 2018 09:39:56   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Nov 05 2018 08:53:34   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3510                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3510                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2019 / 14:23:09 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3510
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3510.
           05  PI-DRKI3510-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3510-G11KDROLLE   PIC S9(4) COMP.
             10 PI-DRKI3510-G11KDSPARTE   PIC  X(0003).
             10 PI-DRKI3510-G11KDVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3510.
           05  PO-DRKI3510-RC                     PIC  X(003).
               88 PO-DRKI3510-RC-OK               VALUE 'OK '.
               88 PO-DRKI3510-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3510-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3510-G11KDAENDKZ   PIC S9(4) COMP.
             10 PO-DRKI3510-G11KDANREDEKZ   PIC  X(0002).
             10 PO-DRKI3510-G11KDBIC   PIC  X(0011).
             10 PO-DRKI3510-G11KDGEBDAT   PIC  X(0008).
             10 PO-DRKI3510-G11KDGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3510-G11KDIBAN   PIC  X(0034).
             10 PO-DRKI3510-G11KDLKZ   PIC  X(0003).
             10 PO-DRKI3510-G11KDNACHNAME   PIC  X(0040).
             10 PO-DRKI3510-G11KDORT   PIC  X(0040).
             10 PO-DRKI3510-G11KDPLZ   PIC  X(0009).
             10 PO-DRKI3510-G11KDPOSTFACH   PIC  X(0045).
             10 PO-DRKI3510-G11KDSTRASSE   PIC  X(0045).
             10 PO-DRKI3510-G11KDVORNAME   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

