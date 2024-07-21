      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3084.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3084.CPv  $
      *
      *   Rev 2.3   Dec 06 2016 15:13:18   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:16:06   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:34:42   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.0   Sep 1 2016 12:05:41 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3084                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3084                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.09.2016 / 12:05:41 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3084
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3084.
           05  PI-DRKI3084-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3084-VBSPERSONENID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3084.
           05  PO-DRKI3084-RC                     PIC  X(003).
               88 PO-DRKI3084-RC-OK               VALUE 'OK '.
               88 PO-DRKI3084-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************



