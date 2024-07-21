      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2926.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2926.CPv  $
      *
      *   Rev 2.2   Mar 02 2015 16:23:52   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.1   Sep 01 2014 10:35:50   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.0   Jul 16 2014 16:39:49 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2926                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2926                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.02.2015 / 16:05:51 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2926
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2926.
           05  PI-DRKI2926-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2926-FOPFONDSKUERZEL   PIC  X(0002).
             10 PI-DRKI2926-FOPPALETTENNAME   PIC  X(0008).
END   *
 
      *
       01  PO-DRKI2926.
           05  PO-DRKI2926-RC                     PIC  X(003).
               88 PO-DRKI2926-RC-OK               VALUE 'OK '.
               88 PO-DRKI2926-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

