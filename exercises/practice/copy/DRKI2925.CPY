      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2925.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2925.CPv  $
      *
      *   Rev 2.2   Mar 02 2015 16:23:48   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.0   Jul 16 2014 16:36:53 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2925                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2925                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.07.2014 / 16:36:53 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2925
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2925.
           05  PI-DRKI2925-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2925-PALPALETTENNAME   PIC  X(0008).
END   *
 
      *
       01  PO-DRKI2925.
           05  PO-DRKI2925-RC                     PIC  X(003).
               88 PO-DRKI2925-RC-OK               VALUE 'OK '.
               88 PO-DRKI2925-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2925-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2925-PALFONDAKT   PIC  X(0001).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

