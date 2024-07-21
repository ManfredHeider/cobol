      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2638.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2638.CPv  $
      *
      *   Rev 2.2   Mar 01 2017 15:26:16   t13ex27
      *AKI01490: Tabelle Produkt aufräumen
      *
      *   Rev 2.1   Aug 14 2012 14:59:12   t13ex14
      *AKI01154: automatische Vergabe neue KI-Produktnummer
      *
      *   Rev 2.0   Jul 17 2012 15:05:49 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2638                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2638                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.02.2017 / 13:33:04 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2638
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI2638.
           05  PI-DRKI2638-KEY.


ANF   *        %M_PI_KEY%
             10 FILLER PIC X(0001).
END   *


           05  PI-DRKI2638-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2638-PRODUKTTEXT   PIC  X(0072).
END   *

      *
       01  PO-DRKI2638.
           05  PO-DRKI2638-RC                     PIC  X(003).
               88 PO-DRKI2638-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2638-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI2638-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2638-PRODUKTID   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER **************************************

