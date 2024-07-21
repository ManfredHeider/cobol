      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3016.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3016.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:15:04   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:49:44   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jun 22 2015 17:18:55 T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3016                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3016                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.06.2015 / 17:18:55 / T13EX48
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01372!01.07.15! 01 ! Neu                       ! HSE      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3016
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3016.
           05  PI-DRKI3016-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3016-LKEKAPENTNNR   PIC S9(4) COMP.
             10 PI-DRKI3016-LKEVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI3016-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI3016-LKEKAPENTNBETRAG   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3016-LKEKAPENTNDAT   PIC  X(010).
END   *

      *
       01  PO-DRKI3016.
           05  PO-DRKI3016-RC                     PIC  X(003).
               88 PO-DRKI3016-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3016-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


