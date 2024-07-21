      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2989.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2989.CPv  $
      *
      *   Rev 2.1   Mar 21 2016 16:18:34   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)
      *
      *   Rev 2.0   Feb 26 2015 10:09:44   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2989                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2989                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.11.2015 / 17:04:00 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01400!24.11.15!    ! NEU                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2989
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2989.
           05  PI-DRKI2989-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2989-VRBPERSONENID   PIC  X(0013).
             10 PI-DRKI2989-VRBPERSONENROLLEKZ   PIC  X(0001).
             10 PI-DRKI2989-VRBVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2989-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2989-VRBABLAUFDATUM   PIC  X(010).
             10 PI-DRKI2989-VRBAVKENNZEICHEN   PIC  X(0001).
             10 PI-DRKI2989-VRBGUELTIGKEITSDAT   PIC  X(010).
             10 PI-DRKI2989-VRBKOMMBETRKZ   PIC  X(0001).
END   *

      *
       01  PO-DRKI2989.
           05  PO-DRKI2989-RC                     PIC  X(003).
               88 PO-DRKI2989-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2989-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

