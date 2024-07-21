      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3391.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3391.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:46:32   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Sep 20 2018 11:04:17 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3391                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3391                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.09.2018 / 11:04:17 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3391
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3391.
           05  PI-DRKI3391-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3391-G42VVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3391-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3391-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3391-G42VANACHLASSKZ   PIC  X(0001).
             10 PI-DRKI3391-G42VANREDE   PIC  X(0001).
             10 PI-DRKI3391-G42VDEPOTSTATUS   PIC  X(0001).
             10 PI-DRKI3391-G42VGEBDAT   PIC  X(0008).
             10 PI-DRKI3391-G42VGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3391-G42VINFOSPERDAT   PIC  X(0008).
             10 PI-DRKI3391-G42VINFOSPRREKZ   PIC  X(0001).
             10 PI-DRKI3391-G42VLKZ   PIC  X(0003).
             10 PI-DRKI3391-G42VMANDANT   PIC S9(9) COMP.
             10 PI-DRKI3391-G42VNACHNAME   PIC  X(0035).
             10 PI-DRKI3391-G42VONLBANKING   PIC  X(0001).
             10 PI-DRKI3391-G42VORT   PIC  X(0040).
             10 PI-DRKI3391-G42VPLZ   PIC  X(0009).
             10 PI-DRKI3391-G42VSTRHNR   PIC  X(0040).
             10 PI-DRKI3391-G42VTELBANKING   PIC  X(0001).
             10 PI-DRKI3391-G42VVBNR   PIC S9(9) COMP.
             10 PI-DRKI3391-G42VVORNAME   PIC  X(0035).
             10 PI-DRKI3391-G42VVRTRGBEGINN   PIC  X(0008).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3391-NI1.
           05 PI-DRKI3391-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3391-NI7.
           05 PI-DRKI3391-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3391.
           05  PO-DRKI3391-RC                     PIC  X(003).
               88 PO-DRKI3391-RC-OK               VALUE 'OK '.
               88 PO-DRKI3391-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

