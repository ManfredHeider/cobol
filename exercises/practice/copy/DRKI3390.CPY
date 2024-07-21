      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3390.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3390.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:46:18   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Sep 20 2018 11:04:15 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3390                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3390                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.09.2018 / 11:04:15 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3390
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3390.
           05  PI-DRKI3390-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3390-G42VVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3390.
           05  PO-DRKI3390-RC                     PIC  X(003).
               88 PO-DRKI3390-RC-OK               VALUE 'OK '.
               88 PO-DRKI3390-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3390-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3390-G42VANACHLASSKZ   PIC  X(0001).
             10 PO-DRKI3390-G42VANREDE   PIC  X(0001).
             10 PO-DRKI3390-G42VDEPOTSTATUS   PIC  X(0001).
             10 PO-DRKI3390-G42VGEBDAT   PIC  X(0008).
             10 PO-DRKI3390-G42VGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3390-G42VINFOSPERDAT   PIC  X(0008).
             10 PO-DRKI3390-G42VINFOSPRREKZ   PIC  X(0001).
             10 PO-DRKI3390-G42VLKZ   PIC  X(0003).
             10 PO-DRKI3390-G42VMANDANT   PIC S9(9) COMP.
             10 PO-DRKI3390-G42VNACHNAME   PIC  X(0035).
             10 PO-DRKI3390-G42VONLBANKING   PIC  X(0001).
             10 PO-DRKI3390-G42VORT   PIC  X(0040).
             10 PO-DRKI3390-G42VPLZ   PIC  X(0009).
             10 PO-DRKI3390-G42VSTRHNR   PIC  X(0040).
             10 PO-DRKI3390-G42VTELBANKING   PIC  X(0001).
             10 PO-DRKI3390-G42VVBNR   PIC S9(9) COMP.
             10 PO-DRKI3390-G42VVORNAME   PIC  X(0035).
             10 PO-DRKI3390-G42VVRTRGBEGINN   PIC  X(0008).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

