      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2967.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2967.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 08:23:50   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:10:42   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:42   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 12:11:25 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2967                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2967                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.02.2018 / 16:10:42 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2967
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2967.
           05  PI-DRKI2967-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2967-G90KVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI2967.
           05  PO-DRKI2967-RC                     PIC  X(003).
               88 PO-DRKI2967-RC-OK               VALUE 'OK '.
               88 PO-DRKI2967-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI2967-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2967-G90KAENDKZ   PIC  X(0002).
             10 PO-DRKI2967-G90KANREDE   PIC S9(4) COMP.
             10 PO-DRKI2967-G90KAPANREDE   PIC S9(4) COMP.
             10 PO-DRKI2967-G90KAPEMAIL   PIC  X(0128).
             10 PO-DRKI2967-G90KAPFUNKTION   PIC  X(0050).
             10 PO-DRKI2967-G90KAPGEBDAT   PIC  X(010).
             10 PO-DRKI2967-G90KAPNACHNAME   PIC  X(0030).
             10 PO-DRKI2967-G90KAPTITEL   PIC S9(4) COMP.
             10 PO-DRKI2967-G90KAPVORNAME   PIC  X(0030).
             10 PO-DRKI2967-G90KFAXNR   PIC  X(0030).
             10 PO-DRKI2967-G90KGESID   PIC S9(4) COMP.
             10 PO-DRKI2967-G90KGRUENDDAT   PIC  X(010).
             10 PO-DRKI2967-G90KINHABER   PIC  X(0100).
             10 PO-DRKI2967-G90KMOBILNR   PIC  X(0030).
             10 PO-DRKI2967-G90KNACHNAME   PIC  X(0030).
             10 PO-DRKI2967-G90KORT   PIC  X(0040).
             10 PO-DRKI2967-G90KPFORT   PIC  X(0040).
             10 PO-DRKI2967-G90KPFPLZ   PIC S9(9) COMP.
             10 PO-DRKI2967-G90KPFPOSTFACH   PIC S9(9) COMP.
             10 PO-DRKI2967-G90KPLZ   PIC S9(9) COMP.
             10 PO-DRKI2967-G90KRECHTSFORM   PIC S9(4) COMP.
             10 PO-DRKI2967-G90KSTRASSE   PIC  X(0040).
             10 PO-DRKI2967-G90KTELNR   PIC  X(0030).
             10 PO-DRKI2967-G90KVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

