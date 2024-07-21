      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2966.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2966.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 08:23:04   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:10:38   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:36   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 12:11:23 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2966                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2966                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.02.2018 / 10:44:22 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2966
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2966.
           05  PI-DRKI2966-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2966-G90KVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI2966-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2966-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2966-G90KAENDKZ   PIC  X(0002).
             10 PI-DRKI2966-G90KANREDE   PIC S9(4) COMP.
             10 PI-DRKI2966-G90KAPANREDE   PIC S9(4) COMP.
             10 PI-DRKI2966-G90KAPEMAIL   PIC  X(0128).
             10 PI-DRKI2966-G90KAPFUNKTION   PIC  X(0050).
             10 PI-DRKI2966-G90KAPGEBDAT   PIC  X(010).
             10 PI-DRKI2966-G90KAPNACHNAME   PIC  X(0030).
             10 PI-DRKI2966-G90KAPTITEL   PIC S9(4) COMP.
             10 PI-DRKI2966-G90KAPVORNAME   PIC  X(0030).
             10 PI-DRKI2966-G90KFAXNR   PIC  X(0030).
             10 PI-DRKI2966-G90KGESID   PIC S9(4) COMP.
             10 PI-DRKI2966-G90KGRUENDDAT   PIC  X(010).
             10 PI-DRKI2966-G90KINHABER   PIC  X(0100).
             10 PI-DRKI2966-G90KMANDANT   PIC S9(9) COMP.
             10 PI-DRKI2966-G90KMOBILNR   PIC  X(0030).
             10 PI-DRKI2966-G90KNACHNAME   PIC  X(0030).
             10 PI-DRKI2966-G90KORT   PIC  X(0040).
             10 PI-DRKI2966-G90KPFORT   PIC  X(0040).
             10 PI-DRKI2966-G90KPFPLZ   PIC S9(9) COMP.
             10 PI-DRKI2966-G90KPFPOSTFACH   PIC S9(9) COMP.
             10 PI-DRKI2966-G90KPLZ   PIC S9(9) COMP.
             10 PI-DRKI2966-G90KRECHTSFORM   PIC S9(4) COMP.
             10 PI-DRKI2966-G90KSTRASSE   PIC  X(0040).
             10 PI-DRKI2966-G90KTELNR   PIC  X(0030).
             10 PI-DRKI2966-G90KVORNAME   PIC  X(0030).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2966-NI1.
           05 PI-DRKI2966-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2966-NI7.
           05 PI-DRKI2966-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI2966.
           05  PO-DRKI2966-RC                     PIC  X(003).
               88 PO-DRKI2966-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2966-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

