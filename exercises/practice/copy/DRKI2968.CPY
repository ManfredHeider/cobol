      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2968.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2968.CPv  $
      *
      *   Rev 2.4   Jul 07 2020 12:20:26   t13ex78
      *AKI01780: Standard OZP ZTKI0475 und notw. Recompiles
      *
      *   Rev 2.3   Mar 13 2018 08:24:38   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:10:48   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:46   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 12:11:27 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2968                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2968                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.07.2020 / 12:05:59 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2968
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2968.
           05  PI-DRKI2968-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2968-G90KVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI2968-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2968-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2968-G90KAENDKZ   PIC  X(0002).
             10 PI-DRKI2968-G90KANREDE   PIC S9(4) COMP.
             10 PI-DRKI2968-G90KAPANREDE   PIC S9(4) COMP.
             10 PI-DRKI2968-G90KAPEMAIL   PIC  X(0128).
             10 PI-DRKI2968-G90KAPFUNKTION   PIC  X(0050).
             10 PI-DRKI2968-G90KAPGEBDAT   PIC  X(010).
             10 PI-DRKI2968-G90KAPNACHNAME   PIC  X(0030).
             10 PI-DRKI2968-G90KAPTITEL   PIC S9(4) COMP.
             10 PI-DRKI2968-G90KAPVORNAME   PIC  X(0030).
             10 PI-DRKI2968-G90KFAXNR   PIC  X(0030).
             10 PI-DRKI2968-G90KGESID   PIC S9(4) COMP.
             10 PI-DRKI2968-G90KGRUENDDAT   PIC  X(010).
             10 PI-DRKI2968-G90KINHABER   PIC  X(0100).
             10 PI-DRKI2968-G90KMANDANT   PIC S9(9) COMP.
             10 PI-DRKI2968-G90KMOBILNR   PIC  X(0030).
             10 PI-DRKI2968-G90KNACHNAME   PIC  X(0030).
             10 PI-DRKI2968-G90KORT   PIC  X(0040).
             10 PI-DRKI2968-G90KPFORT   PIC  X(0040).
             10 PI-DRKI2968-G90KPFPLZ   PIC S9(9) COMP.
             10 PI-DRKI2968-G90KPFPOSTFACH   PIC S9(9) COMP.
             10 PI-DRKI2968-G90KPLZ   PIC S9(9) COMP.
             10 PI-DRKI2968-G90KRECHTSFORM   PIC S9(4) COMP.
             10 PI-DRKI2968-G90KSTRASSE   PIC  X(0040).
             10 PI-DRKI2968-G90KTELNR   PIC  X(0030).
             10 PI-DRKI2968-G90KVORNAME   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2968-NI1.
           05 PI-DRKI2968-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2968-NI7.
           05 PI-DRKI2968-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2968.
           05  PO-DRKI2968-RC                     PIC  X(003).
               88 PO-DRKI2968-RC-OK               VALUE 'OK '.
               88 PO-DRKI2968-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

