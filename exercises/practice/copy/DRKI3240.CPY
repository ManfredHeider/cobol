      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3240.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3240.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 14:03:38   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 07:37:28   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 13 2017 15:08:31 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3240                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3240                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.02.2018 / 16:10:44 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3240
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3240.
           05  PI-DRKI3240-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3240-G20KROLLE   PIC  X(0004).
             10 PI-DRKI3240-G20KVNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3240-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3240-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3240-G20KANREDE   PIC S9(4) COMP.
             10 PI-DRKI3240-G20KAUSL-KZN   PIC  X(0003).
             10 PI-DRKI3240-G20KBIC   PIC  X(0011).
             10 PI-DRKI3240-G20KGEBDAT   PIC  X(0010).
             10 PI-DRKI3240-G20KGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3240-G20KIBAN   PIC  X(0034).
             10 PI-DRKI3240-G20KNACHNAME   PIC  X(0040).
             10 PI-DRKI3240-G20KORT   PIC  X(0040).
             10 PI-DRKI3240-G20KPLZ   PIC  X(0009).
             10 PI-DRKI3240-G20KPOSTFACH   PIC  X(0006).
             10 PI-DRKI3240-G20KSTRASSE   PIC  X(0040).
             10 PI-DRKI3240-G20KVERARB-KZN   PIC  X(0001).
             10 PI-DRKI3240-G20KVORNAME   PIC  X(0040).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3240-NI1.
           05 PI-DRKI3240-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3240-NI7.
           05 PI-DRKI3240-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3240.
           05  PO-DRKI3240-RC                     PIC  X(003).
               88 PO-DRKI3240-RC-OK               VALUE 'OK '.
               88 PO-DRKI3240-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
 

