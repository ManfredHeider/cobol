      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3238.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3238.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 14:02:38   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 07:36:30   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 13 2017 15:08:27 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3238                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3238                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.02.2018 / 16:10:41 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3238
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3238.
           05  PI-DRKI3238-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3238-G20KROLLE   PIC  X(0004).
             10 PI-DRKI3238-G20KVNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3238.
           05  PO-DRKI3238-RC                     PIC  X(003).
               88 PO-DRKI3238-RC-OK               VALUE 'OK '.
               88 PO-DRKI3238-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3238-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3238-G20KANREDE   PIC S9(4) COMP.
             10 PO-DRKI3238-G20KAUSL-KZN   PIC  X(0003).
             10 PO-DRKI3238-G20KBIC   PIC  X(0011).
             10 PO-DRKI3238-G20KGEBDAT   PIC  X(0010).
             10 PO-DRKI3238-G20KGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3238-G20KIBAN   PIC  X(0034).
             10 PO-DRKI3238-G20KNACHNAME   PIC  X(0040).
             10 PO-DRKI3238-G20KORT   PIC  X(0040).
             10 PO-DRKI3238-G20KPLZ   PIC  X(0009).
             10 PO-DRKI3238-G20KPOSTFACH   PIC  X(0006).
             10 PO-DRKI3238-G20KSTRASSE   PIC  X(0040).
             10 PO-DRKI3238-G20KVERARB-KZN   PIC  X(0001).
             10 PO-DRKI3238-G20KVORNAME   PIC  X(0040).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 

