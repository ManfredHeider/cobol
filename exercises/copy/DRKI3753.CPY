      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3753.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3753.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 12:41:16   t13ex78
      *AKI01731: Standard OZP G66_KUNDE
      *
      *   Rev 2.0   Jan 09 2020 10:16:58   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3753                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3753                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2020 / 11:44:55 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3753
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3753.
           05  PI-DRKI3753-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3753-G66KLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3753-G66KVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3753-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3753-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3753-G66KANREDE   PIC  X(0001).
             10 PI-DRKI3753-G66KGEBURTSDATUM   PIC  X(0008).
             10 PI-DRKI3753-G66KISOCODE   PIC  X(0003).
             10 PI-DRKI3753-G66KNAME   PIC  X(0035).
             10 PI-DRKI3753-G66KORT   PIC  X(0030).
             10 PI-DRKI3753-G66KPLZ   PIC  X(0009).
             10 PI-DRKI3753-G66KSTRHNR   PIC  X(0040).
             10 PI-DRKI3753-G66KVORNAME   PIC  X(0035).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3753-NI1.
           05 PI-DRKI3753-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3753-NI7.
           05 PI-DRKI3753-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3753.
           05  PO-DRKI3753-RC                     PIC  X(003).
               88 PO-DRKI3753-RC-OK               VALUE 'OK '.
               88 PO-DRKI3753-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

