      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3752.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3752.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 12:40:30   t13ex78
      *AKI01731: Standard OZP G66_KUNDE
      *
      *   Rev 2.0   Jan 09 2020 10:16:46   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3752                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3752                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2020 / 11:44:53 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3752
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3752.
           05  PI-DRKI3752-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3752-G66KLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3752-G66KVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3752-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3752-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3752-G66KANREDE   PIC  X(0001).
             10 PI-DRKI3752-G66KGEBURTSDATUM   PIC  X(0008).
             10 PI-DRKI3752-G66KISOCODE   PIC  X(0003).
             10 PI-DRKI3752-G66KNAME   PIC  X(0035).
             10 PI-DRKI3752-G66KORT   PIC  X(0030).
             10 PI-DRKI3752-G66KPLZ   PIC  X(0009).
             10 PI-DRKI3752-G66KSTRHNR   PIC  X(0040).
             10 PI-DRKI3752-G66KVORNAME   PIC  X(0035).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3752-NI1.
           05 PI-DRKI3752-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3752-NI7.
           05 PI-DRKI3752-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3752.
           05  PO-DRKI3752-RC                     PIC  X(003).
               88 PO-DRKI3752-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3752-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

