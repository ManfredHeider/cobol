      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3511.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3511.CPv  $
      *
      *   Rev 2.2   Jan 22 2019 14:36:50   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.1   Dec 19 2018 09:40:04   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Nov 05 2018 08:53:42   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3511                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3511                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2019 / 14:23:11 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3511
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3511.
           05  PI-DRKI3511-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3511-G11KDROLLE   PIC S9(4) COMP.
             10 PI-DRKI3511-G11KDSPARTE   PIC  X(0003).
             10 PI-DRKI3511-G11KDVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3511-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3511-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3511-G11KDAENDKZ   PIC S9(4) COMP.
             10 PI-DRKI3511-G11KDANREDEKZ   PIC  X(0002).
             10 PI-DRKI3511-G11KDBIC   PIC  X(0011).
             10 PI-DRKI3511-G11KDGEBDAT   PIC  X(0008).
             10 PI-DRKI3511-G11KDGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3511-G11KDIBAN   PIC  X(0034).
             10 PI-DRKI3511-G11KDLKZ   PIC  X(0003).
             10 PI-DRKI3511-G11KDNACHNAME   PIC  X(0040).
             10 PI-DRKI3511-G11KDORT   PIC  X(0040).
             10 PI-DRKI3511-G11KDPLZ   PIC  X(0009).
             10 PI-DRKI3511-G11KDPOSTFACH   PIC  X(0045).
             10 PI-DRKI3511-G11KDSTRASSE   PIC  X(0045).
             10 PI-DRKI3511-G11KDVORNAME   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3511-NI1.
           05 PI-DRKI3511-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3511-NI7.
           05 PI-DRKI3511-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3511.
           05  PO-DRKI3511-RC                     PIC  X(003).
               88 PO-DRKI3511-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3511-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

