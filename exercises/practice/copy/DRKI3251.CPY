      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3251.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3251.CPv  $
      *
      *   Rev 2.1   Jun 07 2018 10:13:10   t13ae31
      *AKI01567: G10 Umstellung KSDS in DB2 OZP Kunde
      *
      *   Rev 2.0   Jan 30 2018 11:20:31 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3251                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3251                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.01.2018 / 11:56:42 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3251
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3251.
           05  PI-DRKI3251-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3251-G10KDROLLE   PIC S9(4) COMP.
             10 PI-DRKI3251-G10KDSPARTE   PIC  X(0003).
             10 PI-DRKI3251-G10KDVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3251-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3251-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3251-G10KDAENDKZ   PIC S9(4) COMP.
             10 PI-DRKI3251-G10KDANREDEKZ   PIC  X(0002).
             10 PI-DRKI3251-G10KDBIC   PIC  X(0011).
             10 PI-DRKI3251-G10KDGEBDAT   PIC  X(0008).
             10 PI-DRKI3251-G10KDGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3251-G10KDIBAN   PIC  X(0034).
             10 PI-DRKI3251-G10KDLKZ   PIC  X(0003).
             10 PI-DRKI3251-G10KDNACHNAME   PIC  X(0040).
             10 PI-DRKI3251-G10KDORT   PIC  X(0040).
             10 PI-DRKI3251-G10KDPLZ   PIC  X(0009).
             10 PI-DRKI3251-G10KDPOSTFACH   PIC  X(0045).
             10 PI-DRKI3251-G10KDSTRASSE   PIC  X(0045).
             10 PI-DRKI3251-G10KDVORNAME   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3251-NI1.
           05 PI-DRKI3251-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3251-NI7.
           05 PI-DRKI3251-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3251.
           05  PO-DRKI3251-RC                     PIC  X(003).
               88 PO-DRKI3251-RC-OK               VALUE 'OK '.
               88 PO-DRKI3251-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

