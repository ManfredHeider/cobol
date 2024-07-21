      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3333.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3333.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:12:16   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Kunde
      *
      *   Rev 2.0   Mar 19 2018 07:30:06 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3333                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3333                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.03.2018 / 07:30:06 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3333
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3333.
           05  PI-DRKI3333-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3333-G70KVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3333-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3333-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3333-G70KABWBZ   PIC  X(0001).
             10 PI-DRKI3333-G70KAENDKZ   PIC  X(0002).
             10 PI-DRKI3333-G70KANREDEKZ   PIC  X(0002).
             10 PI-DRKI3333-G70KANREDEKZMI   PIC  X(0002).
             10 PI-DRKI3333-G70KBIC   PIC  X(0011).
             10 PI-DRKI3333-G70KBUNDLAND   PIC  X(0002).
             10 PI-DRKI3333-G70KFREIBETR   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3333-G70KGEBDAT   PIC  X(0008).
             10 PI-DRKI3333-G70KGEBDATMI   PIC  X(0008).
             10 PI-DRKI3333-G70KGESCHL   PIC  X(0001).
             10 PI-DRKI3333-G70KGESCHLMI   PIC  X(0001).
             10 PI-DRKI3333-G70KIBAN   PIC  X(0034).
             10 PI-DRKI3333-G70KLETZTAEND   PIC  X(0008).
             10 PI-DRKI3333-G70KLKZ   PIC  X(0003).
             10 PI-DRKI3333-G70KLKZMI   PIC  X(0003).
             10 PI-DRKI3333-G70KMANDREF   PIC  X(0035).
             10 PI-DRKI3333-G70KNACHNAME   PIC  X(0040).
             10 PI-DRKI3333-G70KNACHNAMEMI   PIC  X(0040).
             10 PI-DRKI3333-G70KORT   PIC  X(0040).
             10 PI-DRKI3333-G70KORTMI   PIC  X(0040).
             10 PI-DRKI3333-G70KPLZ   PIC  X(0009).
             10 PI-DRKI3333-G70KPLZMI   PIC  X(0009).
             10 PI-DRKI3333-G70KPOSTFACH   PIC  X(0045).
             10 PI-DRKI3333-G70KPOSTFACHMI   PIC  X(0045).
             10 PI-DRKI3333-G70KROLLEKZ   PIC  X(0002).
             10 PI-DRKI3333-G70KSTRASSE   PIC  X(0045).
             10 PI-DRKI3333-G70KSTRASSEMI   PIC  X(0045).
             10 PI-DRKI3333-G70KVORNAME   PIC  X(0030).
             10 PI-DRKI3333-G70KVORNAMEMI   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3333-NI1.
           05 PI-DRKI3333-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3333-NI7.
           05 PI-DRKI3333-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3333.
           05  PO-DRKI3333-RC                     PIC  X(003).
               88 PO-DRKI3333-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3333-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

