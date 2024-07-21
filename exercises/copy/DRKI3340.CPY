      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3340.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3340.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:13:00   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Kunde
      *
      *   Rev 2.0   Mar 22 2018 08:43:12   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
120318* AKI1575 !12.03.18!    ! NEU                       ! P. Hübner*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3340
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3340.
           05  PI-DRKI3340-PROGRAMM     PIC X(008).
           05  PI-DRKI3340-UFKT-CODE    PIC 9(002).
 
           05  PI-DRKI3340-DATEN.
ANF   *        %M_PI_KEY%
             10 PI-DRKI3340-G70KMANDANT      PIC  S9(009) COMP.
END   *
 
      *
       01  PO-DRKI3340.
           05  PO-DRKI3340-RC                     PIC  X(003).
               88 PO-DRKI3340-RC-OK               VALUE 'OK '.
               88 PO-DRKI3340-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3340-RC-EOF              VALUE 'EOF'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3340-DATEN.
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3340-G70VVERTRAGSNR    PIC  X(0015).
             10 PO-DRKI3340-G70KVERTRAGSNR    PIC  X(0015).
             10 PO-DRKI3340-NI-G70KVERTRAGSNR PIC S9(4) COMP.
             10 PO-DRKI3340-G70KABWBZ         PIC  X(0001).
             10 PO-DRKI3340-G70KAENDKZ        PIC  X(0002).
             10 PO-DRKI3340-G70KANREDEKZ      PIC  X(0002).
             10 PO-DRKI3340-G70KANREDEKZMI    PIC  X(0002).
             10 PO-DRKI3340-G70KBIC           PIC  X(0011).
             10 PO-DRKI3340-G70KBUNDLAND      PIC  X(0002).
             10 PO-DRKI3340-G70KFREIBETR      PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3340-G70KGEBDAT        PIC  X(0008).
             10 PO-DRKI3340-G70KGEBDATMI      PIC  X(0008).
             10 PO-DRKI3340-G70KGESCHL        PIC  X(0001).
             10 PO-DRKI3340-G70KGESCHLMI      PIC  X(0001).
             10 PO-DRKI3340-G70KIBAN          PIC  X(0034).
             10 PO-DRKI3340-G70KLETZTAEND     PIC  X(0008).
             10 PO-DRKI3340-G70KLKZ           PIC  X(0003).
             10 PO-DRKI3340-G70KLKZMI         PIC  X(0003).
             10 PO-DRKI3340-G70KMANDREF       PIC  X(0035).
             10 PO-DRKI3340-G70KNACHNAME      PIC  X(0040).
             10 PO-DRKI3340-G70KNACHNAMEMI    PIC  X(0040).
             10 PO-DRKI3340-G70KORT           PIC  X(0040).
             10 PO-DRKI3340-G70KORTMI         PIC  X(0040).
             10 PO-DRKI3340-G70KPLZ           PIC  X(0009).
             10 PO-DRKI3340-G70KPLZMI         PIC  X(0009).
             10 PO-DRKI3340-G70KPOSTFACH      PIC  X(0045).
             10 PO-DRKI3340-G70KPOSTFACHMI    PIC  X(0045).
             10 PO-DRKI3340-G70KROLLEKZ       PIC  X(0002).
             10 PO-DRKI3340-G70KSTRASSE       PIC  X(0045).
             10 PO-DRKI3340-G70KSTRASSEMI     PIC  X(0045).
             10 PO-DRKI3340-G70KVORNAME       PIC  X(0030).
             10 PO-DRKI3340-G70KVORNAMEMI     PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3340-NI1.
           05 PI-DRKI3340-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3340-NI7.
           05 PI-DRKI3340-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
      *
      ******* ENDE COPY-MEMBER **************************************

