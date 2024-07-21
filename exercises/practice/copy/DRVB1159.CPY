      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.01.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1159.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1159.CPv  $
      *
      *   Rev 2.1   Jan 25 2017 12:30:44   t13ex57
      *AVB01438: Anpassung Sonderdialoge
      *
      *   Rev 2.0   Jan 24 2017 13:22:54   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1159                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0029                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U. BOLL                                          *
      *    DATUM:    24.01.2017                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01438!24.11.17!2.1 ! NEU                       ! U. Boll   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1159 - ERMITTELN VB-DATEN FÜR SONDERDIALOGE   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1159.
           05  PI-DRVB1159-DATEN.
               10  PI-DRVB1159-MA-ST-NR          PIC S9(009) COMP.
      *
       01  PO-DRVB1159.
           05  PO-DRVB1159-RC                    PIC  X(003).
               88  PO-DRVB1159-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1159-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB1159-DATEN.
               10  PO-DRVB1159-ST-PRUEFZIFFER    PIC  X(001).
               10  PO-DRVB1159-DIR-NR            PIC S9(004) COMP.
               10  PO-DRVB1159-MS-BEZ-KURZ       PIC  X(004).
               10  PO-DRVB1159-MAN-NAME          PIC  X(040).
               10  PO-DRVB1159-MAN-VORNAME       PIC  X(020).
      ****
      **** ENDE COPY DRVB1159 ****************************************
      ****

