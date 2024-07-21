      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1998.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1998.CPv  $
      *
      *   Rev 2.1   Dec 06 2006 07:59:50   t13ex43
      *AKI00620: PGM - Migration der Telefon-Nummer
      *
      *   Rev 2.0   Nov 15 2006 12:19:02   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1998                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0198                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    17.11.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00620!17.11.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI1998 - NORMIERUNG VON RUFNUMMERN              *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI1998.
           05  PI-DRKI1998-DATEN.
               10  PI-DRKI1998-HERKUNFT          PIC  X(002).
                   88  PI-DRKI1998-HERKUNFT-KI        VALUE 'KI'.
                   88  PI-DRKI1998-HERKUNFT-VB        VALUE 'VB'.
               10  PI-DRKI1998-ART               PIC  X(001).
                   88  PI-DRKI1998-ART-HANDY          VALUE 'H'.
                   88  PI-DRKI1998-ART-FESTNETZ       VALUE 'F'.
               10  PI-DRKI1998-RUFNUMMER         PIC  X(128).
               10  PI-DRKI1998-KI-PERSONID       PIC  X(013).
               10  PI-DRKI1998-VB-MA-ST-NR       PIC S9(009) COMP.
      *
       01  PO-DRKI1998.
           05  PO-DRKI1998-RC                    PIC  X(003).
               88  PO-DRKI1998-RC-OK                  VALUE 'OK '.
               88  PO-DRKI1998-RC-KEINE-AENDERUNG     VALUE 'KAE'.
               88  PO-DRKI1998-RC-LEER                VALUE 'LER'.
           05  PO-DRKI1998-DATEN.
               10  PO-DRKI1998-RUFNUMMER-LEN     PIC S9(004) COMP.
               10  PO-DRKI1998-RUFNUMMER-NORM    PIC  X(128).
               10  PO-DRKI1998-HINWEIS-FEHLER    PIC  X(001).
                   88  PO-DRKI1998-HW-BLANK           VALUE SPACE.
                   88  PO-DRKI1998-HW-SONDERZ         VALUE 'S'.
                   88  PO-DRKI1998-HW-VBNR            VALUE 'V'.
                   88  PO-DRKI1998-HW-PERSONID        VALUE 'P'.
                   88  PO-DRKI1998-HW-ADRESSID        VALUE 'A'.
                   88  PO-DRKI1998-HW-BETREUER        VALUE 'B'.
                   88  PO-DRKI1998-HW-LANDVOR         VALUE 'L'.
      ****
      **** ENDE COPY DRKI1998 ****************************************
      ****
