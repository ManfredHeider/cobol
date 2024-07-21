      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0883.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0883.CPv  $
      *
      *   Rev 2.3   Jun 24 2015 15:19:34   t13ex71
      *ASL00479: Rückstufungen in Nebenberuf bzw. Sollsaldo in PMRW
      *0018
      *
      *   Rev 2.2   18 Feb 2008 11:22:16   t13ex43
      *ASL00292: PGM - Auszahlung Direktions-Aufbau-Plan
      *
      *   Rev 2.1   14 Jan 2008 13:01:42   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   10 Dec 2007 13:58:00   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0883                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0030                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.12.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!10.12.07!2.1 ! NEU                       ! KLEEMEIER *
180208* ASL00292!18.02.08!2.2 ! BES-BEZUGS-NR HINZU       ! KLEEMEIER *
050615* ASL00479!05.06.15!2.3 ! RÜCKSTUFUNG IN NEBENBERUF ! B.PRETSCH*
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0883 - ERMITTELN AUSZAHLUNGSHINDERNISSE       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0883.
           05  PI-DRVB0883-DATEN.
               10  PI-DRVB0883-MA-ST-NR          PIC S9(009) COMP.
               10  PI-DRVB0883-KNR-BES-GRND      PIC S9(004) COMP.
180208         10  PI-DRVB0883-BES-BEZUGS-NR     PIC S9(009) COMP.
               10  PI-DRVB0883-KNR-BV-TYP        PIC S9(004) COMP.
      *
       01  PO-DRVB0883.
           05  PO-DRVB0883-RC                    PIC  X(003).
               88  PO-DRVB0883-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0883-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0883-DATEN.
               10  PO-DRVB0883-S-AUSTRITT        PIC  X(001).
                   88  PO-DRVB0883-S-AUSTRITT-JA      VALUE 'J'.
                   88  PO-DRVB0883-S-AUSTRITT-NEIN    VALUE 'N'.
050615         10  PO-DRVB0883-S-RCKST-NB        PIC  X(001).
050615             88  PO-DRVB0883-S-RCKST-NB-JA      VALUE 'J'.
050615             88  PO-DRVB0883-S-RCKST-NB-NEIN    VALUE 'N'.
               10  PO-DRVB0883-S-BESCHR          PIC  X(001).
                   88  PO-DRVB0883-S-BESCHR-JA        VALUE 'J'.
                   88  PO-DRVB0883-S-BESCHR-NEIN      VALUE 'N'.
               10  PO-DRVB0883-S-BV-FEHLT        PIC  X(001).
                   88  PO-DRVB0883-S-BV-FEHLT-JA      VALUE 'J'.
                   88  PO-DRVB0883-S-BV-FEHLT-NEIN    VALUE 'N'.
               10  PO-DRVB0883-S-PFAEND          PIC  X(001).
                   88  PO-DRVB0883-S-PFAEND-JA        VALUE 'J'.
                   88  PO-DRVB0883-S-PFAEND-NEIN      VALUE 'N'.
               10  PO-DRVB0883-S-SPERRE          PIC  X(001).
                   88  PO-DRVB0883-S-SPERRE-JA        VALUE 'J'.
                   88  PO-DRVB0883-S-SPERRE-NEIN      VALUE 'N'.
               10  PO-DRVB0883-TEXT-PF-SPERRE    PIC  X(040).
      ****
      **** ENDE COPY DRVB0883 ****************************************
      ****

