      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0723.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0723.CPv  $
      *
      *   Rev 2.1   Sep 27 2006 09:30:10   t13ex43
      *AIS00060: PGM - Geldleistungen konfigurierbar machen
      *
      *   Rev 2.0   Jul 02 2004 15:59:20   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0723                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0100                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00060!08.08.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0723                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0723.
          05 PI-DRVB0723-KNR-GESELL-TYP     PIC S9(004) BINARY.
          05 PI-DRVB0723-FIBUZ-KONTO        PIC  X(012).
      *
       01 PO-DRVB0723.
          05 PO-DRVB0723-RC                 PIC  X(003).
             88 PO-DRVB0723-RC-OK                VALUE 'OK '.
             88 PO-DRVB0723-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0723-DATEN.
             10 PO-DRVB0723-KTY-LEISTUNGSTYP     PIC S9(004) BINARY.
             10 PO-DRVB0723-KNR-LEISTUNGSTYP     PIC S9(004) BINARY.
             10 PO-DRVB0723-KNR-BEZ              PIC  X(080).
             10 PO-DRVB0723-KNR-DATUM            PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0723 *****************************
      *
