      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0745.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0745.CPv  $
      *
      *   Rev 2.1   Feb 11 2005 11:39:14   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Dec 01 2004 15:14:22   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0745                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0745                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    01.12.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00513!01.12.04!    ! NEU                       ! P. Duerr *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0745: LESEN dir. + indir. Partner
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0745.
           05 PI-DRVB0745-PROGRAMM          PIC  X(008).
           05 PI-DRVB0745-UFKT-CODE         PIC  9(002).
           05 PI-DRVB0745-KNR-WB-TYP        PIC  S9(004)  COMP.
           05 PI-DRVB0745-KNR-WERTUNGS-TYP  PIC  S9(004)  COMP.
           05 PI-DRVB0745-KAND-NEU-AB       PIC  X(010).
      *
       01  PO-DRVB0745.

           05 PO-DRVB0745-RC                PIC  X(003).
             88 PO-DRVB0745-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0745-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0745-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0745-RC-ERR                       VALUE 'ERR'.

           05 PO-DRVB0745-DATEN.
             10 PO-DRVB0745-KAND-TUPEL-GESAMT PIC  X(085).
      *
      ******* ENDE COPY-MEMBER **************************************
