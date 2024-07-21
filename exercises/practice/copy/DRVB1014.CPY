      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1014.CPv  $
      *
      *   Rev 2.1   May 09 2011 10:00:36   t13ex43
      *AVB01168: PGM - Pflege Orga-Betreuer (VB47)
      *
      *   Rev 2.0   Apr 27 2011 11:40:16   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1014                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0035                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.04.2011                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01168!27.04.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1014 - ERMITTELN ORGA-BETREUER FÜR DIR-GEM.   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1014.
           05  PI-DRVB1014-DATEN.
               10  PI-DRVB1014-UFKT-CODE         PIC  9(002).
               10  PI-DRVB1014-KTY-NAMZ-TYP      PIC S9(004) COMP.
      *
       01  PO-DRVB1014.
           05  PO-DRVB1014-RC                    PIC  X(003).
               88  PO-DRVB1014-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1014-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB1014-DATEN.
               10  PO-DRVB1014-KTY-NAMZ-TYP      PIC S9(004) COMP.
               10  PO-DRVB1014-KNR-NAMZ-TYP      PIC S9(004) COMP.
               10  PO-DRVB1014-NAMZ-LFD          PIC S9(004) COMP.
               10  PO-DRVB1014-KTY-NAMZ-FUNK     PIC S9(004) COMP.
               10  PO-DRVB1014-KNR-NAMZ-FUNK     PIC S9(004) COMP.
               10  PO-DRVB1014-NAMZ-DAT-VON      PIC  X(026).
               10  PO-DRVB1014-NAMZ-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB1014-DG-BEZ            PIC  X(010).
               10  PO-DRVB1014-ANR-BEZ           PIC  X(004).
               10  PO-DRVB1014-TITEL             PIC  X(010).
               10  PO-DRVB1014-VNAME             PIC  X(015).
               10  PO-DRVB1014-NNAME             PIC  X(020).
      ****
      **** ENDE COPY DRVB1014 ****************************************
      ****
