      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3243.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3243.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 14:05:10   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 07:46:58   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 13 2017 12:45:36   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3243                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3243                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01534!13.12.17!2.1 ! NEUERSTELLUNG             ! SWO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3243
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3243.
           05 PI-DRKI3243-UFKT-CODE               PIC  9(002).
              88 PI-DRKI3243-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI3243-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI3243-UFKT-QUIT                  VALUE 3.
      *
           05 PI-DRKI3243-KEY.
              10 PI-DRKI3243-G20VMANDANT          PIC S9(009) BINARY.
      *
       01  PO-DRKI3243.
           05 PO-DRKI3243-RC                      PIC  X(003).
              88 PO-DRKI3243-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3243-RC-LEER                     VALUE 'LER'.
              88 PO-DRKI3243-RC-EOF                      VALUE 'EOF'.
      *
           05 PO-DRKI3243-DATEN.
              10 PO-DRKI3243-G20KVNR              PIC  X(015).
              10 PO-DRKI3243-G20KROLLE            PIC  X(004).
              10 PO-DRKI3243-G20KVERARB-KZN       PIC  X(001).
              10 PO-DRKI3243-G20KNACHNAME         PIC  X(040).
              10 PO-DRKI3243-G20KVORNAME          PIC  X(040).
              10 PO-DRKI3243-G20KGEBDAT           PIC  X(010).
              10 PO-DRKI3243-G20KANREDE           PIC S9(004) BINARY.
              10 PO-DRKI3243-G20KGESCHLECHT       PIC  X(001).
              10 PO-DRKI3243-G20KSTRASSE          PIC  X(040).
              10 PO-DRKI3243-G20KPOSTFACH         PIC  X(006).
              10 PO-DRKI3243-G20KAUSL-KZN         PIC  X(003).
              10 PO-DRKI3243-G20KPLZ              PIC  X(009).
              10 PO-DRKI3243-G20KORT              PIC  X(040).
              10 PO-DRKI3243-G20KIBAN             PIC  X(034).
              10 PO-DRKI3243-G20KBIC              PIC  X(011).
      *
      ******* ENDE COPY-MEMBER ***************************************
 

