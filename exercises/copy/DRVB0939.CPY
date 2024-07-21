      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0939.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0939.CPv  $
      *
      *   Rev 2.1   Jun 22 2009 14:46:38   t13ex13
      *AVB01041: PGM - VB46 Pflege DG Allfinanz
      *
      *   Rev 2.0   May 11 2009 15:44:40   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0939                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0026                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    11.05.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01041!11.05.09!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0939 - ERMITTELN MITGLIEDER EINER DIR-GEMEIN. *
      *                        DER ALLFINANZ                          *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0939.
           05  PI-DRVB0939-DATEN.
               10  PI-DRVB0939-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0939-DG-BEZ            PIC  X(010).
      *
       01  PO-DRVB0939.
           05  PO-DRVB0939-RC                    PIC  X(003).
               88  PO-DRVB0939-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0939-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0939-DATEN.
               10  PO-DRVB0939-VBER-BEZ          PIC  X(010).
               10  PO-DRVB0939-DIR-NR            PIC S9(004) COMP.
               10  PO-DRVB0939-DL-VBNR           PIC S9(009) COMP.
               10  PO-DRVB0939-DL-NAME           PIC  X(030).
               10  PO-DRVB0939-DIR-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB0939-MAST-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0939-DG-SPRECHER       PIC  X(001).
      ****
      **** ENDE COPY DRVB0939 ****************************************
      ****
