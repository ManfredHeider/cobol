      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0759.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0759.CPv  $
      *
      *   Rev 2.1   Mar 15 2005 15:12:34   t13ex13
      *AAS00532: Basiszahlen Einheiten neuer VB ab 01.07.2003 je Re
      *gion
      *
      *   Rev 2.0   Mar 09 2005 18:10:22   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0759                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0021                        *
      *    FÜR DECLARE CURSOR MIT ORDER BY (ANALOG DRVB0536)          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R. GUTFLEISCH                                    *
      *    DATUM:    09.03.2005                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00532!09.03.05! 01 ! NEUERSTELLUNG             !GUTFLEISCH*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0759                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0759.
          05 PI-DRVB0759-PROGRAMM           PIC  X(008).
          05 PI-DRVB0759-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0759.

          05 PO-DRVB0759-RC                 PIC  X(003).
             88 PO-DRVB0759-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0759-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0759-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0759-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0759-DATEN.
             10 PO-DRVB0759-DIR-NR          PIC  S9(004)  COMP.
             10 PO-DRVB0759-DL-BEZUGS-NR    PIC  S9(009)  COMP.
             10 PO-DRVB0759-KNR-BEZUGS-TYP  PIC  S9(004)  COMP.
             10 PO-DRVB0759-REGION-BEZ10    PIC  X(010).
             10 PO-DRVB0759-KTY-REGION      PIC  S9(004)  COMP.
             10 PO-DRVB0759-KNR-REGION      PIC  S9(004)  COMP.
      *
      ***** ENDE-COPY-MEMBER *****************************************

