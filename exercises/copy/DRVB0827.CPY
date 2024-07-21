      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0827.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0827.CPv  $
      *
      *   Rev 2.1   Jun 21 2006 13:37:24   t13ae13
      *ASL00224: SL04 - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   Jun 09 2006 09:07:36   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0827                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    09.06.2006                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00224!09.06.06!2.1 ! NEUERSTELLUNG             ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0827                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0827.
          05 PI-DRVB0827-MS-NR              PIC S9(004) COMP.
          05 PI-DRVB0827-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0827.
          05 PO-DRVB0827-RC                 PIC X(003).
             88 PO-DRVB0827-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0827-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0827-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0827-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0827-DATEN.
             10 PO-DRVB0827-DATE-DAT-VON    PIC  X(010).
      *
      ****************** ENDE COPY-MEMBER DRVB0827 *********************
