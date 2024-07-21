      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0922.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0922.CPv  $
      *
      *   Rev 2.1   Nov 03 2008 16:22:34   t13ae35
      *AVB01007: Behandlung der Stufe ALP
      *
      *   Rev 2.0   Nov 03 2008 15:31:44   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0922                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering
      *    DATUM:    03.11.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01007!03.11.08!2.1 ! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0922                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
      *
       01 PI-DRVB0922.
          05 PI-DRVB0922-MS-NR              PIC S9(004) COMP.
          05 PI-DRVB0922-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0922-ANZ-MONATE         PIC S9(004) COMP.
          05 PI-DRVB0922-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0922.
          05 PO-DRVB0922-RC                 PIC X(003).
             88 PO-DRVB0922-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0922-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0922-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0922-RC-NOT-FOUND                 VALUE 'NFD'.

      *
      ****************** ENDE COPY-MEMBER DRVB0922 *********************

