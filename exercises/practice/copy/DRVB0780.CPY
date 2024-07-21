      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0780.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0780.CPv  $
      *
      *   Rev 2.1   Jul 07 2005 11:58:20   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Jun 02 2005 17:07:08   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0780                                     *
      *    FUNKTION ADMIRALS-CLUB, AKTUELLE MITGLIEDER ERMITTELN     *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    06.11.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!02.06.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0780                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0780.
          05 PI-DRVB0780-PROGRAMM           PIC  X(008).
          05 PI-DRVB0780-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0780.
          05 PO-DRVB0780-RC                 PIC  X(003).
             88 PO-DRVB0780-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0780-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0780-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0780-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0780-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0780-DATEN.
             10 PO-DRVB0780-MA-ST-NR        PIC  9(007)  COMP-3.
      *
      ******** Ende Copy-Member DRVB0780 *****************************
