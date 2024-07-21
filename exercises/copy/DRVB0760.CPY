      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0760.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0760.CPv  $
      *
      *   Rev 2.1   Jul 07 2005 11:55:48   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Mar 10 2005 14:21:44   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0760                                     *
      *    FUNKTION ADMIRALS-CLUB KRITERIEN PRÜFEN                   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    08.03.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!08.03.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0760                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0760.
          05 PI-DRVB0760-PROGRAMM           PIC  X(008).
          05 PI-DRVB0760-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0760-MA-ST-NR           PIC  9(007).
          05 PI-DRVB0760-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVB0760.
          05 PO-DRVB0760-RC                 PIC  X(003).
             88 PO-DRVB0760-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0760-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0760-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0760-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0760-RC-ERR                       VALUE 'ERR'.

      *
      ******** Ende Copy-Member DRVB0760 *****************************
