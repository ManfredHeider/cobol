      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0949.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0949.CPv  $
      *
      *   Rev 2.1   Nov 25 2009 15:15:10   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.0   Aug 14 2009 14:30:48   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0949                                     *
      *    FUNKTION KAPITÄNS-CLUB, DVAG, WIEN                        *
      *    AKTUELLE MITGLIEDER ERMITTELN                             *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    14.08.2009                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00847!14.08.09! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0949                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0949.
          05 PI-DRVB0949-PROGRAMM           PIC  X(008).
          05 PI-DRVB0949-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0949.
          05 PO-DRVB0949-RC                 PIC  X(003).
             88 PO-DRVB0949-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0949-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0949-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0949-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0949-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0949-DATEN.
             10 PO-DRVB0949-MA-ST-NR        PIC  9(007)  COMP-3.
      *
      ******** Ende Copy-Member DRVB0949 *****************************
