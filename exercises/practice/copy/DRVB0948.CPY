      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0948.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0948.CPv  $
      *
      *   Rev 2.1   Nov 25 2009 15:14:36   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.0   Aug 14 2009 11:21:46   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0948                                     *
      *    FUNKTION KAPITÄNS-CLUB KRITERIEN PRÜFEN DVAG, WIEN        *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    14.08.2009                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00847!14.08.09! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0948                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0948.
          05 PI-DRVB0948-PROGRAMM           PIC  X(008).
          05 PI-DRVB0948-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0948-MA-ST-NR           PIC  9(007).
      *
       01 PO-DRVB0948.
          05 PO-DRVB0948-RC                 PIC  X(003).
             88 PO-DRVB0948-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0948-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0948-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0948-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0948-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVB0948 *****************************
