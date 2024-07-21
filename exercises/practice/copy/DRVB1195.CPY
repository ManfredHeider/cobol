      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1195.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1195.CPv  $
      *
      *   Rev 2.1   Dec 11 2018 13:19:54   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Aug 31 2018 06:53:14   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1195 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    LESEN STELLE UND TYP                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02659!24.07.18!2.0 ! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB1195                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1195.
          05 PI-DRVB1195-DATEN.
             10 PI-DRVB1195-VBNR       PIC S9(009) BINARY.
      *
       01 PO-DRVB1195.
          05 PO-DRVB1195-RC            PIC  X(003).
             88 PO-DRVB1195-RC-OK           VALUE 'OK '.
             88 PO-DRVB1195-RC-NOT-FOUND    VALUE 'NFD'.
             88 PO-DRVB1195-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRVB1195-DATEN.
             10 PO-DRVB1195-KNR-ST-TYP PIC S9(004) BINARY.
             10 PO-DRVB1195-KNR-GESELL-TYP
                                       PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

