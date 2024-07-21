      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1199.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1199.CPv  $
      *
      *   Rev 2.1   Dec 11 2018 13:22:48   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Aug 31 2018 06:53:40   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1199 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    LESEN AKTUELLE STRUKTURSTUFE                               *
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
      *    FÜR DIE FUNKTION DRVB1199                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1199.
          05 PI-DRVB1199-DATEN.
             10 PI-DRVB1199-VBNR       PIC S9(009) BINARY.
      *
       01 PO-DRVB1199.
          05 PO-DRVB1199-RC            PIC  X(003).
             88 PO-DRVB1199-RC-OK           VALUE 'OK '.
             88 PO-DRVB1199-RC-NOT-FOUND    VALUE 'NFD'.
             88 PO-DRVB1199-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRVB1199-DATEN.
             10 PO-DRVB1199-MSNR       PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

