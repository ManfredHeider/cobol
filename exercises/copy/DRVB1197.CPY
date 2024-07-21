      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1197.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1197.CPv  $
      *
      *   Rev 2.2   06 Feb 2019 15:21:38   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.1   Dec 11 2018 13:21:22   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Aug 31 2018 06:53:28   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1197 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    ERMITTLUNG 1. ED-GENERATION                                *
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
      *    FÜR DIE FUNKTION DRVB1197                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1197.
          05 PI-DRVB1197-DATEN.
             10 PI-DRVB1197-ED-ST-NR   PIC S9(009) BINARY.
      *
       01 PO-DRVB1197.
          05 PO-DRVB1197-RC            PIC  X(003).
             88 PO-DRVB1197-RC-OK           VALUE 'OK '.
             88 PO-DRVB1197-RC-NOT-FOUND    VALUE 'NFD'.
             88 PO-DRVB1197-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRVB1197-DATEN.
             10 PO-DRVB1197-EDT-DAT-VON
                                       PIC  X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
 

