      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1206.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1206.CPv  $
      *
      *   Rev 2.1   Dec 11 2018 13:23:34   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Sep 25 2018 11:37:24   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1206 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    LESEN BEGINN HAUPTBERUF                                    *
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
      *    FÜR DIE FUNKTION DRVB1206                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1206.
          05 PI-DRVB1206-DATEN.
             10 PI-DRVB1206-VBNR       PIC S9(009) BINARY.
      *
       01 PO-DRVB1206.
          05 PO-DRVB1206-RC            PIC  X(003).
             88 PO-DRVB1206-RC-OK           VALUE 'OK '.
             88 PO-DRVB1206-RC-NOT-FOUND    VALUE 'NFD'.
             88 PO-DRVB1206-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRVB1206-DATEN.
             10 PO-DRVB1206-MSBZ-DAT-VON
                                       PIC  X(026).
      *
      ******* ENDE COPY-MEMBER **************************************

