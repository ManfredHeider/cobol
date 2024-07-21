      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.01.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1211.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1211.CPv  $
      *
      *   Rev 2.1   06 Feb 2019 15:21:28   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Jan 21 2019 13:01:46   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1211 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    ERMITTLUNG MAT-DAT-BIS                                     *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02659!21.01.19!2.0 ! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB1211                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1211.
          05 PI-DRVB1211-DATEN.
             10 PI-DRVB1211-VBNR       PIC S9(009) BINARY.
      *
       01 PO-DRVB1211.
          05 PO-DRVB1211-RC            PIC  X(003).
             88 PO-DRVB1211-RC-OK           VALUE 'OK '.
             88 PO-DRVB1211-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRVB1211-DATEN.
             10 PO-DRVB1211-MAT-DAT-BIS
                                       PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
 

