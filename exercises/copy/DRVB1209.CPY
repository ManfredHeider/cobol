      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.01.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1209.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1209.CPv  $
      *
      *   Rev 2.1   06 Feb 2019 15:21:12   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Jan 07 2019 11:22:24   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1209 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    ERMITTLUNG EDT-DAT-VON                                     *
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
      *    FÜR DIE FUNKTION DRVB1209                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1209.
          05 PI-DRVB1209-DATEN.
             10 PI-DRVB1209-EDNR       PIC S9(009) BINARY.
      *
       01 PO-DRVB1209.
          05 PO-DRVB1209-RC            PIC  X(003).
             88 PO-DRVB1209-RC-OK           VALUE 'OK '.
             88 PO-DRVB1209-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRVB1209-DATEN.
             10 PO-DRVB1209-EDT-DAT-VON
                                       PIC  X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
 

