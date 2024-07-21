      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1196.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1196.CPv  $
      *
      *   Rev 2.2   06 Feb 2019 15:21:46   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.1   Dec 11 2018 13:20:38   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.0   Aug 31 2018 06:53:20   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1196 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    LESEN ED-GENERATION                                        *
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
      *    FÜR DIE FUNKTION DRVB1196                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1196.
          05 PI-DRVB1196-DATEN.
             10 PI-DRVB1196-VBNR       PIC S9(009) BINARY.
             10 PI-DRVB1196-SUCHE-ED   PIC  X(001).
      *
       01 PO-DRVB1196.
          05 PO-DRVB1196-RC            PIC  X(003).
             88 PO-DRVB1196-RC-OK           VALUE 'OK '.
             88 PO-DRVB1196-RC-NOT-FOUND    VALUE 'NFD'.
             88 PO-DRVB1196-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRVB1196-DATEN.
             10 PO-DRVB1196-MA-ST-NR   PIC S9(009) BINARY.
             10 PO-DRVB1196-MAN-TITEL-ADEL
                                       PIC  X(010).
             10 PO-DRVB1196-MAN-TITEL-AKAD
                                       PIC  X(010).
             10 PO-DRVB1196-MAN-VORNAME
                                       PIC  X(020).
             10 PO-DRVB1196-MAN-NAME
                                       PIC  X(040).
             10 PO-DRVB1196-MAN-GEBURTSDAT
                                       PIC  X(010).
             10 PO-DRVB1196-EDT-DAT-VON
                                       PIC  X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
 

