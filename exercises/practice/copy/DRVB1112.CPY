      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1112.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1112.CPv  $
      *
      *   Rev 2.1   Jun 23 2015 17:19:38   t13ae35
      *ASL00537: Familienabsicherungsplan - Prüfung der Selbstbetei
      *ligungen - Verbesserungen
      *
      *   Rev 2.0   Jun 22 2015 11:29:46   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1112                                     *
      *    FUNKTION ERMITTELN MAST-TYP zum Zeitpunkt                 *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    22.06.2015                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00537!22.06.15! 01 ! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1112                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1112.
          05 PI-DRVB1112-MA-ST-NR           PIC S9(009)    BINARY.
          05 PI-DRVB1112-KTY-MAST-TYP       PIC S9(004)    BINARY.
          05 PI-DRVB1112-KNR-MAST-TYP       PIC S9(004)    BINARY.
          05 PI-DRVB1112-ZEITPUNKT-ISO      PIC X(10).
      *
       01 PO-DRVB1112.
          05 PO-DRVB1112-RC                 PIC  X(005).
             88 PO-DRVB1112-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1112-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1112-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1112-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1112-RC-EOF                       VALUE 'EOF'.
 
      *
      ******** Ende Copy-Member DRVB1112 *****************************

