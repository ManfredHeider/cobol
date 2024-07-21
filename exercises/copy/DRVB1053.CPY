      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1053.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1053.CPv  $
      *
      *   Rev 2.2   Jun 05 2012 15:45:42   t13ae30
      *AVB01234: Anpassung der Adressanlieferung zu Anforderung #00
      *5949
      *
      *   Rev 2.1   Mar 30 2012 14:24:38   t13ae30
      *AVB01227: Erweiterung Host-Daten mit Privatadresse
      *
      *   Rev 2.0   Mar 29 2012 13:32:32   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1053                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    29.03.2012                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01227!29.03.12! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1053                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB1053.
          05 PI-DRVB1053-MAAN-BEZUGS-NR     PIC S9(009) COMP.
          05 PI-DRVB1053-KNR-MAAN-TYP       PIC S9(004) COMP.
      *
       01 PO-DRVB1053.
          05 PO-DRVB1053-RC                 PIC  X(003).
             88 PO-DRVB1053-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1053-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1053-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1053-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1053-DATEN.
             10 PO-DRVB1053-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB1053-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB1053-MAAN-ORT         PIC  X(040).
             10 PO-DRVB1053-AUSL             PIC  X(004).
             10 PO-DRVB1053-MAAN-TELEFAX     PIC  X(026).
             10 PO-DRVB1053-MAAN-TELEFON     PIC  X(026).
             10 PO-DRVB1053-KNR-GRND         PIC S9(004) USAGE COMP.
             10 PO-DRVB1053-KNR-ZUSTELLUNG   PIC S9(004) USAGE COMP.

      *
      ***** ENDE COPY-MEMBER DRVB1053 *****************************
