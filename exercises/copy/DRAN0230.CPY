      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0230.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0230.CPv  $
      *
      *   Rev 2.4   Jul 30 2020 08:31:02   t13ae34
      *AAN01472: Ablösung Lotus-Notes - Produktmaßnahmendatenbank P
      *MDB durch Mendix
      *
      *   Rev 2.3   Jun 26 2009 10:46:10   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.2   Jun 15 2009 10:00:02   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.1   Jun 05 2009 14:38:14   i13az24
      *
      *   Rev 2.0   May 25 2009 09:49:44   i13az24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0230                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0230                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.07.2020 / 14:16:02 / T13AE34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01472!10.07.20!2.4 ! NEU MIT OZPGEN            ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0230
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0230.
           05  PI-DRAN0230-KEY.

             10 PI-DRAN0230-ANLZ-LFD-NR   PIC S9(4) COMP.
             10 PI-DRAN0230-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0230-PROD-PROD-SCHL   PIC S9(4) COMP.


           05  PI-DRAN0230-DATEN.

             10 PI-DRAN0230-ANLZ-GUELTIG-AB   PIC  X(010).
             10 PI-DRAN0230-ANLZ-GUELTIG-BIS   PIC  X(010).
             10 PI-DRAN0230-ANLZ-KENNUNG-UPD   PIC  X(0008).
             10 PI-DRAN0230-ANLZ-LZ-BIS   PIC S9(4) COMP.
             10 PI-DRAN0230-ANLZ-LZ-VON   PIC S9(4) COMP.
             10 PI-DRAN0230-ANLZ-SATZ   PIC S9(4)V9(4) COMP-3.
             10 PI-DRAN0230-ANLZ-ZEIT-UPD   PIC  X(0026).
      *
       01  PO-DRAN0230.
           05  PO-DRAN0230-RC                     PIC  X(003).
               88 PO-DRAN0230-RC-OK               VALUE 'OK '.
               88 PO-DRAN0230-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

