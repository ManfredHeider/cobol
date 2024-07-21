      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0228.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0228.CPv  $
      *
      *   Rev 2.4   Jul 30 2020 08:30:18   t13ae34
      *AAN01472: Ablösung Lotus-Notes - Produktmaßnahmendatenbank P
      *MDB durch Mendix
      *
      *   Rev 2.3   Jun 26 2009 10:45:58   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.2   Jun 15 2009 09:55:36   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.1   Jun 05 2009 14:37:54   i13az24
      *
      *   Rev 2.0   May 25 2009 09:49:12   i13az24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0228                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0228                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.07.2020 / 14:16:00 / T13AE34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01472!10.07.20!2.4 ! NEU MIT OZPGEN            ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0228
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      *    ANMERKUNG: ANLZ_LFD_NR WIRD DURCH TRIGGER ERMITTELT
      *               ANLZ_GUELTIG_BIS='31.12.9999'
      *
      ****************************************************************
      *
       01  PI-DRAN0228.
           05  PI-DRAN0228-KEY.

             10 PI-DRAN0228-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0228-PROD-PROD-SCHL   PIC S9(4) COMP.

           05  PI-DRAN0228-DATEN.

             10 PI-DRAN0228-ANLZ-GUELTIG-AB   PIC  X(010).
             10 PI-DRAN0228-ANLZ-KENNUNG-INS  PIC  X(8).
             10 PI-DRAN0228-ANLZ-LZ-BIS   PIC S9(4) COMP.
             10 PI-DRAN0228-ANLZ-LZ-VON   PIC S9(4) COMP.
             10 PI-DRAN0228-ANLZ-SATZ   PIC S9(4)V9(4) COMP-3.

      *
       01  PO-DRAN0228.
           05  PO-DRAN0228-RC                     PIC  X(003).
               88 PO-DRAN0228-RC-OK               VALUE 'OK '.
               88 PO-DRAN0228-RC-DUPLICATE-VALUE  VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
