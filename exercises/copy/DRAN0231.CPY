      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0231.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0231.CPv  $
      *
      *   Rev 2.4   Jul 30 2020 08:31:20   t13ae34
      *AAN01472: Ablösung Lotus-Notes - Produktmaßnahmendatenbank P
      *MDB durch Mendix
      *
      *   Rev 2.3   Jun 26 2009 10:46:16   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.2   Jun 15 2009 10:00:36   i13az24
      *AAN00860: Automatisierung Host-Konfiguration aufgrund DB PM
      *
      *   Rev 2.1   Jun 05 2009 14:28:34   i13az24
      *
      *   Rev 2.0   May 25 2009 09:49:56   i13az24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0231                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0231                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.07.2020 / 14:16:04 / T13AE34
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
      *    FÜR DIE FUNKTION DRAN0231
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0231.
           05  PI-DRAN0231-KEY.
             10 PI-DRAN0231-ANLZ-LFD-NR   PIC S9(4) COMP.
             10 PI-DRAN0231-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0231-PROD-PROD-SCHL   PIC S9(4) COMP.
      *
       01  PO-DRAN0231.
           05  PO-DRAN0231-RC                     PIC  X(003).
               88 PO-DRAN0231-RC-OK               VALUE 'OK '.
               88 PO-DRAN0231-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

