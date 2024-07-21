      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.07.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0371.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0371.CPv  $
      *
      *   Rev 2.1   Jul 30 2020 08:32:00   t13ae34
      *AAN01472: Ablösung Lotus-Notes - Produktmaßnahmendatenbank P
      *MDB durch Mendix
      *
      *   Rev 2.0   Jul 16 2020 13:03:26   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0371                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0371                    *
      *    --------------------------------------------------------   *
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01472!16.07.20!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0371
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE - AKTUELLE EINTRÄGE BEENDEN <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0371.
           05  PI-DRAN0371-KEY.

             10 PI-DRAN0371-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0371-PROD-PROD-SCHL   PIC S9(4) COMP.


           05  PI-DRAN0371-DATEN.

             10 PI-DRAN0371-ANLZ-GUELTIG-BIS  PIC  X(010).
             10 PI-DRAN0371-ANLZ-KENNUNG-UPD   PIC  X(0008).
      *
       01  PO-DRAN0371.
           05  PO-DRAN0371-RC                     PIC  X(003).
               88 PO-DRAN0371-RC-OK               VALUE 'OK '.
               88 PO-DRAN0371-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
 

