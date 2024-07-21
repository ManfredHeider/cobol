      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.10.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1151.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1151.CPv  $
      *
      *   Rev 2.1   Nov 02 2016 09:51:52   t13ex30
      *ASL00559: Betreuung Versorgungsleistungen 2016 - PMSL0068 un
      *d PMSL0109 (HWO)
      *
      *   Rev 2.0   Oct 11 2016 14:58:52   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1151                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0113                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    11.10.2016                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
111016* ASL00559!11.10.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1151 - ERMITTELN MAX. TIMESTAMP (VSWP) LEIST  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1151.
           05  PI-DRVB1151-DATEN.
               10 PI-DRVB1151-VSWP-LEISTUNG    PIC  X(001).
               10 PI-DRVB1151-VSWP-ART         PIC  X(001).
               10 PI-DRVB1151-KNR-GESELL-TYP   PIC S9(004)      BINARY.
      *
       01  PO-DRVB1151.
           05  PO-DRVB1151-RC                  PIC  X(003).
               88 PO-DRVB1151-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1151-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB1151-DATEN.
               10 PO-DRVB1151-VSWP-LAST-UPDATE PIC X(026).
      ****
      **** ENDE COPY DRVB1151 ****************************************
      ****

