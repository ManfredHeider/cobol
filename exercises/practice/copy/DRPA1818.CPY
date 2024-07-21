      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1818.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1818.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:13:34   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.0   Jun 02 2015 06:43:04   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1818 FUNKTION ERMITTELN KONDITION FÜR   *
      *                                    UMSATZSTEUER              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    02.06.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!02.06.15!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1818                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1818.
          05 PI-DRPA1818-DATEN.
             10 PI-DRPA1818-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1818-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA1818-STICHTAG           PIC  X(010).
      *
       01 PO-DRPA1818.
          05 PO-DRPA1818-RC                   PIC  X(003).
             88 PO-DRPA1818-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1818-RC-NOT-FOUND          VALUE 'VSG'.
          05 PO-DRPA1818-DATEN.
             10 PO-DRPA1818-S-UST-PFLICHTIG   PIC  X(001).
             10 PO-DRPA1818-PROD-UST-SATZ     PIC S9(003)V999
                                                      PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************

