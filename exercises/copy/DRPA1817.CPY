      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1817.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1817.CPv  $
      *
      *   Rev 2.2   Dec 22 2015 10:35:14   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.1   Sep 15 2015 10:06:42   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für LUCY PGV's
      *
      *   Rev 2.0   Jun 02 2015 06:43:04   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1817 FUNKTION UMSATZSTEUER BERECHNEN    *
      *                                    UND VERTEILEN             *
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
      * APA01932!16.12.15!2.2 ! DURCHREICHEN STEUSTAT     ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1817                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1817.
          05 PI-DRPA1817-DATEN.
             10 PI-DRPA1817-PG-UST-BETR        PIC S9(009)V99
                                                       PACKED-DECIMAL.
             10 PI-DRPA1817-PG-UST-SATZ        PIC S9(003)V999
                                                       PACKED-DECIMAL.
             10 PI-DRPA1817-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1817-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA1817-STICHTAG           PIC  X(010).
             10 PI-DRPA1817-S-VB-ST-STATUS-ERM PIC  X(001).
161215    05 PI-DRPA1817-STEUSTAT.
161215    COPY DRPA1843.
      *
       01 PO-DRPA1817.
          05 PO-DRPA1817-RC                   PIC  X(003).
             88 PO-DRPA1817-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1817-RC-VSTAT-GEFUELLT     VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
 

