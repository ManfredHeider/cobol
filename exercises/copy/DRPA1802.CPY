      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1802.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1802.CPv  $
      *
      *   Rev 2.1   Jun 16 2015 08:54:52   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PEG (B
      *atch)
      *
      *
      *
      *   Rev 2.0   Mar 17 2015 09:27:02   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1802 FUNKTION UWPA0015                   *
      *                 DVM - PM FORMAL PRUEFEN/NORMIEREN            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    17.03.2015                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!17.03.15!2.1 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1802                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1802.
             10 PI-DRPA1802-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1802-PEG-DATEN-DVM.
                COPY DRPA1803.
             10 PI-DRPA1802-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1802.
          05 PO-DRPA1802-RC                 PIC  X(003).
             88 PO-DRPA1802-RC-OK                VALUE 'OK '.
             88 PO-DRPA1802-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************

