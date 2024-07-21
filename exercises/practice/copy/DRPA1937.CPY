      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1937.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1937.CPv  $
      *
      *   Rev 2.1   Nov 04 2019 14:16:10   t13ex26
      *APA02879: Baufinanzierung 2.0 - Anbindung einer Finanzierung
      *splattform (Provision)
      *
      *   Rev 2.0   Oct 28 2019 13:50:42   t13ex26
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * **************************************************************
      *    COPY-MEMBER: DRPA1937 FUNKTION UWPA0015                   *
      *                 DVAGHYP     - PM FORMAL PRUEFEN/NORMIEREN    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KRANZ                                           *
      *    DATUM:    28.10.2019                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02879!28.10.19!2.1 ! NEU                       ! KRANZ    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1937                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1937.
             10 PI-DRPA1937-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1937-PEG-DAT-DVAGHYP.
                COPY DRPA1938.
             10 PI-DRPA1937-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1937.
          05 PO-DRPA1937-RC                 PIC  X(003).
             88 PO-DRPA1937-RC-OK                VALUE 'OK '.
             88 PO-DRPA1937-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
