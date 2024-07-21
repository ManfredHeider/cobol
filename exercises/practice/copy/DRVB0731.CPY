      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0731.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0731.CPv  $
      *
      *   Rev 2.1   Oct 11 2004 12:05:38   t13ae27
      *ASL00183: MAP - Manageraufbauplan Ermittlung Ansprüche für D
      *-Leiter (Fiktiv)
      *
      *   Rev 2.0   Aug 24 2004 10:05:32   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0731                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. nächstes Abrechnungsdatum >>        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    24.08.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00183!24.08.04! 01 ! NEU                       ! Duerr    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0731
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01 PI-DRVB0731.
          05 PI-DRVB0731-DATEN.
             10 PI-DRVB0731-KNR-STEUER-INFO         PIC S9(004) BINARY.
             10 PI-DRVB0731-STICHTAG                PIC  X(010).
      *
       01 PO-DRVB0731.
          05 PO-DRVB0731-RC                         PIC  X(003).
             88 PO-DRVB0731-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0731-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB0731-DATEN.
             10 PO-DRVB0731-NAE-MA-ABR-DAT          PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
