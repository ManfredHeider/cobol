      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0983.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0983.CPv  $
      *
      *   Rev 2.1   Feb 16 2011 11:25:46   t13ae13
      *AVB01036: Einheitenzählung bei Betreuerwechsel 
      *
      *   Rev 2.0   Jul 30 2010 12:53:44   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0983                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. ABR-DAT ZU DAT >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    01.12.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01036!01.12.10!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0983
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01 PI-DRVB0983.
          05 PI-DRVB0983-DATEN.
             10 PI-DRVB0983-KNR-STEUER-INFO         PIC S9(004) BINARY.
             10 PI-DRVB0983-STICHTAG-ISO            PIC  X(010).
      *
       01 PO-DRVB0983.
          05 PO-DRVB0983-RC                         PIC  X(003).
             88 PO-DRVB0983-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0983-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB0983-DATEN.
             10 PO-DRVB0983-ZEITPUNKT-STAT-ISO      PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
