      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0747.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0747.CPv  $
      *
      *   Rev 2.1   Feb 11 2005 11:40:08   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Dec 06 2004 12:29:58   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0747                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0747                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    12.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!12.08.99!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0747
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0747.
           05  PI-DRVB0747-PARTEH-BETR        PIC  S9(009)    COMP.
           05  PI-DRVB0747-PARTEH-UNTST       PIC  S9(009)    COMP.
           05  PI-DRVB0747-PARTEH-MONAT       PIC  X(010).
           05  PI-DRVB0747-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0747-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
      *
       01  PO-DRVB0747.
           05  PO-DRVB0747-RC                 PIC  X(003).
               88 PO-DRVB0747-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0747-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0747-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0747-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0747-PARTEH-TUPEL        PIC  X(076).
      *
      ******* ENDE COPY-MEMBER **************************************

