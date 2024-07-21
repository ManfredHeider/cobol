      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0743.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0743.CPv  $
      *
      *   Rev 2.1   Dec 03 2004 15:33:32   t13ae27
      *ASL00196: Organisations-Leistungs-Bonus 2004
      *
      *   Rev 2.0   Nov 26 2004 11:23:14   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0743                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0743                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00196!25.11.04!    ! NEU                       !P. Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0743 für VIVBB082
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0743.
           05  PI-DRVB0743-MA-ST-NR           PIC S9(007) COMP.
           05  PI-DRVB0743-LIST-MONAT         PIC  X(010)     .
           05  PI-DRVB0743-KNR-WB-TYP         PIC S9(004) COMP.
           05  PI-DRVB0743-KNR-WERTUNGS-TYP   PIC S9(004) COMP.
      *
       01  PO-DRVB0743.
           05  PO-DRVB0743-RC                 PIC  X(003).
               88 PO-DRVB0743-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0743-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0743-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0743-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0743-LIST-TUPEL-GESAMT  PIC X(288).
      *
      *********************** ENDE COPY-MEMBER *************************
