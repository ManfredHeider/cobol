      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0895.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0895.CPv  $
      *
      *   Rev 2.1   Mar 14 2008 16:33:14   t13ae27
      *AAS00733: EliteClub Unternehmensaufbau
      *
      *   Rev 2.0   06 Mar 2008 16:11:02   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0895                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0895                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00733!06.03.08!    ! NEU                       !P. Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0895 für VIVBB082
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0895.
           05  PI-DRVB0895-MA-ST-NR           PIC S9(007) COMP.
           05  PI-DRVB0895-LIST-MONAT         PIC  X(010)     .
           05  PI-DRVB0895-KNR-WB-TYP         PIC S9(004) COMP.
           05  PI-DRVB0895-KNR-WERTUNGS-TYP   PIC S9(004) COMP.
           05  PI-DRVB0895-KNR-LIST-TYP       PIC S9(004) COMP.
      *
       01  PO-DRVB0895.
           05  PO-DRVB0895-RC                 PIC  X(003).
               88 PO-DRVB0895-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0895-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0895-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0895-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0895-LIST-TUPEL-GESAMT  PIC X(288).
      *
      *********************** ENDE COPY-MEMBER *************************
