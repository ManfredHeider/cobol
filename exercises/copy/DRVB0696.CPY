      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0696.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0696.CPv  $
      *
      *   Rev 2.2   Mar 22 2004 13:16:10   t13ae27
      *Fehlerkoorektur: LIST_TYP
      *
      *   Rev 2.1   Mar 11 2004 11:38:40   t13ae27
      *Scharfschaltung
      *
      *   Rev 2.0   Feb 17 2003 11:01:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0696                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0696                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00443!20.01.03!    ! NEU                       !P. Duerr  *
      * FAS00128!22.03.04! 2.1! LIST-TYP in PI-Bereich    !P.Duerr   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0696
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0696.
           05  PI-DRVB0696-MA-ST-NR           PIC S9(007) COMP.
           05  PI-DRVB0696-LIST-MONAT         PIC  X(010)     .
           05  PI-DRVB0696-KNR-WB-TYP         PIC S9(004) COMP.
           05  PI-DRVB0696-KNR-WERTUNGS-TYP   PIC S9(004) COMP.
           05  PI-DRVB0696-WERTUNGSSTUFE      PIC S9(004) COMP.
220304     05  PI-DRVB0696-KNR-LIST-TYP       PIC S9(004) COMP.
      *
       01  PO-DRVB0696.
           05  PO-DRVB0696-RC                 PIC  X(003).
               88 PO-DRVB0696-RC-OK                  VALUE 'OK '.
               88 PO-DRVB0696-RC-NOK                 VALUE 'NOK'.
               88 PO-DRVB0696-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRVB0696-RC-ERR                 VALUE 'ERR'.
      *
           05  PO-DRVB0696-LIST-TUPEL-GESAMT  PIC X(288).
      *
      *********************** ENDE COPY-MEMBER *************************
