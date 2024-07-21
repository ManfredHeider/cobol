      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0694.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0694.CPv  $
      *
      *   Rev 2.1   Dec 18 2003 15:33:32   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.0   Feb 17 2003 11:01:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0694                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0694                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Dürr                                          *
      *    DATUM:    17.12.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00443!17.12.03!    ! NEU                       ! P. Dürr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0694
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0694.
           05  PI-DRVB0694-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0694-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0694-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
      *
       01  PO-DRVB0694.
           05  PO-DRVB0694-RC                 PIC  X(003).
               88 PO-DRVB0694-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0694-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0694-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0694-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0694-ERG-TUPEL-GESAMT   PIC X(200).
      *
      ******* ENDE COPY-MEMBER **************************************


