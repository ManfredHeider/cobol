      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0697.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0697.CPv  $
      *
      *   Rev 2.1   Mar 11 2004 11:42:02   t13ae27
      *Scharfschaltung
      *
      *   Rev 2.0   Feb 17 2003 11:01:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0697                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0697                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Dürr                                          *
      *    DATUM:    23.02.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!23.02.01!    ! NEU                       ! P. Dürr  *
      * AAS00443!20.01.04!2.2 ! neue Funktion DRVB0697    ! P. Dürr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0697
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0697.
           05  PI-DRVB0697-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0697-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0697-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0697-WERTUNGSSTUFE      PIC  S9(004)    COMP.
      *
       01  PO-DRVB0697.
           05  PO-DRVB0697-RC                 PIC  X(003).
               88 PO-DRVB0697-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0697-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0697-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0697-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0697-KAND-TUPEL-GESAMT   PIC X(085).
      *
      ******* ENDE COPY-MEMBER **************************************


