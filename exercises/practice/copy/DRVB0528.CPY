      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0528.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0528.CPv  $
      *
      *   Rev 2.1   08 Mar 2001 11:42:04   T13AE27
      *neu
      *
      *   Rev 2.0   22 Feb 2001 16:59:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0528                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0528                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Dürr                                          *
      *    DATUM:    08.03.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!08.03.01!    ! NEU                       ! P. Dürr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0528
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0528.
           05  PI-DRVB0528-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0528-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0528-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0528-KAND-DRUCK-KZ      PIC  S9(004)    COMP.
      *
       01  PO-DRVB0528.
           05  PO-DRVB0528-RC                 PIC  X(003).
               88 PO-DRVB0528-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0528-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0528-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0528-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0528-LAST-UPDATE        PIC X(010).
      *
      ******* ENDE COPY-MEMBER **************************************



