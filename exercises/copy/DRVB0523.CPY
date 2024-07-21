      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0523.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0523.CPv  $
      *
      *   Rev 2.2   08 Mar 2001 11:41:48   T13AE27
      *neu
      *
      *   Rev 2.1   08 Mar 2001 10:02:34   T13AE27
      *neu
      *
      *   Rev 2.0   22 Feb 2001 16:59:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0523                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0523                    *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0523
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0523.
           05  PI-DRVB0523-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0523-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0523-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0523-KNR-WERTUNGSSTUFE  PIC  S9(004)    COMP.
      *
       01  PO-DRVB0523.
           05  PO-DRVB0523-RC                 PIC  X(003).
               88 PO-DRVB0523-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0523-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0523-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0523-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0523-KAND-TUPEL-GESAMT   PIC X(070).
      *
      ******* ENDE COPY-MEMBER **************************************


