      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0014.CPv  $
      *
      *   Rev 2.1   Apr 08 2004 12:09:10   t13ae27
      *Siegeradler-Ranglisten / Trennung DVAG Wien
      *
      *   Rev 2.0   06 Mar 2001 11:47:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0014                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0014                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    20.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !28.07.99!    ! NEU                       ! Dürr     *
      * AAS00333!07.01.02! 2.2! 40-%-Regel ändern         ! Dürr     *
      * AAS00460!09.02.04! 2.3! GESELL für BETR, UNTST    ! Dürr     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0112
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0014.
           05  PI-DRAS0014-VB-NR              PIC  S9(009)    COMP.
           05  PI-DRAS0014-E-V-ABZPROZ-REGEL  PIC  S9(009)V9  COMP-3.
           05  PI-DRAS0014-BASIS-WERT         PIC  S9(009)V9  COMP-3.
           05  PI-DRAS0014-DATUM-VON          PIC  X(10).
           05  PI-DRAS0014-DATUM-BIS          PIC  X(10).
           05  PI-DRAS0014-MS-ABZPROZ-REGEL   PIC  S9(003)V99 COMP-3.
           05  PI-DRAS0014-BETR-GESELL        PIC  S9(004)    COMP.
           05  PI-DRAS0014-VK-GESELL          PIC  S9(004)    COMP.
           05  PI-DRAS0014-DIR-NR             PIC  S9(004)    COMP.
      *
       01  PO-DRAS0014.
           05  PO-DRAS0014-RC                 PIC  X(003).
               88 PO-DRAS0014-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0014-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0014-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0014-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRAS0014-E-N-ABZPROZ-REGEL  PIC  S9(011)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

