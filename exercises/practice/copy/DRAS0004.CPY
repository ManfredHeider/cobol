      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0004.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAS0004.CPv  $
      *
      *   Rev 2.3   Apr 08 2004 12:06:00   t13ae27
      *Siegeradler-Ranglisten / Trennung DVAG Wien
      *
      *   Rev 2.2   Feb 04 2003 10:49:26   t13ae27
      *Anpassungen für 2002 wegen Siegeradlerauswertung
      *
      *   Rev 2.1   Dec 21 1999 09:14:02   T13AE27
      *Neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:55:38   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0004                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0004                    *
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
       01  PI-DRAS0004.
           05  PI-DRAS0004-VB-NR              PIC  S9(009)    COMP.
220102*    05  PI-DRAS0004-E-VOR-40           PIC  S9(009)V9  COMP-3.
220102     05  PI-DRAS0004-E-V-ABZPROZ-REGEL  PIC  S9(009)V9  COMP-3.
           05  PI-DRAS0004-BASIS-WERT         PIC  S9(009)V9  COMP-3.
           05  PI-DRAS0004-DATUM-VON          PIC  X(10).
           05  PI-DRAS0004-DATUM-BIS          PIC  X(10).
220102     05  PI-DRAS0004-MS-ABZPROZ-REGEL   PIC  S9(003)V99 COMP-3.
090104     05  PI-DRAS0004-BETR-GESELL        PIC  S9(004)    COMP.
090104     05  PI-DRAS0004-VK-GESELL          PIC  S9(004)    COMP.
      *
       01  PO-DRAS0004.
           05  PO-DRAS0004-RC                 PIC  X(003).
               88 PO-DRAS0004-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0004-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0004-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0004-RC-ERR                       VALUE 'ERR'.
      *
220102*    05  PO-DRAS0004-E-NACH-40          PIC  S9(011)V99 COMP-3.
220102     05  PO-DRAS0004-E-N-ABZPROZ-REGEL  PIC  S9(011)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

