      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0547.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0547.CPv  $
      *
      *   Rev 2.1   04 Jul 2001 16:25:26   t13ae13
      *ASL00102: neue Beschränkungen
      *
      *   Rev 2.0   15 May 2001 13:33:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0547                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0442                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    20.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.07.99!    ! NEU                       ! Dürr     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0547
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0547.
           05  PI-DRVB0547-BES-KEY.
               10 PI-DRVB0547-BES-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB0547-BES-KTY-NR      PIC  S9(004) COMP.
               10 PI-DRVB0547-BES-KNR-NR-1    PIC  S9(004) COMP.
               10 PI-DRVB0547-BES-KNR-NR-2    PIC  S9(004) COMP.
      *
       01  PO-DRVB0547.
           05  PO-DRVB0547-RC                 PIC  X(003).
               88 PO-DRVB0547-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0547-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0547-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0547-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB0547-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB0547-K-BESCHRAENKUNG             VALUE 'N'.
      ******* ENDE COPY-MEMBER **************************************

