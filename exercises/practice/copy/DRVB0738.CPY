      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0738.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0738.CPv  $
      *
      *   Rev 2.1   Oct 11 2004 12:06:06   t13ae27
      *ASL00183: MAP - Manageraufbauplan Ermittlung Ansprüche für D
      *-Leiter (Fiktiv)
      *
      *   Rev 2.0   Sep 06 2004 14:46:54   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0738                                      *
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
      * ASL00183!06.09.04!    ! NEU                       ! Dürr     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0738
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0738.
           05  PI-DRVB0738-BES-KEY.
               10 PI-DRVB0738-BES-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB0738-BES-KTY-NR      PIC  S9(004) COMP.
               10 PI-DRVB0738-BES-KNR-NR      PIC  S9(004) COMP.
      *
       01  PO-DRVB0738.
           05  PO-DRVB0738-RC                 PIC  X(003).
               88 PO-DRVB0738-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0738-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0738-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0738-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB0738-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB0738-K-BESCHRAENKUNG             VALUE 'N'.
           05  PO-DRVB0738-BES-BEZUGS-NR      PIC  S9(009) COMP.
      ******* ENDE COPY-MEMBER **************************************

