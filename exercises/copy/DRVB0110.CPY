      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0110.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0110.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:56:12   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0110                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0110                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Giese                                            *
      *    DATUM:    12.09.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !12.09.96!    ! NEU                       ! Giese    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0110
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0110.
           05  PI-DRVB0110-DZ-KEY.
               10 PI-DRVB0110-DZ-ST-NR        PIC S9(007) BINARY.
      *
       01  PO-DRVB0110.
           05  PO-DRVB0110-RC                 PIC  X(003).
               88 PO-DRVB0110-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0110-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0110-DZ-DIR-NR          PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
