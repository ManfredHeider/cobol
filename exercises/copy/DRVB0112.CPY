      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0112.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0112.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:56:30   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0112                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0112                    *
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
       01  PI-DRVB0112.
           05  PI-DRVB0112-MAN-KEY.
               10 PI-DRVB0112-MAN-BEZUGS-NR   PIC S9(007) BINARY.
      *
       01  PO-DRVB0112.
           05  PO-DRVB0112-RC                 PIC  X(003).
               88 PO-DRVB0112-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0112-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0112-MAN-VORNAME        PIC  X(020).
           05  PO-DRVB0112-MAN-NAME           PIC  X(040).
      *
      ******* ENDE COPY-MEMBER **************************************
