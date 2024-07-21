      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0118.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0118.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:56:50   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0118                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0118                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    25.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!25.10.96!    ! NEU                       ! AUERBACH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0118: LESEN ST-NR
      *                               ERM. PRüFZIFFER UND ST-TYP
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0118.
           05  PI-DRVB0118-DATEN.
               10 PI-DRVB0118-ST-NR            PIC S9(009) COMP.
      *
       01  PO-DRVB0118.
           05  PO-DRVB0118-RC                  PIC  X(003).
               88 PO-DRVB0118-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0118-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0118-DATEN.
               10 PO-DRVB0118-ST-PRUEFZIFFER   PIC  X(001).
               10 PO-DRVB0118-ST-TYP           PIC S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
