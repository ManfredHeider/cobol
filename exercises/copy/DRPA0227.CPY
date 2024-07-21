      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0227.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0227.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:54:36   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: drpa0227                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION drpa0227                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: giese                                            *
      *    DATUM:    24.02.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.10.96!    ! NEU                       ! BORK     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION drpa0227: LESEN TBPA0008  pgeb
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-drpa0227.
           05  PI-drpa0227-DATEN.
               10 PI-drpa0227-PGEB-GES         PIC S9(004) COMP.
      *
       01  PO-drpa0227.
           05  PO-drpa0227-RC                  PIC  X(003).
               88 PO-drpa0227-RC-OK                        VALUE 'OK '.
               88 PO-drpa0227-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-drpa0227-DATEN.
               10 PO-drpa0227-KNR-vertrieb-ges PIC S9(004) COMP.
      *
