      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0155.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0155.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:48:58   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0155                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0155                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BORK                                             *
      *    DATUM:    24.10.1996                                       *
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
      *    FÜR DIE FUNKTION DRPA0155: LESEN TBPA0015  PVST
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0155.
           05  PI-DRPA0155-DATEN.
               10 PI-DRPA0155-PGEB-GES         PIC S9(004) COMP.
      *
       01  PO-DRPA0155.
           05  PO-DRPA0155-RC                  PIC  X(003).
               88 PO-DRPA0155-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0155-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0155-DATEN.
               10 PO-DRPA0155-KNR-WAEHRUNG     PIC S9(004) COMP.
      *
