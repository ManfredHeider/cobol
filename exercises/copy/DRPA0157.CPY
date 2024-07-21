      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0157.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0157.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:49:16   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0157                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0157                    *
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
      *    FÜR DIE FUNKTION DRPA0157: LESEN PEWA_ID MIT VNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0157.
           05  PI-DRPA0157-DATEN.
               10 PI-DRPA0157-GESELL           PIC S9(004) COMP.
               10 PI-DRPA0157-VNR              PIC  X(015).
      *
       01  PO-DRPA0157.
           05  PO-DRPA0157-RC                  PIC  X(003).
               88 PO-DRPA0157-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0157-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0157-DATEN.
               10 PO-DRPA0157-PEWA-ID          PIC S9(015) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
