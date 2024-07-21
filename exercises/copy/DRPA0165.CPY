      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0165.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0165.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:49:54   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0165                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0165                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    01.11.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!01.11.96!    ! NEU                       ! AUERBACH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0165: ERMITTELN DES MAXIMALEN
      *                               PEWP_ABLAUF FüR EINE PEWA_ID
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0165.
           05  PI-DRPA0165-DATEN.
               10 PI-DRPA0165-PEWA-ID          PIC S9(015)     COMP-3.
      *
       01  PO-DRPA0165.
           05  PO-DRPA0165-RC                  PIC  X(003).
               88 PO-DRPA0165-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0165-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0165-DATEN.
               10 PO-DRPA0165-PEWP-ABLAUF      PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
