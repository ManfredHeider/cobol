      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0163.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0163.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:49:34   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0163                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0163                    *
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
      *    FÜR DIE FUNKTION DRPA0163
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0163.
           05  PI-DRPA0163-KEY.
               10 PI-DRPA0163-PIN-13          PIC  9(13).
      *
       01  PO-DRPA0163.
           05  PO-DRPA0163-RC                 PIC  X(03).
               88 PO-DRPA0163-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0163-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRPA0163-PIN-PRF            PIC  X(02).
      *
      ******* ENDE COPY-MEMBER **************************************
