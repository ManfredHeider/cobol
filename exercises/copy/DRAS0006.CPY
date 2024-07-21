      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAS0006.CPv  $
      *
      *   Rev 2.1   Dec 21 1999 09:15:00   T13AE27
      *neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:55:50   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0006                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0006                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    28.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !28.07.99!    ! NEU                       ! Dürr     *
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
       01  PI-DRAS0006.
           05  PI-DRAS0006-VB-NR              PIC  S9(009)    COMP.
           05  PI-DRAS0006-DATUM-VON          PIC  X(10).
           05  PI-DRAS0006-DATUM-BIS          PIC  X(10).
      *
       01  PO-DRAS0006.
           05  PO-DRAS0006-RC                 PIC  X(003).
               88 PO-DRAS0006-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0006-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0006-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0006-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRAS0006-STUFE              PIC  S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************


