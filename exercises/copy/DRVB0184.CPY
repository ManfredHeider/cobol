      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0184.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0184.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Dec 1997 11:28:22   T13AE35
      *AVB00393 - Neuerstellung, PI/PO fuer ZWVB0006
      *
      *   Rev 1.0   23 Jul 1997 12:12:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0184                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0184                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HERING                                          *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00393!12.11.97! 01 ! NEU                       ! J.HERING *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0184
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0184.
           05  PI-DRVB0184-DIR-KEY.
               10 PI-DRVB0184-DIR-NR         PIC S9(004) BINARY.
      *
       01  PO-DRVB0184.
           05  PO-DRVB0184-RC                 PIC  X(003).
               88 PO-DRVB0184-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0184-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
