      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0200.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0200.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   16 Dec 1997 09:49:04   T13AE35
      *AVB00400 - Neuerstellung, Fkt zu ZWVB0006
      *
      *   Rev 1.0   23 Jul 1997 12:14:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0200                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0200                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HERING                                          *
      *    DATUM:    08.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00400!08.12.97! 01 ! NEU                       ! J.HERING *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0200
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0200.
           05  PI-DRVB0200-DIR-KEY.
               10 PI-DRVB0200-DIR-NR         PIC S9(004) BINARY.
      *
       01  PO-DRVB0200.
           05  PO-DRVB0200-RC                 PIC  X(003).
               88 PO-DRVB0200-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0200-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
