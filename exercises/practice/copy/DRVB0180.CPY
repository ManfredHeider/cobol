      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0180.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0180.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Apr 1998 08:52:18   T13AE35
      *AVB00399 - Neuerstellung, PI/PO fuer ZWVB0001
      *
      *   Rev 1.0   23 Jul 1997 12:11:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0180                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0180                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    23.09.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!23.09.97!    ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0180: LESEN ST-NR
      *                               ERM. PRüFZIFFER UND ST-TYP
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0180.
           05  PI-DRVB0180-DATEN.
               10 PI-DRVB0180-ST-NR            PIC S9(009) COMP.
      *
       01  PO-DRVB0180.
           05  PO-DRVB0180-RC                  PIC  X(003).
               88 PO-DRVB0180-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0180-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0180-DATEN.
               10 PO-DRVB0180-KTY-VERTRIEB-GES  PIC S9(004) BINARY.
               10 PO-DRVB0180-KNR-VERTRIEB-GES  PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
