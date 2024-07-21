      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0612.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0612.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:33:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:54:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0612                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0612                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * Daisy   !11.11.97!01.01! NEU                      ! Jäger    *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0612: LESEN aktuellste PVER MIT VNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0612.
           05  PI-DRPA0612-DATEN.
               10 PI-DRPA0612-PGEB-GES      PIC S9(004) BINARY.
               10 PI-DRPA0612-VNR           PIC  X(015) .
      *
       01  PO-DRPA0612.
           05  PO-DRPA0612-RC               PIC  X(003).
               88 PO-DRPA0612-RC-OK              VALUE 'OK '.
               88 PO-DRPA0612-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA0612-DATEN.
               10 PO-DRPA0612-PRINTE        PIC S9(15) PACKED-DECIMAL.
               10 PO-DRPA0612-VERSION       PIC S9(04) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
