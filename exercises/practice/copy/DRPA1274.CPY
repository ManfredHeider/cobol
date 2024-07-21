      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1274.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1274.CPv  $
      *
      *   Rev 2.1   Jun 30 1999 12:44:28   T13AE18
      *apa00012
      *
      *
      *   Rev 2.0   Mar 30 1999 08:44:20   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1274                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1274                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B. Bohlinger                                     *
      *    DATUM:    30.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * apa00012!30.06.99!01.01! NEU                      ! Bohlinger*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1274: LESEN PVER MIT VNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1274.
           05  PI-DRPA1274-DATEN.
               10 PI-DRPA1274-PGEB-GES      PIC S9(004) BINARY.
               10 PI-DRPA1274-VNR           PIC  X(015) .
      *
       01  PO-DRPA1274.
           05  PO-DRPA1274-RC               PIC  X(003).
               88 PO-DRPA1274-RC-OK              VALUE 'OK '.
               88 PO-DRPA1274-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA1274-DATEN.
               10 PO-DRPA1274-PRINTE        PIC S9(15) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
