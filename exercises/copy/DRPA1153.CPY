      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1153.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1153.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:30:18   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 27 1999 12:55:50   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   21 Oct 1998 14:01:20   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1153                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1153                    *
      *                  LESEN PRINTE-VERTRAG ZU PRINTE-VORVERTRAG    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    14.01.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!14.01.99!01.01! NEU                      ! BOHLINGER*
      * APA02908!17.02.20!2.1  ! GES-UEBERGR EVS          ! SCHEIDEL *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1153: LESEN aktuellste PVER MIT PIN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1153.
           05  PI-DRPA1153-DATEN.
170220         10 PI-DRPA1153-PGEB-GES-VORVER
                                             PIC S9(004) BINARY.
               10 PI-DRPA1153-PRINTE         PIC S9(015) PACKED-DECIMAL.
      *
       01  PO-DRPA1153.
           05  PO-DRPA1153-RC                PIC  X(003).
               88 PO-DRPA1153-RC-OK               VALUE 'OK '.
               88 PO-DRPA1153-RC-NOT-FOUND        VALUE 'NFD'.
           05  PO-DRPA1153-DATEN.
170220         10 PO-DRPA1153-PGEB-GES       PIC S9(004) BINARY.
               10 PO-DRPA1153-PRINTE-VERTRAG PIC S9(015) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

