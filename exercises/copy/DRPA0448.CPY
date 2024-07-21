      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0448.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0448.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:51:50   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:38:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0448                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<JAHRESBEITR. UMRECH.>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    14.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.08.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!20.10.98!    ! CRPA0027                  ! WOLFF    *
      *         !        !    !   - NEU: PGEB-GES,        !          *
      *         !        !    !          STICHTAG         !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0448                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0448.
          05 PI-DRPA0448-DATEN.
             10 PI-DRPA0448-JAHRES-BEITRAG          PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0448-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0448-KNR-ZAHLWEISE           PIC S9(004) BINARY.
             10 PI-DRPA0448-STICHTAG                PIC  X(010).
      *
       01 PO-DRPA0448.
          05 PO-DRPA0448-RC                         PIC  X(003).
             88 PO-DRPA0448-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0448-DATEN.
             10 PO-DRPA0448-BEITRAG                 PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
