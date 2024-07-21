      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1361.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1361.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:32:38   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Apr 13 2000 15:51:32   T13EX21
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1361  FUNKTION VORVERTRAGSNUMMER PRÜFEN  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    29.03.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!29.03.00!2.1  ! NEUERSTELLUNG             ! STORCH  *
      * APA02908!17.02.20!2.2  ! GES-UEBERGR EVS           ! SCHEIDEL*
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1361.
170220    05 PI-DRPA1361-PGEB-GES                PIC S9(004) BINARY.
          05 PI-DRPA1361-EXT-VNR                 PIC  X(015).
          05 PI-DRPA1361-PGV                     PIC S9(004) COMP.
          05 PI-DRPA1361-VVNR-TAB                OCCURS 10
                                      INDEXED BY PI-DRPA1361-VVNR-IND.
170220       10 PI-DRPA1361-VVGES                PIC  9(002).
             10 PI-DRPA1361-VVNR                 PIC  X(015).
      *
       01 PO-DRPA1361.
          05 PO-DRPA1361-RC                     PIC  X(003).
             88 PO-DRPA1361-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1361-RC-VSTAT-GEFUELLT            VALUE 'VSG'.
      *
      ****** ENDE COPY-MEMBER ****************************************

