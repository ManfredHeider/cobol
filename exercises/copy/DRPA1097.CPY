      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1097.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1097.CPv  $
      *
      *   Rev 2.0   Mar 18 1999 08:07:30   T13EX44
      *Umstellung MFE
      *
      *   Rev 1.0   20 Jul 1998 10:21:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1097  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    09.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.10.98! 01 ! NEUERSTELLUNG             ! SCHEIDEL *
      * AFPA0012!30.11.98! 02 ! ANPASSUNG AN NEUE DB-STRUK! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FUER DIE FUNKTION DRPA1097                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN VERTRAGSHISTORIE             *       *
      *                                                              *
      ****************************************************************
          01 PI-DRPA1097.
           10 PI-DRPA1097-UFKT              PIC  X(001).
            88 PI-DRPA1097-UFKT-READ        VALUE '1'.
            88 PI-DRPA1097-UFKT-READ-LAST   VALUE '2'.
           05 PI-DRPA1097-DAT30-KEY.
            10 PI-DRPA1097-DAT30-GES        PIC  9(002).
            10 PI-DRPA1097-DAT30-VSNR-ALT   PIC  9(015).
            10 PI-DRPA1097-DAT30-VONDATUM   PIC  9(006).
      *
          01 PO-DRPA1097.
           05 PO-DRPA1097-RC                PIC  X(003).
            88 PO-DRPA1097-RC-OK            VALUE 'OK '.
            88 PO-DRPA1097-RC-NOT-FOUND     VALUE 'NFD'.
           05 PO-DRPA1097-DAT30-DATEN.
            10 PO-DRPA1097-DAT30-GES        PIC  9(002).
            10 PO-DRPA1097-DAT30-VSNR-ALT   PIC  9(015).
301198      10 po-drpa1097-dat30-jh         pic  9(002).
            10 PO-DRPA1097-DAT30-VONDATUM   PIC  9(006).
            10 PO-DRPA1097-DAT30-ALTER-GES  PIC  9(002).
            10 PO-DRPA1097-DAT30-VSNR-NEU   PIC  9(015).
            10 PO-DRPA1097-DAT30-FOLGENR    PIC  9(002).
            10 PO-DRPA1097-DAT30-BISDATUM   PIC  9(006).
