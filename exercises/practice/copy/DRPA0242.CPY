      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0242.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0242.CPv  $
      *
      *   Rev 2.0   May 26 1999 12:59:54   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:13:56   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    Copy-Member: DRPA0242  Funktionen Standard-OZP            *
      *                           PGVG TBPA0020                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Boglinger                                       *
      *    DATUM:    27.03.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.03.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !16.07.97!    ! Anpassungen aufgrund von  ! Storch   *
      *         !        !    ! Tabellenänderungen        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0020.
           05 PI-ZTPA0020-FUNKTION            PIC X(008).
              88 PI-ZTPA0020-DRPA0243-INS         VALUE 'DRPA0243'.
              88 PI-ZTPA0020-DRPA0244-UPD         VALUE 'DRPA0244'.
              88 PI-ZTPA0020-DRPA0245-DEL         VALUE 'DRPA0245'.
              88 PI-ZTPA0020-DRPA0246-SEL-ALL     VALUE 'DRPA0246'.
              88 PI-ZTPA0020-DRPA0247-SEL-AKT     VALUE 'DRPA0247'.
160797*       88 PI-ZTPA0020-DRPA0248-UPD-DAT     VALUE 'DRPA0248'.
