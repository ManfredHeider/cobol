      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0245.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0245.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:00:00   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:14:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0246  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.03.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!27.03.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !04.07.97!    ! ANPASSUNGEN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENäNDERUNGEN        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0246                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       PGVG TBPA0020                      *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0245.
          05 PI-DRPA0245-LAST-UPDATE           PIC  X(026).
          05 PI-DRPA0245-PGVG-KEY.
             10 PI-DRPA0245-PGEB-GES           PIC S9(004)  BINARY.
040797*      10 PI-DRPA0245-PGVG-GUELT-AB      PIC   X(010).
             10 PI-DRPA0245-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
      *
       01 PO-DRPA0245.
          05 PO-DRPA0245-RC                    PIC  X(003).
             88 PO-DRPA0245-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0245-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0245-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
